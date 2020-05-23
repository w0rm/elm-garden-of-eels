module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Browser
import Browser.Events
import Const
import Coordinates exposing (World)
import Duration
import Eel exposing (Eel)
import Html exposing (Html)
import Json.Decode as Decode
import Length exposing (Meters)
import Plankter exposing (Plankter)
import Point2d exposing (Point2d)
import Quantity
import Random exposing (Seed)
import Speed exposing (Speed)
import Time


type alias Model =
    { mouse : Point2d Meters World
    , current : Speed
    , plankters : List Plankter
    , seed : Seed
    , spawner : Timeline Float
    , eels : List Eel
    }


type Msg
    = MouseMove Float Float
    | MouseDown Float Float
    | Tick Time.Posix


eelsNumber : number
eelsNumber =
    4


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { mouse = Point2d.meters 100 100
                  , eels =
                        List.range 0 (eelsNumber - 1)
                            |> List.map (\i -> toFloat i / eelsNumber)
                            |> List.map
                                (\t ->
                                    Eel.init
                                        (Length.meters 0.6)
                                        (Point2d.interpolateFrom
                                            (Point2d.meters -1 0.4)
                                            (Point2d.meters 1.2 -0.2)
                                            t
                                        )
                                )
                  , plankters = []
                  , current =
                        Quantity.interpolateFrom
                            Const.minCurrent
                            Const.maxCurrent
                            0.5
                  , seed = Random.initialSeed 1
                  , spawner = Animator.init 0
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove x y ->
            ( { model | mouse = Coordinates.screenToWorld (Point2d.pixels x y) }
            , Cmd.none
            )

        MouseDown _ _ ->
            ( { model | eels = List.map (hideEel model.current model.mouse) model.eels }
            , Cmd.none
            )

        Tick time ->
            ( model
                |> spawnPlankters time
                |> animatePlankters time
                |> animateEels time
                |> eatPlankters
            , Cmd.none
            )


spawnPlankters : Time.Posix -> Model -> Model
spawnPlankters time model =
    let
        spawner =
            Animator.update time animator model.spawner

        duration =
            Quantity.at_ model.current (Length.meters 0.3)
                |> Duration.inSeconds
    in
    if Animator.previous spawner == Animator.current spawner then
        let
            ( plankter, seed ) =
                Random.step Plankter.random model.seed
        in
        { model
            | plankters = plankter :: model.plankters
            , seed = seed
            , spawner = Animator.go (Animator.seconds duration) (Animator.current spawner + 1) spawner
        }

    else
        { model | spawner = spawner }


animatePlankters : Time.Posix -> Model -> Model
animatePlankters time model =
    let
        move plankter =
            { plankter
                | position = Plankter.positionIn (Duration.milliseconds 16) model.current plankter
                , timeline = Animator.update time animator plankter.timeline
            }
    in
    { model
        | plankters = List.map move model.plankters
    }


animateEels : Time.Posix -> Model -> Model
animateEels time model =
    let
        animate eel =
            { eel | timeline = Animator.update time animator eel.timeline }
    in
    { model
        | eels = List.map animate model.eels
    }


hideEel : Speed -> Point2d Meters World -> Eel -> Eel
hideEel current mouse eel =
    let
        { head } =
            Eel.properties current eel
    in
    if
        Quantity.lessThan (Length.meters 0.2) (Point2d.distanceFrom head mouse)
            && (Animator.current eel.timeline /= Eel.Hidden)
    then
        { eel
            | timeline =
                Animator.interrupt
                    [ Animator.event (Animator.seconds 1) Eel.Hidden
                    , Animator.wait (Animator.seconds 5)
                    , Animator.event (Animator.seconds 2) Eel.Resting
                    ]
                    eel.timeline
        }

    else
        eel


eatPlankters : Model -> Model
eatPlankters model =
    eatPlanktersHelp model.eels model.plankters [] [] [] model


eatPlanktersHelp : List Eel -> List Plankter -> List Plankter -> List Eel -> List Plankter -> Model -> Model
eatPlanktersHelp currentEels currentPlankters requeuedPlankters resultEels resultPlankters model =
    case currentEels of
        [] ->
            { model
                | eels = resultEels
                , plankters = requeuedPlankters ++ currentPlankters ++ resultPlankters
            }

        eel :: remainingCurrentEels ->
            if
                (Animator.previous eel.timeline /= Eel.Resting)
                    || (Animator.current eel.timeline /= Eel.Resting)
            then
                -- skip the busy eel
                eatPlanktersHelp remainingCurrentEels currentPlankters requeuedPlankters (eel :: resultEels) resultPlankters model

            else
                case currentPlankters of
                    [] ->
                        -- try with the next eel
                        eatPlanktersHelp remainingCurrentEels requeuedPlankters [] (eel :: resultEels) resultPlankters model

                    plankter :: remainingCurrentPlankters ->
                        if Animator.current plankter.timeline == Plankter.Eaten then
                            if
                                (Animator.previous plankter.timeline == Plankter.Eaten)
                                    || Quantity.lessThan (Quantity.negate Coordinates.maxX) (Point2d.xCoordinate plankter.position)
                            then
                                -- completely remove the plankter that has been eaten or moved outside the screen
                                eatPlanktersHelp remainingCurrentEels remainingCurrentPlankters requeuedPlankters (eel :: resultEels) resultPlankters model

                            else
                                -- skip the plankter that is currently being eaten
                                eatPlanktersHelp (eel :: remainingCurrentEels) remainingCurrentPlankters requeuedPlankters resultEels (plankter :: resultPlankters) model

                        else if canEat model.current plankter eel then
                            let
                                newEel =
                                    { eel
                                        | timeline =
                                            eel.timeline
                                                |> Animator.interrupt
                                                    [ Animator.event Animator.quickly (Eel.Striking (Plankter.positionIn Animator.quickly model.current plankter))
                                                    , Animator.wait (Animator.millis 1)
                                                    , Animator.event Animator.quickly Eel.Resting
                                                    ]
                                    }

                                newPlankter =
                                    { plankter | timeline = Animator.go Animator.quickly Plankter.Eaten plankter.timeline }
                            in
                            -- eat the plankter
                            eatPlanktersHelp remainingCurrentEels remainingCurrentPlankters requeuedPlankters (newEel :: resultEels) (newPlankter :: resultPlankters) model

                        else
                            -- requeue the plankter for the next
                            eatPlanktersHelp (eel :: remainingCurrentEels) remainingCurrentPlankters (plankter :: requeuedPlankters) resultEels resultPlankters model


canEat : Speed -> Plankter -> Eel -> Bool
canEat current plankter eel =
    let
        position =
            Plankter.positionIn Animator.quickly current plankter

        { length, head, burrow } =
            Eel.properties current eel
    in
    Quantity.lessThan (Quantity.multiplyBy 0.98 length) (Point2d.distanceFrom burrow position)
        && Quantity.lessThan (Length.meters 0.2) (Point2d.distanceFrom head position)
        && Quantity.lessThan (Point2d.xCoordinate position) (Point2d.xCoordinate head)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onMouseMove
            (Decode.map2 MouseMove
                (Decode.field "pageX" Decode.float)
                (Decode.field "pageY" Decode.float)
            )
        , Browser.Events.onMouseDown
            (Decode.map2 MouseDown
                (Decode.field "pageX" Decode.float)
                (Decode.field "pageY" Decode.float)
            )
        ]


view : Model -> Html a
view { current, eels, plankters } =
    Html.div []
        [ Coordinates.view
            (List.map Plankter.view plankters
                ++ List.map (Eel.view current) eels
            )
        ]


animator : Animator (Timeline any)
animator =
    Animator.watching identity always Animator.animator
