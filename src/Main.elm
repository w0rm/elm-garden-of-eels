module Main exposing (main)

import Animator exposing (Timeline)
import Browser
import Browser.Events
import Const
import Coordinates exposing (World)
import Duration exposing (Duration)
import Eel exposing (Eel)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Length exposing (Meters)
import Plankter exposing (Plankter, PlankterKind(..))
import Point2d exposing (Point2d)
import Quantity
import Random exposing (Seed)
import Speed exposing (Speed)
import Splash exposing (Splash)
import Svg
import Svg.Attributes
import Time
import Vector2d


type alias Model =
    { current : Timeline Speed
    , time : Time.Posix
    , score : Int
    , lives : Int
    , plankters : List Plankter
    , splashes : List Splash
    , seed : Seed
    , spawner : Timeline Float
    , eels : List Eel
    }


type Msg
    = MouseDown Float Float
    | Tick Float


burrows : List (Point2d Meters World)
burrows =
    [ Point2d.meters -1.15 0.13
    , Point2d.meters -0.36 -0.12
    , Point2d.meters 0.23 -0.68
    , Point2d.meters 0.72 -0.43
    ]


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { eels =
                        burrows
                            |> List.map
                                (\burrow ->
                                    Eel.init
                                        (Length.meters 0.6)
                                        burrow
                                )
                  , plankters = []
                  , splashes = []
                  , current = Animator.init Const.midCurrent
                  , seed = Random.initialSeed 1
                  , spawner = Animator.init 0
                  , time = Time.millisToPosix 0
                  , lives = 5
                  , score = 0
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
        MouseDown x y ->
            ( { model
                | splashes = Splash.init (Coordinates.screenToWorld (Point2d.pixels x y)) :: model.splashes
              }
            , Cmd.none
            )

        Tick dt ->
            ( let
                time =
                    Time.millisToPosix (Time.posixToMillis model.time + round dt)

                delta =
                    Duration.milliseconds dt
              in
              { model
                | time = time
                , current = Animator.updateTimeline time model.current
              }
                |> spawnPlankters
                |> animatePlankters delta
                |> animateEels
                |> animateSplashes
                |> eatPlankters
            , Cmd.none
            )


getCurrent : Timeline Speed -> Speed
getCurrent timeline =
    Animator.move timeline
        (\speed -> Animator.at (Speed.inMetersPerSecond speed))
        |> Speed.metersPerSecond


spawnPlankters : Model -> Model
spawnPlankters model =
    let
        spawner =
            Animator.updateTimeline model.time model.spawner

        duration =
            Quantity.at_ (getCurrent model.current) (Length.meters 0.6)
                |> Duration.inSeconds
    in
    if Animator.arrived spawner == Animator.current spawner then
        let
            ( plankter, seed ) =
                Random.step Plankter.random model.seed
        in
        { model
            | plankters = plankter :: model.plankters
            , seed = seed
            , spawner =
                Animator.go
                    (Animator.seconds duration)
                    (Animator.current spawner + 1)
                    spawner
        }

    else
        { model | spawner = spawner }


animateSplashes : Model -> Model
animateSplashes model =
    let
        animate splash =
            if Animator.arrived splash.timeline == Splash.Final then
                Nothing

            else
                Just
                    { splash
                        | timeline = Animator.updateTimeline model.time splash.timeline
                    }
    in
    { model
        | splashes = List.filterMap animate model.splashes
    }


animatePlankters : Duration -> Model -> Model
animatePlankters delta model =
    let
        move plankter =
            let
                splashDistance =
                    Splash.velocityAt plankter.position model.splashes
                        |> Vector2d.for delta
            in
            { plankter
                | position =
                    Plankter.positionIn delta (getCurrent model.current) plankter
                        |> Point2d.translateBy splashDistance
                , timeline = Animator.updateTimeline model.time plankter.timeline
            }
    in
    { model
        | plankters = List.map move model.plankters
    }


animateEels : Model -> Model
animateEels model =
    animateEelsHelp model.eels model.lives [] model


animateEelsHelp : List Eel -> Int -> List Eel -> Model -> Model
animateEelsHelp currentEels lives resultEels model =
    case currentEels of
        [] ->
            { model
                | eels = resultEels
                , lives = lives
            }

        eel :: remainingEels ->
            let
                { head } =
                    Eel.properties (getCurrent model.current) eel

                hit =
                    not (Animator.upcoming Eel.Hidden eel.timeline)
                        && (Animator.current eel.timeline == Eel.Resting)
                        && (Animator.arrived eel.timeline == Eel.Resting)
                        && List.any
                            (\splash ->
                                let
                                    { center, radius } =
                                        Splash.properties splash
                                in
                                Quantity.lessThan radius (Point2d.distanceFrom head center)
                            )
                            model.splashes

                newEel =
                    if hit then
                        { eel
                            | timeline =
                                Animator.updateTimeline model.time eel.timeline
                                    |> Animator.queue
                                        [ Animator.event (Animator.seconds 1) Eel.Hidden
                                        , Animator.wait (Animator.seconds 5)
                                        , Animator.event (Animator.seconds 2) Eel.Resting
                                        ]
                        }

                    else
                        { eel | timeline = Animator.updateTimeline model.time eel.timeline }

                newLives =
                    if hit then
                        lives - 1

                    else
                        lives
            in
            animateEelsHelp remainingEels newLives (newEel :: resultEels) model


eatPlankters : Model -> Model
eatPlankters model =
    eatPlanktersHelp model.eels model.plankters [] [] [] [] model


applyPlankterEvent : PlankterKind -> Model -> Model
applyPlankterEvent kind model =
    case kind of
        Slower ->
            { model
                | current =
                    Animator.interrupt
                        [ Animator.event (Animator.seconds 2) Const.minCurrent
                        , Animator.wait (Animator.seconds 15)
                        , Animator.event (Animator.seconds 2) Const.midCurrent
                        ]
                        model.current
                , score = model.score + 10
            }

        Faster ->
            { model
                | current =
                    Animator.interrupt
                        [ Animator.event (Animator.seconds 2) Const.maxCurrent
                        , Animator.wait (Animator.seconds 15)
                        , Animator.event (Animator.seconds 2) Const.midCurrent
                        ]
                        model.current
                , score = model.score + 10
            }

        Bonus ->
            { model | score = model.score + 30 }

        Normal ->
            { model | score = model.score + 10 }

        Poisonous ->
            { model | lives = model.lives - 1 }


eatPlanktersHelp : List Eel -> List Plankter -> List Plankter -> List Eel -> List Plankter -> List PlankterKind -> Model -> Model
eatPlanktersHelp currentEels currentPlankters requeuedPlankters resultEels resultPlankters resultEvents model =
    case currentEels of
        [] ->
            List.foldl applyPlankterEvent
                { model
                    | eels = resultEels
                    , plankters = requeuedPlankters ++ currentPlankters ++ resultPlankters
                }
                resultEvents

        eel :: remainingCurrentEels ->
            if
                (Animator.current eel.timeline == Eel.Resting)
                    && (Animator.arrived eel.timeline == Eel.Resting)
            then
                case currentPlankters of
                    [] ->
                        -- try with the next eel
                        eatPlanktersHelp remainingCurrentEels requeuedPlankters [] (eel :: resultEels) resultPlankters resultEvents model

                    plankter :: remainingCurrentPlankters ->
                        if Animator.current plankter.timeline == Plankter.Eaten then
                            if
                                (Animator.arrived plankter.timeline == Plankter.Eaten)
                                    || Quantity.lessThan (Quantity.negate Coordinates.maxX) (Point2d.xCoordinate plankter.position)
                            then
                                -- completely remove the plankter that has been eaten or moved outside the screen
                                eatPlanktersHelp remainingCurrentEels remainingCurrentPlankters requeuedPlankters (eel :: resultEels) resultPlankters (plankter.kind :: resultEvents) model

                            else
                                -- skip the plankter that is currently being eaten
                                eatPlanktersHelp (eel :: remainingCurrentEels) remainingCurrentPlankters requeuedPlankters resultEels (plankter :: resultPlankters) resultEvents model

                        else if canEat (getCurrent model.current) plankter eel then
                            let
                                newEel =
                                    { eel
                                        | timeline =
                                            if plankter.kind == Poisonous then
                                                eel.timeline
                                                    |> Animator.queue
                                                        [ Animator.event Animator.quickly (Eel.Striking (Plankter.positionIn Animator.quickly (getCurrent model.current) plankter))
                                                        , Animator.event Animator.quickly Eel.Resting
                                                        , Animator.event (Animator.seconds 1) Eel.Hidden
                                                        , Animator.wait (Animator.seconds 5)
                                                        , Animator.event (Animator.seconds 2) Eel.Resting
                                                        ]

                                            else
                                                eel.timeline
                                                    |> Animator.queue
                                                        [ Animator.event Animator.quickly (Eel.Striking (Plankter.positionIn Animator.quickly (getCurrent model.current) plankter))
                                                        , Animator.event Animator.quickly Eel.Resting
                                                        ]
                                    }

                                newPlankter =
                                    { plankter | timeline = Animator.go Animator.quickly Plankter.Eaten plankter.timeline }
                            in
                            -- eat the plankter
                            eatPlanktersHelp remainingCurrentEels remainingCurrentPlankters requeuedPlankters (newEel :: resultEels) (newPlankter :: resultPlankters) resultEvents model

                        else
                            -- requeue the plankter for the next
                            eatPlanktersHelp (eel :: remainingCurrentEels) remainingCurrentPlankters (plankter :: requeuedPlankters) resultEels resultPlankters resultEvents model

            else
                -- skip the busy eel
                eatPlanktersHelp remainingCurrentEels currentPlankters requeuedPlankters (eel :: resultEels) resultPlankters resultEvents model


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
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onMouseDown
            (Decode.map2 MouseDown
                (Decode.field "pageX" Decode.float)
                (Decode.field "pageY" Decode.float)
            )
        ]


view : Model -> Html a
view { current, eels, splashes, plankters, lives, score } =
    Html.div
        [ Html.Attributes.style "position" "relative"
        , Html.Attributes.style "width" "960px"
        , Html.Attributes.style "height" "640px"
        , Html.Attributes.style "background" "url(img/eels-bg.svg)"
        , Html.Attributes.style "-webkit-touch-callout" "none"
        , Html.Attributes.style "-webkit-user-select" "none"
        , Html.Attributes.style "-khtml-user-select" "none"
        , Html.Attributes.style "-moz-user-select" "none"
        , Html.Attributes.style "-ms-user-select" "none"
        , Html.Attributes.style "user-select" "none"
        ]
        [ Coordinates.view
            (List.concat
                [ List.map Plankter.view plankters
                , List.map (Eel.view (getCurrent current)) eels
                , List.map Splash.view splashes
                ]
                ++ [ Svg.placeIn Coordinates.topLeftFrame
                        (Svg.text_
                            [ Html.Attributes.style "font" "24px/1 sans-serif"
                            , Svg.Attributes.x "20"
                            , Svg.Attributes.y "40"
                            ]
                            [ Svg.text ("Lives: " ++ String.fromInt lives ++ ", score: " ++ String.fromInt score) ]
                        )
                   ]
            )
        ]
