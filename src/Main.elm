module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Browser
import Browser.Events
import Const
import Coordinates exposing (World)
import Eel exposing (Eel)
import Html exposing (Html)
import Json.Decode as Decode
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Quantity
import Speed exposing (Speed)
import Time


type alias Model =
    { mouse : Point2d Meters World
    , current : Speed
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
                                            (Point2d.meters -1 -0.2)
                                            (Point2d.meters 1.2 -0.2)
                                            t
                                        )
                                )
                  , current =
                        Quantity.interpolateFrom
                            Const.minCurrent
                            Const.maxCurrent
                            0.5
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
            ( { model | eels = List.map (updateEel model.current model.mouse time) model.eels }
            , Cmd.none
            )


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


updateEel : Speed -> Point2d Meters World -> Time.Posix -> Eel -> Eel
updateEel current mouse time eel =
    let
        { length, head, burrow } =
            Eel.properties current eel

        newEel =
            if
                Quantity.lessThan (Quantity.multiplyBy 0.98 length) (Point2d.distanceFrom burrow mouse)
                    && Quantity.lessThan (Length.meters 0.2) (Point2d.distanceFrom head mouse)
                    && Quantity.lessThan (Point2d.xCoordinate mouse) (Point2d.xCoordinate head)
                    && (Animator.previous eel.timeline == Eel.Resting)
                    && (Animator.current eel.timeline == Eel.Resting)
            then
                { eel
                    | timeline =
                        eel.timeline
                            |> Animator.interrupt
                                [ Animator.event Animator.quickly (Eel.Striking mouse)
                                , Animator.wait (Animator.millis 1)
                                , Animator.event Animator.quickly Eel.Resting
                                ]
                }

            else
                eel
    in
    { newEel | timeline = Animator.update time animator newEel.timeline }


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
view { current, eels } =
    Html.div []
        [ Coordinates.view
            (List.map (Eel.view current) eels)
        ]


animator : Animator (Timeline any)
animator =
    Animator.watching identity always Animator.animator
