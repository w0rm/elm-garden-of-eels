module Main exposing (main)

import Browser
import Browser.Events
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode exposing (Decoder)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Svg
import Svg.Attributes as SvgAttributes
import Vector2d


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { mouse = Point2d.origin }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        MouseMoved mouse ->
            ( { mouse = Point2d.placeIn topLeftFrame mouse }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onMouseMove (Decode.map MouseMoved decodePosition)


decodePosition : Decoder (Point2d Pixels Screen)
decodePosition =
    Decode.map2
        Point2d.pixels
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


type alias Model =
    { mouse : Point2d Pixels World
    }


type Msg
    = MouseMoved (Point2d Pixels Screen)


type World
    = World


type Screen
    = Screen


type alias Eel =
    { burrow : Point2d Pixels World
    , burrowDirection : Direction2d World
    , length : Quantity Float Pixels
    , head : Point2d Pixels World
    , headDirection : Direction2d World
    }


eel : Eel
eel =
    { burrow = Point2d.pixels 0 0
    , burrowDirection = Direction2d.y
    , length = Pixels.pixels 60
    , head = Point2d.pixels 10 50
    , headDirection = Direction2d.x
    }


eelSpline : Eel -> Point2d Pixels World -> CubicSpline2d Pixels World
eelSpline { burrow, burrowDirection, length, head, headDirection } mouse =
    let
        target =
            if
                Quantity.lessThan
                    (Quantity.multiplyBy 0.95 length)
                    (Point2d.distanceFrom burrow mouse)
            then
                mouse

            else
                head

        getSpline d =
            let
                len =
                    Quantity.multiplyBy d length

                spline =
                    CubicSpline2d.fromEndpoints
                        burrow
                        (Vector2d.withLength len burrowDirection)
                        target
                        (Vector2d.withLength len headDirection)
            in
            if
                Quantity.equalWithin
                    (Pixels.pixels 1)
                    length
                    (splineLength spline)
            then
                spline

            else
                getSpline (d + 0.01)
    in
    getSpline 0.01


splineLength : CubicSpline2d Pixels World -> Quantity Float Pixels
splineLength spline =
    spline
        |> CubicSpline2d.nondegenerate
        |> Result.map
            (CubicSpline2d.arcLengthParameterized
                { maxError = Pixels.pixels 0.5 }
                >> CubicSpline2d.arcLength
            )
        |> Result.withDefault Quantity.zero


topLeftFrame : Frame2d Pixels World { defines : Screen }
topLeftFrame =
    Frame2d.atPoint (Point2d.pixels -320 240)
        |> Frame2d.reverseY


view : Model -> Html a
view { mouse } =
    Svg.svg
        [ HtmlAttributes.style "display" "block"
        , SvgAttributes.width "640"
        , SvgAttributes.height "480"
        ]
        [ Svg.relativeTo topLeftFrame
            (Svg.cubicSpline2d
                [ SvgAttributes.fill "transparent"
                , SvgAttributes.stroke "black"
                , SvgAttributes.strokeWidth "5"
                , SvgAttributes.strokeLinecap "round"
                ]
                (eelSpline eel mouse)
            )
        ]
