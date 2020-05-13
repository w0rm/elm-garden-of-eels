module Main exposing (main)

import Angle
import Browser
import Browser.Events
import Circle2d
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Length, Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed)
import Svg
import Svg.Attributes as SvgAttributes
import Vector2d


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { mouse = Point2d.meters 100 100 }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        MouseMoved mouse ->
            ( { mouse =
                    Point2d.placeIn topLeftFrame mouse
                        |> Point2d.at_ pixelDensity
              }
            , Cmd.none
            )


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
    { mouse : Point2d Meters World
    }


type Msg
    = MouseMoved (Point2d Pixels Screen)


type World
    = World


type Screen
    = Screen


type alias Eel =
    { burrow : Point2d Meters World
    , burrowDirection : Direction2d World
    , length : Length
    , head : Point2d Meters World
    , headDirection : Direction2d World
    }


minCurrent : Speed
minCurrent =
    Speed.metersPerSecond (3.3 / 100)


maxCurrent : Speed
maxCurrent =
    Speed.metersPerSecond (29 / 100)


eel : Speed -> Length -> Point2d Meters World -> Eel
eel current length burrow =
    let
        minCurrentHeadOffset =
            Vector2d.meters 0.02 0.55

        maxCurrentHeadOffset =
            Vector2d.meters -0.1 0.4

        t =
            Quantity.ratio (Quantity.minus minCurrent current) (Quantity.minus minCurrent maxCurrent)

        headOffset =
            Vector2d.interpolateFrom minCurrentHeadOffset maxCurrentHeadOffset t

        burrowDirection =
            Direction2d.fromAngle (Quantity.interpolateFrom (Angle.degrees 90) (Angle.degrees 140) t)
    in
    { burrow = burrow
    , burrowDirection = burrowDirection
    , length = length
    , head = Point2d.translateBy headOffset burrow
    , headDirection = Direction2d.x
    }


eelSpline : Eel -> Point2d Meters World -> Html a
eelSpline { burrow, burrowDirection, length, head, headDirection } mouse =
    let
        target =
            if
                Quantity.lessThan (Quantity.multiplyBy 0.98 length) (Point2d.distanceFrom burrow mouse)
                    && Quantity.lessThan (Length.meters 0.2) (Point2d.distanceFrom head mouse)
                    && Quantity.lessThan (Point2d.xCoordinate mouse) (Point2d.xCoordinate head)
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
                    (Length.meters 0.005)
                    length
                    (splineLength spline)
            then
                spline

            else
                getSpline (d + 0.01)
    in
    Svg.g []
        [ Svg.cubicSpline2d
            [ SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "black"
            , SvgAttributes.strokeWidth "12"
            , SvgAttributes.strokeLinecap "round"
            ]
            (CubicSpline2d.at pixelDensity (getSpline 0.01))
        , Svg.cubicSpline2d
            [ SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "white"
            , SvgAttributes.strokeWidth "10"
            , SvgAttributes.strokeLinecap "round"
            ]
            (CubicSpline2d.at pixelDensity (getSpline 0.01))
        , Svg.circle2d
            [ SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "black"
            , SvgAttributes.strokeWidth "1"
            ]
            (Circle2d.atPoint (Point2d.translateIn headDirection (Length.meters -0.02) target)
                (Length.meters 0.006)
                |> Circle2d.at pixelDensity
            )
        ]


splineLength : CubicSpline2d Meters World -> Quantity Float Meters
splineLength spline =
    spline
        |> CubicSpline2d.nondegenerate
        |> Result.map
            (CubicSpline2d.arcLengthParameterized
                { maxError = Length.meters 0.005 }
                >> CubicSpline2d.arcLength
            )
        |> Result.withDefault Quantity.zero


topLeftFrame : Frame2d Pixels World { defines : Screen }
topLeftFrame =
    Frame2d.atPoint (Point2d.pixels -320 240)
        |> Frame2d.reverseY


pixelDensity : Quantity Float (Rate Pixels Meters)
pixelDensity =
    Pixels.pixels 200 |> Quantity.per (Length.meters 1)


view : Model -> Html a
view { mouse } =
    Svg.svg
        [ HtmlAttributes.style "display" "block"
        , SvgAttributes.width "640"
        , SvgAttributes.height "480"
        ]
        [ Svg.relativeTo topLeftFrame
            (Svg.g []
                (List.range 0 4
                    |> List.map (\i -> toFloat i / 4)
                    |> List.map
                        (\t ->
                            eelSpline
                                (eel
                                    (Quantity.interpolateFrom maxCurrent minCurrent t)
                                    (Length.meters 0.6)
                                    (Point2d.interpolateFrom (Point2d.meters -1.2 -0.2) (Point2d.meters 1.2 -0.2) t)
                                )
                                mouse
                        )
                )
            )
        ]
