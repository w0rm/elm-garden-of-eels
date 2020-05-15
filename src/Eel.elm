module Eel exposing (Eel, init, view)

import Angle
import Circle2d
import Const
import Coordinates exposing (World)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Speed exposing (Speed)
import Svg
import Svg.Attributes as SvgAttributes
import Vector2d


type alias Eel =
    { burrow : Point2d Meters World
    , burrowDirection : Direction2d World
    , length : Length
    , head : Point2d Meters World
    , headDirection : Direction2d World
    }


init : Speed -> Length -> Point2d Meters World -> Eel
init current length burrow =
    let
        minCurrentHeadOffset =
            Vector2d.meters 0.02 0.55

        maxCurrentHeadOffset =
            Vector2d.meters -0.1 0.4

        t =
            Quantity.ratio (Quantity.minus Const.minCurrent current) (Quantity.minus Const.minCurrent Const.maxCurrent)

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


view : Eel -> Point2d Meters World -> Html a
view { burrow, burrowDirection, length, head, headDirection } mouse =
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
            (CubicSpline2d.at Coordinates.pixelDensity (getSpline 0.01))
        , Svg.cubicSpline2d
            [ SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "white"
            , SvgAttributes.strokeWidth "10"
            , SvgAttributes.strokeLinecap "round"
            ]
            (CubicSpline2d.at Coordinates.pixelDensity (getSpline 0.01))
        , Svg.circle2d
            [ SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "black"
            , SvgAttributes.strokeWidth "1"
            ]
            (Circle2d.atPoint (Point2d.translateIn headDirection (Length.meters -0.02) target)
                (Length.meters 0.006)
                |> Circle2d.at Coordinates.pixelDensity
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
