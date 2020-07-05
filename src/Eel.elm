module Eel exposing (Eel, EelProperties, EelState(..), init, properties, view)

import Angle exposing (Angle)
import Animator exposing (Timeline)
import Circle2d
import Const
import Coordinates exposing (World)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d
import Ellipse2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length, Meters)
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Speed exposing (Speed)
import Svg
import Svg.Attributes as SvgAttributes
import Vector2d


type EelState
    = Hidden
    | Resting
    | Striking (Point2d Meters World)


type alias Eel =
    { burrow : Point2d Meters World
    , maxLength : Length
    , timeline : Timeline EelState
    , width : Length
    , eyeColor : String
    , bodyColor : String
    }


init : String -> String -> Length -> Length -> Point2d Meters World -> Eel
init eyeColor bodyColor width maxLength burrow =
    { burrow = burrow
    , eyeColor = eyeColor
    , bodyColor = bodyColor
    , width = width
    , maxLength = maxLength
    , timeline =
        Animator.init Hidden
            |> Animator.go (Animator.seconds 2) Resting
    }


type alias EelProperties =
    { headDirection : Angle
    , burrowDirection : Angle
    , head : Point2d Meters World
    , burrow : Point2d Meters World
    , length : Length
    }


properties : Speed -> Eel -> EelProperties
properties current { burrow, maxLength, timeline } =
    let
        t =
            Quantity.ratio
                (Quantity.minus Const.minCurrent current)
                (Quantity.minus Const.minCurrent Const.maxCurrent)

        headOffset =
            Vector2d.interpolateFrom
                Const.minCurrentHeadOffset
                Const.maxCurrentHeadOffset
                t

        restingHead =
            Point2d.translateBy headOffset burrow

        restingBurrowDirection =
            Quantity.interpolateFrom
                Const.minCurrentBurrowDirection
                Const.maxCurrentBurrowDirection
                t

        head =
            Animator.xy timeline
                (\state ->
                    case state of
                        Hidden ->
                            pointToMovement burrow

                        Resting ->
                            pointToMovement restingHead

                        Striking prey ->
                            pointToMovement prey
                )
                |> Point2d.fromMeters

        burrowDirection =
            Animator.move timeline
                (\state ->
                    case state of
                        Hidden ->
                            angleToMovement restingBurrowDirection

                        Resting ->
                            angleToMovement restingBurrowDirection

                        Striking _ ->
                            angleToMovement restingBurrowDirection
                )
                |> Angle.radians

        headDirection =
            Animator.move timeline
                (\state ->
                    case state of
                        Hidden ->
                            angleToMovement (Angle.degrees 90)

                        Resting ->
                            angleToMovement (Angle.degrees 0)

                        Striking _ ->
                            angleToMovement (Angle.degrees 0)
                )
                |> Angle.radians

        length =
            Animator.move timeline
                (\state ->
                    case state of
                        Hidden ->
                            lengthToMovement Quantity.zero
                                |> Animator.leaveSmoothly 0.5
                                |> Animator.arriveSmoothly 0.95

                        Resting ->
                            lengthToMovement maxLength

                        Striking _ ->
                            lengthToMovement maxLength
                )
                |> Length.meters
    in
    { headDirection = headDirection
    , burrowDirection = burrowDirection
    , head = head
    , burrow = burrow
    , length = length
    }


view : Speed -> Eel -> Html a
view current eel =
    let
        { headDirection, burrowDirection, head, burrow, length } =
            properties current eel

        getSpline d =
            let
                len =
                    Quantity.multiplyBy d length

                spline =
                    CubicSpline2d.fromEndpoints
                        burrow
                        (Vector2d.rTheta len burrowDirection)
                        head
                        (Vector2d.rTheta len headDirection)
            in
            if
                Quantity.lessThanOrEqualTo
                    (splineLength spline)
                    length
            then
                spline

            else
                getSpline (d + 0.02)

        eelSpline =
            getSpline 0.33
    in
    if length == Quantity.zero then
        Svg.g []
            [ Svg.ellipse2d
                [ SvgAttributes.fill "#000" ]
                (Ellipse2d.with
                    { centerPoint =
                        burrow
                            |> Point2d.translateIn Direction2d.negativeY (Length.meters 0.01)
                    , xDirection = Direction2d.x
                    , xRadius = Length.meters 0.06
                    , yRadius = Length.meters 0.03
                    }
                    |> Ellipse2d.at Coordinates.pixelDensity
                )
            ]

    else
        Svg.g []
            [ Svg.ellipse2d
                [ SvgAttributes.fill "#000" ]
                (Ellipse2d.with
                    { centerPoint =
                        burrow
                            |> Point2d.translateIn Direction2d.negativeY (Length.meters 0.01)
                    , xDirection = Direction2d.x
                    , xRadius = Length.meters 0.06
                    , yRadius = Length.meters 0.03
                    }
                    |> Ellipse2d.at Coordinates.pixelDensity
                )
            , Svg.cubicSpline2d
                [ SvgAttributes.fill "transparent"
                , SvgAttributes.stroke eel.bodyColor
                , SvgAttributes.strokeWidth (String.fromFloat (Pixels.inPixels (Quantity.at Coordinates.pixelDensity eel.width)))
                , SvgAttributes.strokeLinecap "round"
                ]
                (CubicSpline2d.at Coordinates.pixelDensity eelSpline)
            , Svg.circle2d
                [ SvgAttributes.fill eel.eyeColor ]
                (Circle2d.atPoint (Point2d.translateIn (Direction2d.fromAngle headDirection) (Length.meters -0.01) head)
                    (Length.meters 0.008)
                    |> Circle2d.at Coordinates.pixelDensity
                )
            ]


pointToMovement : Point2d Meters World -> { x : Animator.Movement, y : Animator.Movement }
pointToMovement point =
    let
        { x, y } =
            Point2d.toMeters point
    in
    { x = Animator.at x
    , y = Animator.at y
    }


angleToMovement : Angle -> Animator.Movement
angleToMovement angle =
    Animator.at (Angle.inRadians angle)


lengthToMovement : Length -> Animator.Movement
lengthToMovement length =
    Animator.at (Length.inMeters length)


splineLength : CubicSpline2d Meters World -> Quantity Float Meters
splineLength spline =
    spline
        |> CubicSpline2d.nondegenerate
        |> Result.map
            (CubicSpline2d.arcLengthParameterized
                { maxError = Length.meters 0.1 }
                >> CubicSpline2d.arcLength
            )
        |> Result.withDefault Quantity.zero
