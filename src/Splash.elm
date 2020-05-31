module Splash exposing (Splash, SplashState(..), init, properties, velocityAt, view)

import Animator exposing (Timeline)
import Circle2d
import Coordinates exposing (World)
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length, Meters)
import Pixels
import Point2d exposing (Point2d)
import Quantity
import Speed exposing (MetersPerSecond)
import Svg
import Svg.Attributes as SvgAttributes
import Vector2d exposing (Vector2d)


type SplashState
    = Initial
    | Final


type alias Splash =
    { center : Point2d Meters World
    , timeline : Timeline SplashState
    }


maxWaveRadius : Length
maxWaveRadius =
    Length.meters 0.25


velocityAt : Point2d Meters World -> List Splash -> Vector2d MetersPerSecond World
velocityAt point splashes =
    case splashes of
        splash :: _ ->
            velocityAtHelp point splashes (Length.meters 1000) splash

        _ ->
            Vector2d.zero


velocityAtHelp : Point2d Meters World -> List Splash -> Length -> Splash -> Vector2d MetersPerSecond World
velocityAtHelp point splashes minDistance closestSplash =
    case splashes of
        splash :: remainingSplashes ->
            let
                distance =
                    Point2d.distanceFrom splash.center point

                { radius } =
                    properties splash
            in
            if Quantity.lessThan minDistance distance && Quantity.lessThan radius distance then
                velocityAtHelp point remainingSplashes distance splash

            else
                velocityAtHelp point remainingSplashes minDistance closestSplash

        [] ->
            let
                { center, radius } =
                    properties closestSplash
            in
            if Quantity.lessThan radius minDistance then
                let
                    ratio =
                        Quantity.ratio (Quantity.minus minDistance maxWaveRadius) maxWaveRadius

                    speed =
                        Speed.metersPerSecond 1
                            |> Quantity.multiplyBy ratio
                in
                case Direction2d.from center point of
                    Just dir ->
                        Vector2d.withLength speed dir

                    Nothing ->
                        Vector2d.zero

            else
                Vector2d.zero


properties : Splash -> { center : Point2d Meters World, radius : Length }
properties { timeline, center } =
    let
        radius =
            Animator.move timeline
                (\state ->
                    case state of
                        Initial ->
                            lengthToMovement Quantity.zero

                        Final ->
                            lengthToMovement maxWaveRadius
                )
                |> Length.meters
    in
    { center = center
    , radius = radius
    }


init : Point2d Meters World -> Splash
init center =
    { center = center
    , timeline =
        Animator.init Initial
            |> Animator.go (Animator.seconds 1) Final
    }


view : Splash -> Html a
view splash =
    let
        { center, radius } =
            properties splash

        ratio =
            Quantity.ratio radius maxWaveRadius

        segment =
            2 * pi * Pixels.inPixels (Quantity.at Coordinates.pixelDensity radius) / 36

        stroke =
            segment * (1 - ratio)

        space =
            segment * ratio
    in
    Svg.circle2d
        [ SvgAttributes.fill "transparent"
        , SvgAttributes.stroke "black"
        , SvgAttributes.strokeWidth "1"
        , SvgAttributes.strokeDasharray (String.fromFloat stroke ++ " " ++ String.fromFloat space)
        ]
        (Circle2d.atPoint center radius
            |> Circle2d.at Coordinates.pixelDensity
        )


lengthToMovement : Length -> Animator.Movement
lengthToMovement length =
    Animator.at (Length.inMeters length)
