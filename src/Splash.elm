module Splash exposing (Splash, SplashState(..), init, properties, view)

import Animator exposing (Timeline)
import Circle2d
import Coordinates exposing (World)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Svg
import Svg.Attributes as SvgAttributes


type SplashState
    = Initial
    | Final


type alias Splash =
    { center : Point2d Meters World
    , timeline : Timeline SplashState
    }


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
                            lengthToMovement (Length.meters 0.15)
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
    in
    Svg.circle2d
        [ SvgAttributes.fill "transparent"
        , SvgAttributes.stroke "black"
        , SvgAttributes.strokeWidth "1"
        ]
        (Circle2d.atPoint center radius
            |> Circle2d.at Coordinates.pixelDensity
        )


lengthToMovement : Length -> Animator.Movement
lengthToMovement length =
    Animator.at (Length.inMeters length)
