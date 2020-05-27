module Plankter exposing (Plankter, PlankterState(..), positionIn, random, view)

import Animator exposing (Timeline)
import Circle2d
import Coordinates exposing (World)
import Direction2d
import Duration exposing (Duration)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Quantity
import Random exposing (Generator)
import Speed exposing (Speed)
import Svg
import Svg.Attributes as SvgAttributes


type PlankterState
    = Moving
    | Eaten


type alias Plankter =
    { position : Point2d Meters World
    , timeline : Timeline PlankterState
    }


positionIn : Duration -> Speed -> Plankter -> Point2d Meters World
positionIn duration current { position } =
    let
        distance =
            Quantity.for duration current
    in
    Point2d.translateIn Direction2d.negativeX distance position


maxTop : Float
maxTop =
    Length.inMeters Coordinates.maxY


minTop : Float
minTop =
    -0.3


random : Generator Plankter
random =
    Random.weighted ( 20, 0 ) [ ( 30, 1 ), ( 50, 2 ) ]
        |> Random.andThen
            (\bucket ->
                let
                    interval =
                        (maxTop - minTop) / 3
                in
                Random.float (bucket * interval) ((bucket - 1) * interval)
            )
        |> Random.map
            (\y ->
                { timeline = Animator.init Moving
                , position = Point2d.xy Coordinates.maxX (Length.meters (minTop + y))
                }
            )


view : Plankter -> Html a
view plankton =
    Svg.circle2d
        [ SvgAttributes.fill "transparent"
        , SvgAttributes.stroke "black"
        , SvgAttributes.strokeWidth "1"
        ]
        (Circle2d.atPoint plankton.position
            (Length.meters 0.006)
            |> Circle2d.at Coordinates.pixelDensity
        )
