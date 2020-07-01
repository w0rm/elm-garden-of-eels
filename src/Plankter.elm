module Plankter exposing (Plankter, PlankterKind(..), PlankterState(..), positionIn, random, view)

import Animator exposing (Timeline)
import Circle2d
import Coordinates exposing (World)
import Direction2d
import Duration exposing (Duration)
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
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


type PlankterKind
    = Normal
    | Poisonous
    | Slower
    | Faster
    | Bonus


kindToText : PlankterKind -> String
kindToText kind =
    case kind of
        Normal ->
            ""

        Poisonous ->
            "poison"

        Slower ->
            "slower"

        Faster ->
            "faster"

        Bonus ->
            "bonus"


randomKind : Random.Generator PlankterKind
randomKind =
    Random.weighted
        ( 50, Normal )
        [ ( 20, Poisonous )
        , ( 10, Slower )
        , ( 10, Faster )
        , ( 15, Bonus )
        ]


type alias Plankter =
    { position : Point2d Meters World
    , timeline : Timeline PlankterState
    , kind : PlankterKind
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
    Length.inMeters Coordinates.maxY - 0.3


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
        |> Random.map2
            (\kind y ->
                { timeline = Animator.init Moving
                , position = Point2d.xy Coordinates.maxX (Length.meters (minTop + y))
                , kind = kind
                }
            )
            randomKind


view : Plankter -> Html a
view plankter =
    Svg.placeIn
        (Frame2d.atPoint plankter.position
            |> Frame2d.at Coordinates.pixelDensity
        )
        (Svg.g
            []
            [ Svg.circle2d [ SvgAttributes.fill "black" ]
                (Circle2d.atOrigin (Length.meters 0.01)
                    |> Circle2d.at Coordinates.pixelDensity
                )
            , Svg.text_
                [ SvgAttributes.x "10"
                , SvgAttributes.y "10"
                , HtmlAttributes.style "font" "12px/1.3 sans-serif"
                , SvgAttributes.transform "scale(1,-1)"
                ]
                [ Svg.text (kindToText plankter.kind) ]
            ]
        )
