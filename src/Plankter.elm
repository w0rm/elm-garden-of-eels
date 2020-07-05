module Plankter exposing (Plankter, PlankterKind(..), PlankterState(..), positionIn, random, view)

import Animator exposing (Timeline)
import Circle2d
import Coordinates exposing (World)
import Direction2d
import Duration exposing (Duration)
import Frame2d
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


type PlankterKind
    = Normal
    | Poisonous
    | Slower
    | Faster
    | Bonus


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
        (Frame2d.atPoint plankter.position |> Frame2d.at Coordinates.pixelDensity)
        (case plankter.kind of
            Poisonous ->
                Svg.path
                    [ SvgAttributes.fill "black"
                    , SvgAttributes.d "M3.85549 0.497921C4.10012 0.497921 4.16127 -0.419444 4.16127 -0.878126L5.91951 -0.954571C6.04694 -1.0565 6.2712 -1.47441 6.14889 -2.33062C5.99599 -3.40088 7.67783 -4.54758 6.68402 -4.31824C5.88897 -4.13477 4.9767 -4.75144 4.61995 -5.08271L3.09101 -4.85337L1.9443 -4.31824L2.78522 -3.78311L1.40917 -3.40088L-2.56607 -6.00007L-5.08883 -5.69429L-4.78304 -4.31824L-6.54132 -2.33062L-7 -0.266548C-6.51584 0.217617 -5.50164 1.1095 -5.31817 0.803711C-5.08883 0.421476 -3.2541 -1.33681 -2.56607 -0.648783C-2.01566 -0.0983639 -2.79541 0.803711 -3.2541 1.18595L-1.41937 0.497921L-0.807792 1.56818L-0.34911 0.497921C-0.247181 -0.139137 0.109572 -1.22978 0.721148 -0.495889C1.48562 0.421476 -0.960686 3.09712 -1.41937 2.2562C-1.87805 1.41529 -5.08883 2.71489 -5.54751 3.09712C-6.00619 3.47936 -3.33054 6.38434 -2.33673 6.76658C-1.34292 7.14882 0.95049 3.86159 0.721148 4.47317C0.491807 5.08474 3.16746 6.99592 4.16127 6.38434C5.15509 5.77277 3.5497 0.497921 3.85549 0.497921Z"
                    ]
                    []

            Normal ->
                Svg.circle2d [ SvgAttributes.fill "black" ]
                    (Circle2d.atOrigin (Length.meters 0.01)
                        |> Circle2d.at Coordinates.pixelDensity
                    )

            _ ->
                Svg.path
                    [ SvgAttributes.fill "black"
                    , SvgAttributes.d "M1.51369 -4.80906C1.32623 -5.54185 -5.35972 -4.13265 -4.98481 -1.14515C-4.98481 1.27868 -3.48515 2.40603 -2.11047 3.02608C-0.735791 3.64613 -1.42313 4.94259 -0.0484505 4.99896C1.32623 5.05533 0.201491 2.80061 1.82611 3.02608C3.45074 3.25155 4.20056 2.12419 4.82542 0.884099C5.45027 -0.355995 4.26305 0.0949483 3.38825 -0.412363C2.51346 -0.919675 3.63819 -2.49798 3.01334 -3.11802C2.38848 -3.73807 2.38848 -2.49798 1.51369 -3.11802C0.63889 -3.73807 1.70114 -4.07628 1.51369 -4.80906Z"
                    ]
                    []
        )
