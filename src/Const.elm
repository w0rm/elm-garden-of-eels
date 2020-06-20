module Const exposing
    ( maxCurrent
    , maxCurrentBurrowDirection
    , maxCurrentHeadOffset
    , midCurrent
    , minCurrent
    , minCurrentBurrowDirection
    , minCurrentHeadOffset
    )

import Angle exposing (Angle)
import Coordinates exposing (World)
import Length exposing (Meters)
import Quantity
import Speed exposing (Speed)
import Vector2d exposing (Vector2d)


minCurrent : Speed
minCurrent =
    Speed.metersPerSecond ((3.3 + 7) / 100)


midCurrent : Speed
midCurrent =
    Quantity.interpolateFrom
        minCurrent
        maxCurrent
        0.5


maxCurrent : Speed
maxCurrent =
    Speed.metersPerSecond ((29 + 5) / 100)


minCurrentBurrowDirection : Angle
minCurrentBurrowDirection =
    Angle.degrees 90


maxCurrentBurrowDirection : Angle
maxCurrentBurrowDirection =
    Angle.degrees 140


minCurrentHeadOffset : Vector2d Meters World
minCurrentHeadOffset =
    Vector2d.meters 0.02 0.55


maxCurrentHeadOffset : Vector2d Meters World
maxCurrentHeadOffset =
    Vector2d.meters -0.1 0.4
