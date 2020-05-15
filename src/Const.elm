module Const exposing (maxCurrent, minCurrent)

import Speed exposing (Speed)


minCurrent : Speed
minCurrent =
    Speed.metersPerSecond (3.3 / 100)


maxCurrent : Speed
maxCurrent =
    Speed.metersPerSecond (29 / 100)
