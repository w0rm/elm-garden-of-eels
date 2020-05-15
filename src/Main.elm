module Main exposing (main)

import Browser
import Browser.Events
import Const
import Coordinates exposing (World)
import Eel
import Html exposing (Html)
import Json.Decode as Decode
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Quantity


type alias Model =
    { mouse : Point2d Meters World
    }


type Msg
    = MouseMoved Float Float


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
        MouseMoved x y ->
            ( { mouse = Coordinates.screenToWorld (Point2d.pixels x y)
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onMouseMove
        (Decode.map2 MouseMoved
            (Decode.field "pageX" Decode.float)
            (Decode.field "pageY" Decode.float)
        )


view : Model -> Html a
view { mouse } =
    Coordinates.view
        (List.range 0 4
            |> List.map (\i -> toFloat i / 4)
            |> List.map
                (\t ->
                    Eel.view
                        (Eel.init
                            (Quantity.interpolateFrom Const.maxCurrent Const.minCurrent t)
                            (Length.meters 0.6)
                            (Point2d.interpolateFrom (Point2d.meters -1.2 -0.2) (Point2d.meters 1.2 -0.2) t)
                        )
                        mouse
                )
        )
