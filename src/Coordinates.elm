module Coordinates exposing (World, pixelDensity, screenToWorld, view)

import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Svg
import Svg.Attributes as SvgAttributes


type World
    = World


type Screen
    = Screen


screenToWorld : Point2d Pixels Screen -> Point2d Meters World
screenToWorld point =
    point
        |> Point2d.placeIn topLeftFrame
        |> Point2d.at_ pixelDensity


topLeftFrame : Frame2d Pixels World { defines : Screen }
topLeftFrame =
    Frame2d.atPoint (Point2d.pixels -480 320)
        |> Frame2d.reverseY


pixelDensity : Quantity Float (Rate Pixels Meters)
pixelDensity =
    Quantity.per (Length.meters 1) (Pixels.pixels 300)


view : List (Html a) -> Html a
view html =
    Svg.svg
        [ HtmlAttributes.style "display" "block"
        , SvgAttributes.width "960"
        , SvgAttributes.height "640"
        ]
        [ Svg.relativeTo topLeftFrame (Svg.g [] html)
        ]
