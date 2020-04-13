module Plot exposing (..)

import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale Time.Posix
xScale =
    Scale.time Time.utc ( 0, w - 2 * padding ) (Time.millisToPosix 1577833200000, Time.millisToPosix 1586642400000)



yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0.0, 200.0)


xAxis : Svg msg
xAxis =
    Axis.bottom [ Axis.tickCount 10 ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


line : List ( Time.Posix, Float ) -> Path
line model =
    List.map transformToLineData model
        |> Shape.line Shape.linearCurve


plotDistances : List ( Time.Posix, Float ) -> Svg msg
plotDistances model =
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (line model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        , g [ transform [ Translate (w / 2) (padding + 20) ] ]
        [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorMiddle ] [ text "Route Distance" ]
        ]
        ]