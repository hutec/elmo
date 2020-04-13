module Plot exposing (..)

import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox, fontFamily, textAnchor)
import TypedSvg.Attributes.InPx exposing (strokeWidth, fontSize)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..), AnchorAlignment(..))
import Statistics
import Route exposing (Coordinate)
import Random


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


defaultOrPosix : Maybe (Int, Int) -> (Time.Posix, Time.Posix)
defaultOrPosix a = 
    case a of
        Nothing ->
            (Time.millisToPosix 1577833200000, Time.millisToPosix 1586642400000)

        Just (x , y) ->
            (Time.millisToPosix x, Time.millisToPosix y)


plotDistances : List ( Time.Posix, Float ) -> Svg msg
plotDistances model =
    let
        xScale : ContinuousScale Time.Posix
        xScale =
            model
                |> List.map (Tuple.first >> Time.posixToMillis)
                |> Statistics.extent
                |> defaultOrPosix
                |> Scale.time Time.utc ( 0, w - 2 * padding )

        xAxis : Svg msg
        xAxis =
            Axis.bottom [ Axis.tickCount 10 ] xScale

        yScale : ContinuousScale Float
        yScale = 
            model
                |> List.map Tuple.second
                |> List.maximum >> Maybe.withDefault 0
                |> (\b -> (0, b))
                |> Scale.linear (h - 2 * padding, 0)

        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickCount 5 ] yScale


        transformToLineData : ( Time.Posix, Float ) -> Maybe ( Float, Float )
        transformToLineData ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale y )


        line : List ( Time.Posix, Float ) -> Path
        line data =
            List.map transformToLineData data
                |> Shape.line Shape.linearCurve
                
    in 

    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (line model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        , g [ transform [ Translate (w / 2) padding ] ]
        [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorMiddle ] [ text "Route Distance" ]
        ]
        ]


{-| Plots a list of routes -}
plotRoutes : List (List Coordinate) -> Svg msg
plotRoutes model =
    let
        xScale : ContinuousScale Float
        xScale =
            model
                |> List.map ((List.map .latitude) >> List.maximum >> Maybe.withDefault 0)
                |> Statistics.extent
                |> Maybe.withDefault ( 48.75, 48.95 )
                |> Scale.linear ( 0, w - 2 * padding )

        xAxis : Svg msg
        xAxis =
            Axis.bottom [ Axis.tickCount 5 ] xScale

        yScale : ContinuousScale Float
        yScale = 
            model
                |> List.map ((List.map .longitude) >> List.maximum >> Maybe.withDefault 0)
                |> Statistics.extent
                |> Maybe.withDefault ( 8.64, 9.04 )
                |> Scale.linear (h - 2 * padding, 0)

        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickCount 5 ] yScale


        transformToLineData : Coordinate -> Maybe ( Float, Float )
        transformToLineData coord =
            Just ( Scale.convert xScale coord.latitude, Scale.convert yScale coord.longitude )

        line : List Coordinate -> Path
        line data =
            List.map transformToLineData data
                |> Shape.line Shape.linearCurve

        pathElement : List Coordinate -> Svg msg
        pathElement data =
            Path.element (line data) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 1, fill PaintNone ]

    in 
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] (List.map pathElement model)
        , g [ transform [ Translate (w / 2) padding ] ]
        [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorMiddle ] [ text "Routes" ]
        ]
        ]
