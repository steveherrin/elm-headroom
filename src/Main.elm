module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { scale : Float
    , rotation : Float
    , transX : Float
    , transY : Float
    }


initialModel : Model
initialModel =
    { scale = 1
    , rotation = 0
    , transX = 0
    , transY = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- CONSTANTS


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Segment =
    { start : Point
    , end_ : Point
    }


type alias Wall =
    { color : String
    , segments : List Segment
    }


nLines =
    16


rotate { x, y, z } =
    -- rotate (x, y, z) such that
    -- (1, 1, 1) -> (0, 0, sqrt(3))
    -- (1, 1, -1) -> (2*sqrt(2/3), 0, 1/sqrt(3))
    { x = x / sqrt 6 + y / sqrt 6 - 2 * z / sqrt 6
    , y = -x / sqrt 2 + y / sqrt 2
    , z = x / sqrt 3 + y / sqrt 3 + z / sqrt 3
    }


walls =
    let
        wall1 i =
            { start = rotate { x = 1.0, y = i / nLines, z = 1.0 }
            , end = rotate { x = -1.0, y = i / nLines, z = 1.0 }
            }

        wall2 i =
            { start = rotate { x = 1.0, y = i / nLines, z = 1.0 }
            , end = rotate { x = 1.0, y = i / nLines, z = -1.0 }
            }

        wall3 i =
            -- not sure why I need the factor of 2 (TODO)
            { start = rotate { x = 1.0, y = 1.0 - 1.0 / nLines, z = 1 - 2 * i / nLines }
            , end = rotate { x = -1.0, y = 1.0 - 1.0 / nLines, z = 1 - 2 * i / nLines }
            }
    in
    [ { color = "#ff0000"
      , segments =
            List.map wall1
                (List.map toFloat <|
                    List.range 0 (nLines - 1)
                )
      }
    , { color = "#00ff00"
      , segments =
            List.map wall2
                (List.map toFloat <|
                    List.range 0 (nLines - 1)
                )
      }
    , { color = "#0000ff"
      , segments =
            List.map wall3
                (List.map toFloat <|
                    List.range 0 (nLines - 1)
                )
      }
    ]



-- UPDATE


type Msg
    = ScaleUp
    | ScaleDown
    | RotateCW
    | RotateCCW
    | TransPlusX
    | TransMinusX
    | TransPlusY
    | TransMinusY


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScaleUp ->
            ( { model | scale = model.scale + 0.1 }, Cmd.none )

        ScaleDown ->
            ( { model | scale = model.scale - 0.1 }, Cmd.none )

        RotateCW ->
            ( { model | rotation = model.rotation + 5 }, Cmd.none )

        RotateCCW ->
            ( { model | rotation = model.rotation - 5 }, Cmd.none )

        TransPlusX ->
            ( { model | transX = model.transX + 0.05 }, Cmd.none )

        TransMinusX ->
            ( { model | transX = model.transX - 0.05 }, Cmd.none )

        TransPlusY ->
            ( { model | transY = model.transY + 0.05 }, Cmd.none )

        TransMinusY ->
            ( { model | transY = model.transY - 0.05 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        transforms =
            "scale("
                ++ toString model.scale
                ++ "), rotate("
                ++ toString model.rotation
                ++ "), translate("
                ++ toString model.transX
                ++ " "
                ++ toString model.transY
                ++ ")"

        l color segment =
            line
                [ x1 (toString segment.start.x)
                , x2 (toString segment.end.x)
                , y1 (toString segment.start.y)
                , y2 (toString segment.end.y)
                , stroke color
                , strokeWidth "0.01"
                , transform transforms
                ]
                []

        segments w =
            List.map (l w.color) w.segments
    in
    div []
        [ svg
            [ viewBox "-0.65 -0.65 1.3 1.3"
            , width "600px"
            , Svg.Attributes.style "background-color: black"
            ]
            (List.concat (List.map segments walls))
        , div []
            [ button [ onClick ScaleDown ] [ Html.text "Scale-" ]
            , button [ onClick ScaleUp ] [ Html.text "Scale+" ]
            , button [ onClick RotateCW ] [ Html.text "CW" ]
            , button [ onClick RotateCCW ] [ Html.text "CCW" ]
            , button [ onClick TransPlusX ] [ Html.text "X+" ]
            , button [ onClick TransMinusX ] [ Html.text "X-" ]
            , button [ onClick TransPlusY ] [ Html.text "Y+" ]
            , button [ onClick TransMinusY ] [ Html.text "Y-" ]
            ]
        , div []
            [ Html.text ("Scale: " ++ toString model.scale)
            , Html.text ("Rot: " ++ toString model.rotation)
            , Html.text ("Trans: (" ++ toString model.transX ++ " , " ++ toString model.transY ++ ")")
            ]
        ]
