module Main exposing (..)

import Html exposing (..)
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
    {}


initialModel : Model
initialModel =
    {}


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
    -- rotate (x, y, z) such that (1, 1, 1) -> (0, 0, sqrt(3))
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
            { start = rotate { x = 1.0, y = 1.0 - 1.0 / nLines, z = i / nLines }
            , end = rotate { x = -1.0, y = 1.0 - 1.0 / nLines, z = i / nLines }
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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        l color segment =
            line
                [ x1 (toString segment.start.x)
                , x2 (toString segment.end.x)
                , y1 (toString segment.start.y)
                , y2 (toString segment.end.y)
                , stroke color
                , strokeWidth "0.01"
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
        ]
