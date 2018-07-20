module Main exposing (main)

import Html exposing (Html, div, text)
import Dict exposing (Dict)


type alias Model =
    { actors : List Actor }


type alias TransformData =
    { x : Int
    , y : Int
    }


type Component
    = TransformComponent TransformData


type alias Actor =
    { id : Int
    , components : Dict String Component
    }


type Msg
    = NoOp


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    { actors =
        [ { id = 1
          , components =
                Dict.fromList
                    [ ( "transform", TransformComponent { x = 1, y = 2 } )
                    ]
          }
        ]
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    List.range 0 2
        |> List.map
            (\x ->
                List.range 0 2
                    |> List.map
                        (\y ->
                            text (toString ( x, y ))
                        )
                    |> div []
            )
        |> div []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
