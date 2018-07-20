module Main exposing (main)

import Html exposing (Html, div, text)
import Dict exposing (Dict)
import Maybe.Extra


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
    , components : List Component
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
                [ TransformComponent { x = 1, y = 2 }
                ]
          }
        , { id = 2
          , components =
                [ TransformComponent { x = 2, y = 0 }
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
            (\y ->
                List.range 0 2
                    |> List.map
                        (\x ->
                            -- text (toString ( x, y ))
                            model.actors
                                |> List.map
                                    (\a ->
                                        a.components
                                            |> List.map
                                                (\c ->
                                                    case c of
                                                        TransformComponent d ->
                                                            Just ( a, d )
                                                 -- _ ->
                                                 --     Nothing
                                                )
                                    )
                                |> List.concat
                                |> Maybe.Extra.values
                                |> List.filter
                                    (\( a, d ) ->
                                        d.x == x && d.y == y
                                    )
                                |> List.head
                                |> Maybe.andThen
                                    (\( a, _ ) ->
                                        Just <| text <| "[" ++ (toString (a.id)) ++ "]"
                                    )
                                |> Maybe.withDefault (text "[]")
                        )
                    |> div []
            )
        |> div []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
