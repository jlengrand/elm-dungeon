module Main exposing (main)

import Html exposing (Html, div, text)
import Maybe.Extra
import Keyboard
import Dict exposing (Dict)


type alias Model =
    { actors : Dict Int Actor }


type alias TransformData =
    { x : Int
    , y : Int
    }


type Component
    = TransformComponent TransformData
    | KeyboardComponent


type alias Actors =
    Dict Int Actor


type alias Actor =
    { id : Int
    , components : List Component
    }


leftArrow : Int
leftArrow =
    37


upArrow : Int
upArrow =
    38


rightArrow : Int
rightArrow =
    39


downArrow : Int
downArrow =
    40


validKeyCodes : List Int
validKeyCodes =
    [ leftArrow, rightArrow, downArrow, upArrow ]


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
        Dict.fromList
            [ ( 1
              , { id = 1
                , components =
                    [ TransformComponent { x = 1, y = 2 }
                    , KeyboardComponent
                    ]
                }
              )
            , ( 2
              , { id = 2
                , components =
                    [ TransformComponent { x = 2, y = 0 }
                    ]
                }
              )
            ]
    }
        ! []


type Msg
    = NoOp
    | KeyPressed Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyPressed keycode ->
            { model | actors = handleKeyboardEvent keycode model.actors } ! []


handleKeyboardEvent : Keyboard.KeyCode -> Actors -> Actors
handleKeyboardEvent keycode actors =
    List.foldr
        (\( actorId, actor ) acc ->
            List.foldr
                (\component acc ->
                    case component of
                        KeyboardComponent ->
                            updateKeyboardComponent keycode actor acc

                        _ ->
                            acc
                )
                acc
                actor.components
        )
        actors
        (Dict.toList actors)


updateKeyboardComponent : Keyboard.KeyCode -> Actor -> Actors -> Actors
updateKeyboardComponent keycode actor acc =
    if List.member keycode validKeyCodes then
        getTransformData actor
            |> Maybe.andThen
                (\transformData ->
                    let
                        _ =
                            Debug.log "hi" "hi"
                    in
                        Just
                            { x = transformData.x + 1
                            , y = transformData.y
                            }
                )
            |> Maybe.andThen
                (\transformData ->
                    let
                        newComponents =
                            List.filter
                                (\c ->
                                    isTransformComponent c |> not
                                )
                                actor.components
                    in
                        Just { actor | components = (TransformComponent transformData) :: newComponents }
                )
            |> Maybe.andThen
                (\actor ->
                    Just <|
                        Dict.insert
                            actor.id
                            actor
                            acc
                )
            |> Maybe.withDefault acc
    else
        let
            _ =
                Debug.log "not valid keycode" (toString keycode)
        in
            acc


isTransformComponent : Component -> Bool
isTransformComponent component =
    case component of
        TransformComponent _ ->
            True

        _ ->
            False


getTransformData : Actor -> Maybe TransformData
getTransformData actor =
    List.filterMap
        (\c ->
            case c of
                TransformComponent d ->
                    Just d

                _ ->
                    Nothing
        )
        actor.components
        |> List.head


view : Model -> Html Msg
view model =
    List.range 0 2
        |> List.map
            (\y ->
                List.range 0 2
                    |> List.map
                        (\x ->
                            -- text (toString ( x, y ))
                            Dict.toList model.actors
                                |> List.map
                                    (\( _, a ) ->
                                        a.components
                                            |> List.map
                                                (\c ->
                                                    case c of
                                                        TransformComponent d ->
                                                            Just ( a, d )

                                                        _ ->
                                                            Nothing
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
    Keyboard.downs KeyPressed
