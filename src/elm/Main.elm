module Main exposing (main)

import Html exposing (Html, div, text)
import Maybe.Extra
import Keyboard
import Dict exposing (Dict)


type alias Model =
    { actors : Dict Int Actor }


type alias Position =
    { x : Int
    , y : Int
    }


type ObjectTypeData
    = Player
    | Enemy


type Component
    = TransformComponent Position
    | KeyboardComponent
    | ObjectTypeComponent ObjectTypeData


type alias Actors =
    Dict Int Actor


type alias Actor =
    { id : Int
    , components : List Component
    }


type KeyCodes
    = LeftArrow
    | UpArrow
    | RightArrow
    | DownArrow


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


validKeyCodesMap : Dict Keyboard.KeyCode KeyCodes
validKeyCodesMap =
    Dict.fromList
        [ ( leftArrow, LeftArrow )
        , ( rightArrow, RightArrow )
        , ( downArrow, DownArrow )
        , ( upArrow, UpArrow )
        ]


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
                    , ObjectTypeComponent Player
                    ]
                }
              )
            , ( 2
              , { id = 2
                , components =
                    [ TransformComponent { x = 1, y = 1 }
                    , ObjectTypeComponent Enemy
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


isActorPlayer : Actor -> Bool
isActorPlayer actor =
    List.filter
        (\c ->
            case c of
                ObjectTypeComponent Player ->
                    True

                _ ->
                    False
        )
        actor.components
        |> List.isEmpty
        |> not


removeEnemiesAtPlayer : Actor -> Actors -> Actors
removeEnemiesAtPlayer actor acc =
    [ actor ]
        |> List.filter isActorPlayer
        |> List.filterMap getPosition
        |> List.map
            (\position ->
                actorsAt position acc
            )
        |> List.foldr
            (\actors acc ->
                Dict.filter
                    (\actorId _ ->
                        actor.id /= actorId
                    )
                    actors
                    |> Dict.foldr
                        (\actorId _ acc ->
                            Dict.remove actorId acc
                        )
                        acc
            )
            acc


handleKeyboardEvent : Keyboard.KeyCode -> Actors -> Actors
handleKeyboardEvent keycode actors =
    List.foldr
        (\( actorId, actor ) acc ->
            List.foldr
                (\component acc ->
                    case component of
                        KeyboardComponent ->
                            updateKeyboardComponent keycode actor acc
                                |> removeEnemiesAtPlayer actor

                        _ ->
                            acc
                )
                acc
                actor.components
        )
        actors
        (Dict.toList actors)


moveFromKeyCode : Keyboard.KeyCode -> Position -> Position
moveFromKeyCode keycode position =
    case Dict.get keycode validKeyCodesMap of
        Just LeftArrow ->
            { x = max 0 (position.x - 1), y = position.y }

        Just RightArrow ->
            { x = min 2 (position.x + 1), y = position.y }

        Just UpArrow ->
            { x = position.x, y = max 0 (position.y - 1) }

        Just DownArrow ->
            { x = position.x, y = min 2 (position.y + 1) }

        _ ->
            position


actorsAt : Position -> Actors -> Actors
actorsAt position actors =
    Dict.filter
        (\actorId actor ->
            getPosition actor
                |> Maybe.andThen
                    (\actorPos ->
                        Just <| actorPos == position
                    )
                |> Maybe.withDefault False
        )
        actors


updateKeyboardComponent : Keyboard.KeyCode -> Actor -> Actors -> Actors
updateKeyboardComponent keycode actor acc =
    getPosition actor
        |> Maybe.andThen
            (\position ->
                Just <|
                    moveFromKeyCode keycode position
            )
        |> Maybe.andThen
            (\position ->
                let
                    newComponents =
                        List.filter
                            (\c ->
                                isTransformComponent c |> not
                            )
                            actor.components
                in
                    Just { actor | components = (TransformComponent position) :: newComponents }
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


isTransformComponent : Component -> Bool
isTransformComponent component =
    case component of
        TransformComponent _ ->
            True

        _ ->
            False


getPosition : Actor -> Maybe Position
getPosition actor =
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
