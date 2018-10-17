module Main exposing (main)

-- import Browser.Events

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (id, property)
import Json.Encode exposing (string)
import Maybe.Extra
import String


type alias Model =
    { actors : Actors
    }


type alias Health =
    Int


type alias Monies =
    Int


type alias PlayerWeaponDurability =
    Int


type alias WeaponDurability =
    Int


type alias Strength =
    Int


type alias CoinValue =
    Int


type alias Position =
    { x : Int
    , y : Int
    }


type ObjectTypeData
    = Player
    | Enemy Strength
    | Coin CoinValue
    | Weapon WeaponDurability


type Component
    = TransformComponent Position
    | KeyboardComponent
    | MoniesCollectedComponent Monies
    | HealthComponent Health
    | WeaponComponent PlayerWeaponDurability
    | ObjectTypeComponent ObjectTypeData


type alias Actors =
    List Actor


type alias Actor =
    { id : Int
    , components : List Component
    }



-- type Direction
--     = Left
--     | Up
--     | Right
--     | Down
--     | Other


enemySymbol : String
enemySymbol =
    "👾"


coinSymbol : String
coinSymbol =
    "💰"


playerSymbol : String
playerSymbol =
    "👩\u{1F3FB}\u{200D}🚀"


weaponSymbol : String
weaponSymbol =
    "🔫"


startHealth : Int
startHealth =
    10


numberRows : Int
numberRows =
    3


numberColumns : Int
numberColumns =
    3


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



-- keyDecoder : Decode.Decoder Direction
-- keyDecoder =
--     Decode.map toDirection (Decode.field "key" Decode.string)
-- toDirection : String -> Direction
-- toDirection string =
--   case string of
--     "ArrowLeft" ->
--       Left
--     "ArrowRight" ->
--       Right
--     "ArrowUp" ->
--       Up
--     "ArrowDown" ->
--       Down
--     _ ->
--       Other
-- validKeyCodesMap : Dict Keyboard.KeyCode KeyCodes
-- validKeyCodesMap =
--     Dict.fromList
--         [ ( leftArrow, LeftArrow )
--         , ( rightArrow, RightArrow )
--         , ( downArrow, DownArrow )
--         , ( upArrow, UpArrow )
--         ]


componentToPrintSymbol : Component -> Maybe String
componentToPrintSymbol component =
    case component of
        ObjectTypeComponent Player ->
            Just playerSymbol

        ObjectTypeComponent (Enemy _) ->
            Just enemySymbol

        ObjectTypeComponent (Coin value) ->
            Just coinSymbol

        ObjectTypeComponent (Weapon _) ->
            Just weaponSymbol

        _ ->
            Maybe.Nothing


actorToPrintSymbol : Actor -> String
actorToPrintSymbol actor =
    actor.components
        |> List.filterMap
            componentToPrintSymbol
        |> List.head
        |> Maybe.withDefault playerSymbol


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actors =
            [ { id = 1
              , components =
                    [ TransformComponent { x = 1, y = 2 }
                    , KeyboardComponent
                    , ObjectTypeComponent Player
                    , MoniesCollectedComponent 0
                    , HealthComponent startHealth
                    , WeaponComponent 0
                    ]
              }
            , { id = 2
              , components =
                    [ TransformComponent { x = 1, y = 1 }
                    , ObjectTypeComponent (Enemy 2)
                    ]
              }
            , { id = 3
              , components =
                    [ TransformComponent { x = 0, y = 0 }
                    , ObjectTypeComponent (Coin 4)
                    ]
              }
            , { id = 4
              , components =
                    [ TransformComponent { x = 2, y = 2 }
                    , ObjectTypeComponent (Weapon 5)
                    ]
              }
            , { id = 5
              , components =
                    [ TransformComponent { x = 0, y = 2 }
                    , ObjectTypeComponent (Weapon 1)
                    ]
              }
            , { id = 6
              , components =
                    [ TransformComponent { x = 0, y = 1 }
                    , ObjectTypeComponent (Enemy 6)
                    ]
              }
            ]
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- KeyPressed keycode ->
--     ( { model | actors = handleKeyboardEvent keycode model.actors }
--     , Cmd.none
--     )


getActorFromId : Int -> Actors -> Maybe Actor
getActorFromId actorId actors =
    List.filter
        (\actor -> actor.id == actorId)
        actors
        |> List.head


getPlayerActor : Actors -> Maybe Actor
getPlayerActor actors =
    List.filter
        (\actor -> isActorPlayer actor)
        actors
        |> List.head


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


removeEnemiesAtPlayer : Int -> Actors -> Actors
removeEnemiesAtPlayer actorId acc =
    case getActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                    )
                |> List.filter actorIsEnemy
                |> List.foldr insertActor []
                |> killEnemyAndHurtPlayer actor acc

        Nothing ->
            acc


collectCoinsAtPlayer : Int -> Actors -> Actors
collectCoinsAtPlayer actorId acc =
    case getActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                    )
                |> List.filter actorIsCollectible
                |> List.foldr insertActor []
                |> consumeCoinsAndGiveToPlayer actor acc

        Nothing ->
            acc


grabWeaponsAtPlayer : Int -> Actors -> Actors
grabWeaponsAtPlayer actorId acc =
    case getActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                    )
                |> List.filter actorIsWeapon
                |> List.foldr insertActor []
                |> grabWeaponAndGiveToPlayer actor acc

        Nothing ->
            acc


grabWeaponAndGiveToPlayer : Actor -> Actors -> Actors -> Actors
grabWeaponAndGiveToPlayer player acc weaponActors =
    acc
        |> removeActorsById (getActorIds weaponActors)
        |> updatePlayerWeaponDurabilityCount player (getWeaponDurabilityFromActors weaponActors)


killEnemyAndHurtPlayer : Actor -> Actors -> Actors -> Actors
killEnemyAndHurtPlayer player acc enemyActors =
    acc
        |> removeActorsById (getActorIds enemyActors)
        |> updateHealthCount player (getHealthCountFromActors enemyActors)


consumeCoinsAndGiveToPlayer : Actor -> Actors -> Actors -> Actors
consumeCoinsAndGiveToPlayer player acc coinActors =
    acc
        |> removeActorsById (getActorIds coinActors)
        |> updateCoinCount player (getCoinCountFromActors coinActors)



-- handleKeyboardEvent : Keyboard.KeyCode -> Actors -> Actors
-- handleKeyboardEvent keycode actors =
--     List.foldr
--         (\actor acc ->
--             List.foldr
--                 (\component acc ->
--                     case component of
--                         KeyboardComponent ->
--                             updateKeyboardComponent keycode actor acc
--                                 |> collectCoinsAtPlayer actor.id
--                                 |> removeEnemiesAtPlayer actor.id
--                                 |> grabWeaponsAtPlayer actor.id
--                         _ ->
--                             acc
--                 )
--                 acc
--                 actor.components
--         )
--         actors
--         actors
-- moveFromKeyCode : Keyboard.KeyCode -> Position -> Position
-- moveFromKeyCode keycode position =
--     case Dict.get keycode validKeyCodesMap of
--         Just LeftArrow ->
--             { x = max 0 (position.x - 1), y = position.y }
--         Just RightArrow ->
--             { x = min numberColumns (position.x + 1), y = position.y }
--         Just UpArrow ->
--             { x = position.x, y = max 0 (position.y - 1) }
--         Just DownArrow ->
--             { x = position.x, y = min numberRows (position.y + 1) }
--         _ ->
--             position


actorsAt : Position -> Actors -> Actors
actorsAt position actors =
    List.filter
        (\actor ->
            getPosition actor
                |> Maybe.andThen
                    (\actorPos ->
                        Just <| actorPos == position
                    )
                |> Maybe.withDefault False
        )
        actors



-- updateKeyboardComponent : Keyboard.KeyCode -> Actor -> Actors -> Actors
-- updateKeyboardComponent keycode actor acc =
--     getPosition actor
--         |> Maybe.andThen
--             (\position ->
--                 Just <|
--                     moveFromKeyCode keycode position
--             )
--         |> Maybe.andThen
--             (\position ->
--                 let
--                     newComponents =
--                         List.filter
--                             (\c ->
--                                 isTransformComponent c |> not
--                             )
--                             actor.components
--                 in
--                 Just { actor | components = TransformComponent position :: newComponents }
--             )
--         |> Maybe.andThen
--             (\actor ->
--                 Just <|
--                     insertActor
--                         actor
--                         acc
--             )
--         |> Maybe.withDefault acc


actorIsWeapon : Actor -> Bool
actorIsWeapon actor =
    actorIs isWeaponComponent actor


actorIsCollectible : Actor -> Bool
actorIsCollectible actor =
    actorIs isCoinComponent actor


actorIsEnemy : Actor -> Bool
actorIsEnemy actor =
    actorIs isEnemyComponent actor


actorIs : (Component -> Bool) -> Actor -> Bool
actorIs isComponent actor =
    List.filter isComponent actor.components
        |> List.isEmpty
        |> not


isWeaponComponent : Component -> Bool
isWeaponComponent component =
    case component of
        ObjectTypeComponent (Weapon _) ->
            True

        _ ->
            False


isEnemyComponent : Component -> Bool
isEnemyComponent component =
    case component of
        ObjectTypeComponent (Enemy _) ->
            True

        _ ->
            False


isCoinComponent : Component -> Bool
isCoinComponent component =
    case component of
        ObjectTypeComponent (Coin _) ->
            True

        _ ->
            False


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


addCoinsToMoniesCollectedComponent : Int -> Int -> Actor -> Actor
addCoinsToMoniesCollectedComponent currentCoins additionalCoins actor =
    MoniesCollectedComponent (currentCoins + additionalCoins)
        :: actor.components
        |> setActorComponents actor


addDamageToHealthComponent : Int -> Int -> Actor -> Actor
addDamageToHealthComponent currentHealth damage actor =
    HealthComponent (currentHealth - damage)
        :: actor.components
        |> setActorComponents actor


addWeaponToWeaponComponent : Int -> Int -> Actor -> Actor
addWeaponToWeaponComponent currentDurability additionalDurability actor =
    if currentDurability > additionalDurability then
        WeaponComponent currentDurability
            :: actor.components
            |> setActorComponents actor

    else
        WeaponComponent additionalDurability
            :: actor.components
            |> setActorComponents actor


getWeaponDurabilityFromActorOrZero : Actor -> Int
getWeaponDurabilityFromActorOrZero actor =
    actor.components
        |> List.map getWeaponDurabilityFromComponentOrZero
        |> List.sum


getPlayerWeaponDurabilityFromActorOrZero : Actor -> Int
getPlayerWeaponDurabilityFromActorOrZero actor =
    actor.components
        |> List.map getPlayerWeaponDurabilityFromComponentOrZero
        |> List.sum


getMoniesFromActorOrZero : Actor -> Int
getMoniesFromActorOrZero actor =
    actor.components
        |> List.map getMoniesFromComponentOrZero
        |> List.sum


getHealthFromActorOrZero : Actor -> Int
getHealthFromActorOrZero actor =
    actor.components
        |> List.map getHealthFromComponentOrZero
        |> List.sum


removeHealthComponent : Actor -> Actor
removeHealthComponent actor =
    List.filter
        (\c ->
            case c of
                HealthComponent _ ->
                    False

                _ ->
                    True
        )
        actor.components
        |> setActorComponents actor


removeWeaponComponent : Actor -> Actor
removeWeaponComponent actor =
    List.filter
        (\c ->
            case c of
                WeaponComponent _ ->
                    False

                _ ->
                    True
        )
        actor.components
        |> setActorComponents actor


removeMoniesCollectedComponent : Actor -> Actor
removeMoniesCollectedComponent actor =
    List.filter
        (\c ->
            case c of
                MoniesCollectedComponent _ ->
                    False

                _ ->
                    True
        )
        actor.components
        |> setActorComponents actor


getActorIds : Actors -> List Int
getActorIds actors =
    List.map
        (\actor -> actor.id)
        actors


removeActorsById : List Int -> Actors -> Actors
removeActorsById actorIdsToRemove acc =
    List.filter
        (\actor ->
            List.member actor.id actorIdsToRemove
                |> not
        )
        acc


setActorComponents : Actor -> List Component -> Actor
setActorComponents actor components =
    { actor | components = components }


getMoniesFromComponentOrZero : Component -> Int
getMoniesFromComponentOrZero component =
    case component of
        MoniesCollectedComponent monies ->
            monies

        _ ->
            0


getHealthFromComponentOrZero : Component -> Int
getHealthFromComponentOrZero component =
    case component of
        HealthComponent health ->
            health

        _ ->
            0


getWeaponDurabilityFromComponentOrZero : Component -> Int
getWeaponDurabilityFromComponentOrZero component =
    case component of
        ObjectTypeComponent (Weapon durability) ->
            durability

        _ ->
            0


getPlayerWeaponDurabilityFromComponentOrZero : Component -> Int
getPlayerWeaponDurabilityFromComponentOrZero component =
    case component of
        WeaponComponent weaponDurability ->
            weaponDurability

        _ ->
            0


getMonies : Model -> Int
getMonies model =
    case getPlayerActor model.actors of
        Just actor ->
            List.foldr (+) 0 <| List.map getMoniesFromComponentOrZero actor.components

        Nothing ->
            0


getHealth : Model -> Int
getHealth model =
    case getPlayerActor model.actors of
        Just actor ->
            List.foldr (+) 0 <| List.map getHealthFromComponentOrZero actor.components

        Nothing ->
            0


getPlayerWeaponDurability : Model -> Int
getPlayerWeaponDurability model =
    case getPlayerActor model.actors of
        Just actor ->
            List.foldr (+) 0 <| List.map getPlayerWeaponDurabilityFromComponentOrZero actor.components

        Nothing ->
            0


getHealthCountFromActors : Actors -> Int
getHealthCountFromActors actors =
    List.foldr
        (\actor health ->
            if actorIsEnemy actor then
                health + getEnemyStrengthFromActorOrZero actor

            else
                health
        )
        0
        actors


getEnemyStrengthFromActorOrZero : Actor -> Int
getEnemyStrengthFromActorOrZero actor =
    actor.components
        |> List.map getEnemyStrengthFromComponentOrZero
        |> List.sum


getEnemyStrengthFromComponentOrZero : Component -> Int
getEnemyStrengthFromComponentOrZero component =
    case component of
        ObjectTypeComponent (Enemy strength) ->
            strength

        _ ->
            0


getWeaponDurabilityFromActors : Actors -> Int
getWeaponDurabilityFromActors actors =
    List.foldr
        (\actor weaponDurability ->
            if actorIsWeapon actor && getWeaponDurabilityFromActorOrZero actor > weaponDurability then
                getWeaponDurabilityFromActorOrZero actor

            else
                weaponDurability
        )
        0
        actors


getCoinCountFromActors : Actors -> Int
getCoinCountFromActors actors =
    List.foldr
        (\actor coins ->
            if actorIsCollectible actor then
                coins + getCoinValueFromActor actor

            else
                coins
        )
        0
        actors


getCoinValueFromActor : Actor -> Int
getCoinValueFromActor actor =
    actor.components
        |> List.map getCoinValueFromComponentOrZero
        |> List.sum


getCoinValueFromComponentOrZero : Component -> Int
getCoinValueFromComponentOrZero component =
    case component of
        ObjectTypeComponent (Coin value) ->
            value

        _ ->
            0


updateCoinCount : Actor -> Int -> Actors -> Actors
updateCoinCount actor coins acc =
    actor
        |> removeMoniesCollectedComponent
        |> addCoinsToMoniesCollectedComponent (getMoniesFromActorOrZero actor) coins
        |> (\a -> insertActor a acc)


updateHealthCount : Actor -> Int -> Actors -> Actors
updateHealthCount actor health acc =
    actor
        |> removeHealthComponent
        |> addDamageToHealthComponent (getHealthFromActorOrZero actor) health
        |> (\a -> insertActor a acc)


updatePlayerWeaponDurabilityCount : Actor -> Int -> Actors -> Actors
updatePlayerWeaponDurabilityCount actor weaponDurability acc =
    actor
        |> removeWeaponComponent
        |> addWeaponToWeaponComponent (getPlayerWeaponDurabilityFromActorOrZero actor) weaponDurability
        |> (\a -> insertActor a acc)


insertActor : Actor -> Actors -> Actors
insertActor actor actors =
    if hasActorId actor.id actors then
        replaceActor actor actors

    else
        actor :: actors


replaceActor : Actor -> Actors -> Actors
replaceActor actor actors =
    List.map
        (\a ->
            if a.id == actor.id then
                actor

            else
                a
        )
        actors


hasActorId : Int -> Actors -> Bool
hasActorId actorId actors =
    List.any
        (\actor -> actor.id == actorId)
        actors


view : Model -> Html Msg
view model =
    div [ id "game-view" ]
        [ List.range 0 numberRows
            |> List.map
                (\y ->
                    List.range 0 numberColumns
                        |> List.map
                            (\x ->
                                model.actors
                                    |> List.map
                                        (\a ->
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
                                        (\( _, d ) ->
                                            d.x == x && d.y == y
                                        )
                                    |> List.head
                                    |> Maybe.andThen
                                        (\( a, _ ) ->
                                            Just <|
                                                div [ id "cell-view" ]
                                                    [ div [] [ text (actorToPrintSymbol a) ]
                                                    ]
                                        )
                                    |> Maybe.withDefault
                                        (div
                                            [ id "cell-view" ]
                                            []
                                        )
                            )
                        |> div [ id "row-view" ]
                )
            |> div [ id "cells-view" ]
        , div [ id "game-info" ]
            [ div [] [ text ("Monies collected : " ++ String.fromInt (getMonies model) ++ "!") ]
            , div [] [ text ("Current health : " ++ String.fromInt (getHealth model) ++ "!") ]
            , div [] [ text ("Weapon durability : " ++ String.fromInt (getPlayerWeaponDurability model) ++ "!") ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Keyboard.downs KeyPressed
