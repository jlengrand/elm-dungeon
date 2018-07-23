module Main exposing (main)

import Html exposing (Html, div, text)
import Maybe.Extra
import Keyboard
import Dict exposing (Dict)
import Swiper
import Debug


type alias Model =
    { actors : Dict Int Actor
    , swipingState : Swiper.SwipingState
    , lastSwiped : SwipeStates
    }


type alias Health =
    Int


type alias Monies =
    Int


type alias PlayerWeaponDurability =
    Int


type alias WeaponDurability =
    Int


type alias Position =
    { x : Int
    , y : Int
    }


type ObjectTypeData
    = Player
    | Enemy
    | Coin
    | Weapon WeaponDurability


type Component
    = TransformComponent Position
    | KeyboardComponent
    | SwipeComponent
    | MoniesCollectedComponent Monies
    | HealthComponent Health
    | WeaponComponent PlayerWeaponDurability
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


type SwipeStates
    = SwipeUp
    | SwipeDown
    | SwipeLeft
    | SwipeRight
    | SwipeUnknown
    | SwipeInit


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
                    , SwipeComponent
                    , ObjectTypeComponent Player
                    , MoniesCollectedComponent 0
                    , HealthComponent startHealth
                    , WeaponComponent 0
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
            , ( 3
              , { id = 3
                , components =
                    [ TransformComponent { x = 0, y = 0 }
                    , ObjectTypeComponent Coin
                    ]
                }
              )
            , ( 4
              , { id = 4
                , components =
                    [ TransformComponent { x = 2, y = 2 }
                    , ObjectTypeComponent (Weapon 5)
                    ]
                }
              )
            , ( 5
              , { id = 5
                , components =
                    [ TransformComponent { x = 0, y = 2 }
                    , ObjectTypeComponent (Weapon 1)
                    ]
                }
              )
            ]
    , swipingState = Swiper.initialSwipingState
    , lastSwiped = SwipeInit
    }
        ! []


type Msg
    = KeyPressed Keyboard.KeyCode
    | Swiped Swiper.SwipeEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed keycode ->
            { model | actors = handleKeyboardEvent keycode model.actors } ! []

        Swiped evt ->
            let
                ( newState, swipedLeft ) =
                    Swiper.hasSwipedLeft evt model.swipingState

                ( _, swipedRight ) =
                    Swiper.hasSwipedLeft evt model.swipingState

                ( _, swipedUp ) =
                    Swiper.hasSwipedLeft evt model.swipingState

                ( _, swipedDown ) =
                    Swiper.hasSwipedLeft evt model.swipingState

                swipeType =
                    if swipedLeft then
                        SwipeLeft
                    else if swipedRight then
                        SwipeRight
                    else if swipedUp then
                        SwipeUp
                    else if swipedDown then
                        SwipeDown
                    else
                        SwipeUnknown

                _ =
                    Debug.log "Swipe Event!"
            in
                ( { actors = model.actors, swipingState = newState, lastSwiped = swipeType }, Cmd.none )


getPlayerActorFromId : Int -> Actors -> Maybe Actor
getPlayerActorFromId actorId actors =
    Dict.filter
        (\_ actor -> actor.id == actorId)
        actors
        |> Dict.toList
        |> List.head
        |> Maybe.map Tuple.second


getPlayerActor : Actors -> Maybe Actor
getPlayerActor actors =
    Dict.filter
        (\_ actor -> isActorPlayer actor)
        actors
        |> Dict.toList
        |> List.head
        |> Maybe.map Tuple.second


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
    case getPlayerActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filter isActorPlayer
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                            |> Dict.values
                    )
                |> List.filter actorIsEnemy
                -- This is transforming a (List Actor) to (Dict Int Actor) which is type alias of Actors
                |> List.foldr insertActor Dict.empty
                |> killEnemyAndHurtPlayer actor acc

        Nothing ->
            acc


collectCoinsAtPlayer : Int -> Actors -> Actors
collectCoinsAtPlayer actorId acc =
    case getPlayerActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filter isActorPlayer
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                            |> Dict.values
                    )
                |> List.filter actorIsCollectible
                -- This is transforming a (List Actor) to (Dict Int Actor) which is type alias of Actors
                |> List.foldr insertActor Dict.empty
                |> consumeCoinsAndGiveToPlayer actor acc

        Nothing ->
            acc


grabWeaponsAtPlayer : Int -> Actors -> Actors
grabWeaponsAtPlayer actorId acc =
    case getPlayerActorFromId actorId acc of
        Just actor ->
            [ actor ]
                |> List.filter isActorPlayer
                |> List.filterMap getPosition
                |> List.concatMap
                    (\position ->
                        actorsAt position acc
                            |> Dict.values
                    )
                |> List.filter actorIsWeapon
                -- This is transforming a (List Actor) to (Dict Int Actor) which is type alias of Actors
                |> List.foldr insertActor Dict.empty
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


handleKeyboardEvent : Keyboard.KeyCode -> Actors -> Actors
handleKeyboardEvent keycode actors =
    List.foldr
        (\( actorId, actor ) acc ->
            List.foldr
                (\component acc ->
                    case component of
                        KeyboardComponent ->
                            updateKeyboardComponent keycode actor acc
                                |> removeEnemiesAtPlayer actorId
                                |> collectCoinsAtPlayer actorId
                                |> grabWeaponsAtPlayer actorId

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
            { x = min numberColumns (position.x + 1), y = position.y }

        Just UpArrow ->
            { x = position.x, y = max 0 (position.y - 1) }

        Just DownArrow ->
            { x = position.x, y = min numberRows (position.y + 1) }

        _ ->
            position


actorsAt : Position -> Actors -> Actors
actorsAt position actors =
    Dict.filter
        (\_ actor ->
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
                    Just { actor | components = TransformComponent position :: newComponents }
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


actorIsWeapon : Actor -> Bool
actorIsWeapon actor =
    List.filter isWeaponComponent actor.components
        |> List.isEmpty
        |> not


actorIsCollectible : Actor -> Bool
actorIsCollectible actor =
    List.filter isCoinComponent actor.components
        |> List.isEmpty
        |> not


actorIsEnemy : Actor -> Bool
actorIsEnemy actor =
    List.filter isEnemyComponent actor.components
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
        ObjectTypeComponent Enemy ->
            True

        _ ->
            False


isCoinComponent : Component -> Bool
isCoinComponent component =
    case component of
        ObjectTypeComponent Coin ->
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
    Dict.keys actors


removeActorsById : List Int -> Actors -> Actors
removeActorsById actorIdsToRemove acc =
    Dict.filter
        (\actorId _ ->
            List.member actorId actorIdsToRemove
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
    Dict.foldr
        (\_ actor health ->
            if actorIsEnemy actor then
                health + 1
            else
                health
        )
        0
        actors


getWeaponDurabilityFromActors : Actors -> Int
getWeaponDurabilityFromActors actors =
    Dict.foldr
        (\_ actor weaponDurability ->
            if actorIsWeapon actor && getWeaponDurabilityFromActorOrZero actor > weaponDurability then
                getWeaponDurabilityFromActorOrZero actor
            else
                weaponDurability
        )
        0
        actors


getCoinCountFromActors : Actors -> Int
getCoinCountFromActors actors =
    Dict.foldr
        (\_ actor coins ->
            if actorIsCollectible actor then
                coins + 1
            else
                coins
        )
        0
        actors


updateCoinCount : Actor -> Int -> Actors -> Actors
updateCoinCount actor coins acc =
    actor
        |> removeMoniesCollectedComponent
        |> addCoinsToMoniesCollectedComponent (getMoniesFromActorOrZero actor) coins
        |> flip insertActor acc


updateHealthCount : Actor -> Int -> Actors -> Actors
updateHealthCount actor health acc =
    actor
        |> removeHealthComponent
        |> addDamageToHealthComponent (getHealthFromActorOrZero actor) health
        |> flip insertActor acc


updatePlayerWeaponDurabilityCount : Actor -> Int -> Actors -> Actors
updatePlayerWeaponDurabilityCount actor weaponDurability acc =
    actor
        |> removeWeaponComponent
        |> addWeaponToWeaponComponent (getPlayerWeaponDurabilityFromActorOrZero actor) weaponDurability
        |> flip insertActor acc


insertActor : Actor -> Actors -> Actors
insertActor actor actors =
    Dict.insert
        actor.id
        actor
        actors


view : Model -> Html Msg
view model =
    div ([] ++ Swiper.onSwipeEvents Swiped)
        [ List.range 0 numberRows
            |> List.map
                (\y ->
                    List.range 0 numberColumns
                        |> List.map
                            (\x ->
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
                                        (\( _, d ) ->
                                            d.x == x && d.y == y
                                        )
                                    |> List.head
                                    |> Maybe.andThen
                                        (\( a, _ ) ->
                                            Just <| text <| "[" ++ toString a.id ++ "]"
                                        )
                                    |> Maybe.withDefault (text "[ ]")
                            )
                        |> div []
                )
            |> div []
        , div [] [ text ("Monies : " ++ toString (getMonies model) ++ "!") ]
        , div [] [ text ("Current health : " ++ toString (getHealth model) ++ "!") ]
        , div [] [ text ("Weapon durability : " ++ toString (getPlayerWeaponDurability model) ++ "!") ]
        , div [] [ text ("Last Swipe : " ++ toString (model.lastSwiped) ++ "!") ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyPressed
