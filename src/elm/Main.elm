module Main exposing (main)

import Html exposing (Html)


type alias Model =
    {}


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
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "Plop"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
