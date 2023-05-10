module Main exposing (main)

import Browser
import Html.Styled as H exposing (Html)
import Life.V2 as Life



---- MODEL ----


type alias Model =
    { game : Life.Game
    , version : String
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Life.newGame 64 64, version = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = GotLifeMsg Life.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLifeMsg submsg ->
            let
                ( game, cmd ) =
                    Life.update submsg model.game
            in
            ( { model | game = game }, cmd |> Cmd.map GotLifeMsg )



---- VIEW ----


view : Model -> Html Msg
view { game } =
    Life.view game
        |> H.map GotLifeMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions { game } =
    Life.subscriptions game
        |> Sub.map GotLifeMsg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> H.toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
