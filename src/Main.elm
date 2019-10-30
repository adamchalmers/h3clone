port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Dict as D exposing (Dict)
import Engine exposing (..)
import Grid as G exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode
import List as L
import Maybe as M
import Render exposing (..)



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { game : Game
    , serverMessage : String
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { serverMessage = "", game = initialGame }, Cmd.none )


initialGame =
    { players = zeroPlayers
    , map = exampleMap
    , date = zeroDate
    }


exampleMap : Map
exampleMap =
    let
        mine1 =
            TMine { resource = Wood, dailyOutput = 3, owner = PlayerId 0 }

        mine2 =
            TMine { resource = Gems, dailyOutput = 3, owner = PlayerId 1 }

        mine3 =
            TMine { resource = Gold, dailyOutput = 3, owner = PlayerId 2 }

        exampleGridMaybe =
            G.fromList
                [ [ Grass, Grass, Grass, Grass, Mountain, Grass, Grass, Grass, Grass, Grass ]
                , [ mine1, mine2, mine3, Grass, Mountain, Grass, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Grass, Grass, Mountain, Mountain, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Grass, Grass, Grass, Mountain, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Mountain, Grass, Grass, Grass, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Mountain, Mountain, Grass, Grass, Grass, Grass, Grass, Grass ]
                , [ Mountain, Mountain, Mountain, Grass, Grass, Grass, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Mountain, Grass, Grass, Grass, Grass, Grass, Grass, Grass ]
                , [ Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass, Grass ]
                ]

        exampleGrid =
            case exampleGridMaybe of
                Just g ->
                    g

                Nothing ->
                    Debug.todo "ill-formed grid"
    in
    { width = 10
    , height = 10
    , grid = exampleGrid
    }



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = EndTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        EndTurn ->
            ( { model | game = finishTurn model.game }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl _ ->
            "BadUrl"

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header []
            [ -- img [ src "/images/logo.png" ] []
              span [ class "logo" ] []
            , h1 [] [ text "HoMM3 clone in Elm" ]
            ]
        , renderGame model.game
        , nextTurnBtn
        ]


nextTurnBtn =
    button
        [ class "pure-button pure-button-primary"
        , onClick EndTurn
        ]
        [ text "Next turn" ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
