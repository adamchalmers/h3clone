module Render exposing (renderGame)

import Engine exposing (..)
import Grid as G exposing (Grid)
import Html exposing (..)
import Html.Attributes exposing (..)
import List as L


renderGame : Game -> Html m
renderGame game =
    div []
        [ renderMap game
        , renderPlayerResources game
        , renderTurn game
        ]


renderPlayerResources : Game -> Html m
renderPlayerResources g =
    div []
        [ renderAPlayer (PlayerId 0) g.players.p0
        , renderAPlayer (PlayerId 1) g.players.p1
        , renderAPlayer (PlayerId 2) g.players.p2
        ]


renderAPlayer : PlayerId -> Player -> Html m
renderAPlayer i p =
    table [ class "pure-table" ]
        [ tr []
            [ td [ rowspan 2 ] [ text <| "Player " ++ (String.fromInt <| (+) 1 <| toInt i) ]
            , td [] [ text "Gold" ]
            , td [] [ text "Gems" ]
            , td [] [ text "Wood" ]
            ]
        , tr []
            [ td [] [ text <| String.fromInt p.resources.gold ]
            , td [] [ text <| String.fromInt p.resources.gems ]
            , td [] [ text <| String.fromInt p.resources.wood ]
            ]
        ]


renderTurn : Game -> Html m
renderTurn g =
    let
        s =
            "Day " ++ String.fromInt (g.date.day + 1) ++ ", week " ++ String.fromInt (g.date.week + 1) ++ " (player " ++ (String.fromInt <| (+) 1 <| toInt <| g.players.curr) ++ "'s turn)"
    in
    p [] [ text s ]


renderMap : Game -> Html m
renderMap game =
    let
        rows =
            L.range 0 (G.height game.map.grid - 1)
                |> L.map rowFor

        rowFor h =
            L.range 0 (G.width game.map.grid - 1)
                |> L.map (tdFor h)
                |> tr []

        tdFor h w =
            renderTile (G.get ( w, h ) game.map.grid)
    in
    table [ class "pure-table", class "gamemap" ] rows


renderTile : Maybe Tile -> Html m
renderTile t =
    case t of
        Nothing ->
            td [ class "maptile" ] [ text "?" ]

        Just Grass ->
            td [ class "maptile" ] [ text "☘️" ]

        Just Mountain ->
            td [ class "maptile" ] [ text "⛰" ]

        Just (TMine m) ->
            td [ class "maptile" ]
                [ text <|
                    case m.resource of
                        Wood ->
                            "🌲"

                        Gold ->
                            "⚜️"

                        Gems ->
                            "💎"
                ]

        Just (TCity c) ->
            td [ class "maptile" ] [ text "🌆" ]
