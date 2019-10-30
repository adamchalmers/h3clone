module Engine exposing (City, CityState, Date, Game, Hero, Map, Mine, Player, PlayerId(..), Players, Point, Resource(..), Resources, Tile(..), add, dailyResources, finishTurn, getCities, getMines, isFirstPlayer, nextDate, nextPlayer, toInt, updateCurrentPlayer, zeroDate, zeroPlayer, zeroPlayers, zeroResources)

import Grid as G exposing (Grid)
import List as L


type Resource
    = Wood
    | Gems
    | Gold



-- Resources is an additive monoid


type alias Resources =
    { gold : Int
    , gems : Int
    , wood : Int
    }


zeroResources : Resources
zeroResources =
    { gold = 0, gems = 0, wood = 0 }


add : Resources -> Resources -> Resources
add r1 r2 =
    { gold = r1.gold + r2.gold
    , gems = r1.gems + r2.gems
    , wood = r1.wood + r2.wood
    }


type PlayerId
    = PlayerId Int


toInt : PlayerId -> Int
toInt p =
    case p of
        PlayerId i ->
            i


type alias Player =
    { resources : Resources
    , heroes : List Hero
    }


zeroPlayer : Player
zeroPlayer =
    { resources = zeroResources, heroes = [] }


type alias Date =
    { week : Int
    , day : Int
    }


zeroDate : Date
zeroDate =
    { day = 0, week = 0 }


type alias Game =
    { players : Players
    , map : Map
    , date : Date
    }


type alias Players =
    { p0 : Player
    , p1 : Player
    , p2 : Player
    , curr : PlayerId
    }


zeroPlayers : Players
zeroPlayers =
    { p0 = zeroPlayer
    , p1 = zeroPlayer
    , p2 = zeroPlayer
    , curr = PlayerId 0
    }


isFirstPlayer : Players -> Bool
isFirstPlayer p =
    toInt p.curr == 0


nextPlayer : Players -> Players
nextPlayer p =
    { p | curr = PlayerId <| modBy 3 (toInt p.curr + 1) }


updateCurrentPlayer : (Player -> Player) -> Players -> Players
updateCurrentPlayer f p =
    case toInt p.curr of
        0 ->
            { p | p0 = f p.p0 }

        1 ->
            { p | p1 = f p.p1 }

        2 ->
            { p | p2 = f p.p2 }

        _ ->
            Debug.todo <| String.fromInt (toInt p.curr) ++ " isn't a real player"


nextDate : Date -> Date
nextDate d =
    if d.day == 6 then
        { week = d.week + 1, day = 0 }

    else
        { week = d.week, day = d.day + 1 }


finishTurn : Game -> Game
finishTurn g =
    let
        incPlayer =
            nextPlayer g.players

        date =
            g.date
                |> (if isFirstPlayer incPlayer then
                        nextDate

                    else
                        Basics.identity
                   )

        resources =
            dailyResources g incPlayer.curr

        players =
            updateCurrentPlayer (\pl -> { pl | resources = add pl.resources resources }) incPlayer
    in
    { g | players = players, date = date }


dailyResources : Game -> PlayerId -> Resources
dailyResources g player =
    let
        townGold =
            if g.date.day == 0 then
                g.map
                    |> getCities
                    |> L.filter (\city -> city.owner == player)
                    |> L.map (\city -> city.state.goldPerWeek)
                    |> L.sum

            else
                0

        fromMines : Resource -> Int
        fromMines resource =
            g.map
                |> getMines
                |> L.filter (\mine -> mine.owner == player)
                |> L.filter (\mine -> mine.resource == resource)
                |> L.map (\mine -> mine.dailyOutput)
                |> L.sum
    in
    { gold = townGold + fromMines Gold
    , gems = fromMines Gems
    , wood = fromMines Wood
    }


type alias Map =
    { width : Int
    , height : Int
    , grid : Grid Tile
    }


type Tile
    = TMine Mine
    | TCity City
    | Grass
    | Mountain


getMines : Map -> List Mine
getMines m =
    let
        f tile partial =
            case tile of
                TMine x ->
                    x :: partial

                _ ->
                    partial
    in
    G.foldl f [] m.grid


getCities : Map -> List City
getCities m =
    let
        f tile partial =
            case tile of
                TCity x ->
                    x :: partial

                _ ->
                    partial
    in
    G.foldl f [] m.grid


type alias Point =
    { x : Int, y : Int }


type alias City =
    { name : String, state : CityState, owner : PlayerId }


type alias Mine =
    { resource : Resource, dailyOutput : Int, owner : PlayerId }


type alias CityState =
    { goldPerWeek : Int }


type alias Hero =
    { mapPoint : Point, name : String }
