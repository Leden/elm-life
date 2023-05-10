module Life.V1 exposing (Game, Msg, newGame, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events exposing (onAnimationFrame)
import Css exposing (hex, px)
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed exposing (node)
import Set exposing (Set)
import Time



---- MODEL ----


type alias Game =
    { isPaused : Bool
    , world : World
    }


type alias World =
    { alive : Set Pos
    , width : Int
    , height : Int
    }


type alias Pos =
    ( Int, Int )


newWorld : Int -> Int -> World
newWorld w h =
    { alive = Set.empty, width = w, height = h }


newGame : Int -> Int -> Game
newGame w h =
    { isPaused = True, world = newWorld w h }



---- UPDATE ----


type Msg
    = ToggleCell Pos
    | Tick Time.Posix
    | TogglePause


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | isPaused = not model.isPaused }
            , Cmd.none
            )

        ToggleCell pos ->
            ( { model | world = toggleCell pos model.world }
            , Cmd.none
            )

        Tick _ ->
            ( { model | world = evolve model.world }
            , Cmd.none
            )


mapAll : (Pos -> Bool -> a) -> World -> List (List a)
mapAll fn { alive, width, height } =
    List.map
        (\i -> List.map (\j -> fn ( i, j ) (Set.member ( i, j ) alive)) (List.range 0 (width - 1)))
        (List.range 0 (height - 1))


getEvolvable : World -> Set Pos
getEvolvable world =
    world.alive
        |> setFlatMap (neighbours world)
        |> Set.union world.alive


dictFlatMap : (comparable1 -> a -> Dict comparable2 b) -> Dict comparable1 a -> Dict comparable2 b
dictFlatMap fn =
    Dict.foldl (\k v -> fn k v |> Dict.union) Dict.empty


setFlatMap : (comparable1 -> Set comparable2) -> Set comparable1 -> Set comparable2
setFlatMap fn =
    Set.foldl (fn >> Set.union) Set.empty


dictFilterMap : (comparable1 -> a -> Maybe ( comparable2, b )) -> Dict comparable1 a -> Dict comparable2 b
dictFilterMap fn =
    Dict.foldl
        (\k v acc ->
            fn k v
                |> Maybe.map (\( k1, v1 ) -> Dict.insert k1 v1 acc)
                |> Maybe.withDefault acc
        )
        Dict.empty


setFilterMap : (comparable1 -> Maybe comparable2) -> Set comparable1 -> Set comparable2
setFilterMap fn =
    Set.foldl
        (\k acc ->
            fn k
                |> Maybe.map (\k1 -> Set.insert k1 acc)
                |> Maybe.withDefault acc
        )
        Set.empty


toggleCell : Pos -> World -> World
toggleCell pos world =
    if Set.member pos world.alive then
        { world | alive = Set.remove pos world.alive }

    else
        { world | alive = Set.insert pos world.alive }


evolve : World -> World
evolve world =
    -- map over all world.aliveNeighbors
    -- determine (pos: Pos, isAlive: Bool, neighborsCount: Int)
    -- partition by `evolvePos` into two groups: toLive, toDie
    -- union currently alive with toLive
    -- subtract toDie from the prev result
    -- the result is the next world alive state
    { world
        | alive =
            world
                |> getEvolvable
                |> setFilterMap (evolvePos world)
    }


evolvePos : World -> Pos -> Maybe Pos
evolvePos world pos =
    -- alive with <2 neighbors dies
    -- alive with 2 or 3 neighbors stays alive
    -- alive with >3 neighbors dies
    -- dead with 3 neighbors lives
    case ( Set.member pos world.alive, aliveNeighbours world pos ) of
        ( True, 2 ) ->
            Just pos

        ( True, 3 ) ->
            Just pos

        ( True, _ ) ->
            Nothing

        ( False, 3 ) ->
            Just pos

        ( False, _ ) ->
            Nothing


aliveNeighbours : World -> Pos -> Int
aliveNeighbours world =
    neighbours world >> Set.intersect world.alive >> Set.size


neighbours : World -> Pos -> Set Pos
neighbours { width, height } ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]
        |> List.map (Tuple.mapBoth (modBy width) (modBy height))
        |> Set.fromList



---- VIEW ----


view : Game -> Html Msg
view model =
    H.div []
        [ H.div []
            (model.world
                |> mapAll cell
                |> List.map (node "div" [ css [ Css.displayFlex ] ])
            )
        , H.button [ onClick TogglePause ]
            [ H.text
                (if model.isPaused then
                    "Unpause"

                 else
                    "Pause"
                )
            ]
        ]


cell : Pos -> Bool -> ( String, Html Msg )
cell pos alive =
    H.div
        [ onClick (ToggleCell pos)
        , css
            [ Css.width (px 10)
            , Css.height (px 10)
            , Css.border3 (px 1) Css.solid (hex "ccc")
            , Css.borderRadius (px 5)
            , Css.backgroundColor
                (if alive then
                    getColor pos

                 else
                    hex "fff"
                )
            ]
        ]
        [ H.text "" ]
        |> withKey pos


withKey : Pos -> Html Msg -> ( String, Html Msg )
withKey ( x, y ) html =
    ( [ x, y ] |> List.map String.fromInt |> String.join ":"
    , html
    )


colors : Array Css.Color
colors =
    [ "95D5B2"
    , "74C69D"
    , "52B788"
    , "40916C"
    , "40916C"
    , "52B788"
    , "74C69D"
    , "95D5B2"
    ]
        |> List.map hex
        |> Array.fromList


getColor : Pos -> Css.Color
getColor ( x, y ) =
    colors
        |> Array.get (modBy (Array.length colors) (x * y))
        |> Maybe.withDefault (hex "ccc")



---- SUBSCRIPTIONS ----


subscriptions : Game -> Sub Msg
subscriptions { isPaused } =
    if not isPaused then
        onAnimationFrame Tick

    else
        Sub.none
