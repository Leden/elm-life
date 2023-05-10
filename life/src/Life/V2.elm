module Life.V2 exposing (Game, Msg, newGame, subscriptions, update, view)

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


newGame : Int -> Int -> Game
newGame w h =
    { isPaused = True, world = newWorld w h }


type alias World =
    { width : Int
    , height : Int
    , alive : Set Pos
    , aliveAdj : Dict Pos Int
    }


newWorld : Int -> Int -> World
newWorld w h =
    { width = w, height = h, alive = Set.empty, aliveAdj = Dict.empty }


type alias Pos =
    ( Int, Int )



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
            ( { model
                | world = evolve model.world
              }
            , Cmd.none
            )


toggleCell : Pos -> World -> World
toggleCell pos world =
    if isAlive pos world then
        makeDead pos world

    else
        makeAlive pos world


evolve : World -> World
evolve world =
    Dict.foldl
        (\pos rank wrld ->
            let
                nowAlive =
                    isAlive pos wrld
            in
            case ( nowAlive, shouldLive nowAlive rank ) of
                ( False, True ) ->
                    makeAlive pos wrld

                ( True, False ) ->
                    makeDead pos wrld

                _ ->
                    wrld
        )
        world
        world.aliveAdj


shouldLive : Bool -> Int -> Bool
shouldLive alive rank =
    case ( alive, rank ) of
        ( True, 2 ) ->
            True

        ( _, 3 ) ->
            True

        ( _, _ ) ->
            False


isAlive : Pos -> World -> Bool
isAlive pos =
    .alive >> Set.member pos


makeDead : Pos -> World -> World
makeDead pos world =
    { world
        | alive = world.alive |> Set.remove pos
        , aliveAdj = world.aliveAdj |> updateRegion (adjacentTo world pos) (flipApp2 (-) 1)
    }


makeAlive : Pos -> World -> World
makeAlive pos world =
    { world
        | alive = world.alive |> Set.insert pos
        , aliveAdj = world.aliveAdj |> updateRegion (adjacentTo world pos) ((+) 1)
    }


flipApp2 : (a -> b -> c) -> b -> a -> c
flipApp2 fn b a =
    fn a b


updateRegion : List Pos -> (Int -> Int) -> Dict Pos Int -> Dict Pos Int
updateRegion region fn dict =
    List.foldl (\pos -> updateWithDefault pos (fn >> Just) 0) dict region


updateWithDefault :
    comparable
    -> (v -> Maybe v)
    -> v
    -> Dict comparable v
    -> Dict comparable v
updateWithDefault key fn default dict =
    Dict.update key (Maybe.withDefault default >> fn) dict


getWithDefault : comparable -> v -> Dict comparable v -> v
getWithDefault key default =
    Dict.get key >> Maybe.withDefault default


adjacentTo : World -> Pos -> List Pos
adjacentTo world ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]
        |> List.map (wrap world)


wrap : World -> Pos -> Pos
wrap { width, height } =
    Tuple.mapBoth (modBy width) (modBy height)


gridMap : (Pos -> Bool -> Int -> a) -> World -> List (List a)
gridMap fn { alive, aliveAdj, width, height } =
    List.map
        (\i -> List.map (\j -> fn ( i, j ) (Set.member ( i, j ) alive) (getWithDefault ( i, j ) 0 aliveAdj)) (List.range 0 (width - 1)))
        (List.range 0 (height - 1))



---- SUBSCRIPTIONS ----


subscriptions : Game -> Sub Msg
subscriptions { isPaused } =
    if not isPaused then
        onAnimationFrame Tick

    else
        Sub.none



---- VIEW ----


view : Game -> Html Msg
view model =
    H.div []
        [ H.div []
            (model.world
                |> gridMap cell
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


cell : Pos -> Bool -> Int -> ( String, Html Msg )
cell pos alive aliveAdj =
    H.div
        [ onClick (ToggleCell pos)
        , css
            [ Css.width (px 10)
            , Css.height (px 10)
            , Css.border3 (px 1) Css.solid (hex "ccc")
            , Css.borderRadius (px 5)
            , Css.backgroundColor
                (if alive then
                    getColor aliveAdj

                 else
                    hex "fff"
                )
            ]
        ]
        --[ aliveAdj |> String.fromInt |> H.text ]
        [ H.text "" ]
        |> withKey pos


withKey : Pos -> Html Msg -> ( String, Html Msg )
withKey ( x, y ) html =
    ( [ x, y ] |> List.map String.fromInt |> String.join ":"
    , html
    )


colors : Array Css.Color
colors =
    --[ "95D5B2" , "74C69D" , "52B788" , "40916C" ]
    [ "FFBA08"
    , "FAA307"
    , "F48C06"
    , "E85D04"
    , "DC2F02"
    , "D00000"
    , "9D0208"
    , "6A040F"
    , "370617"
    ]
        |> List.map hex
        |> Array.fromList


getColor : Int -> Css.Color
getColor aliveAdj =
    colors
        |> Array.get (clamp 0 (Array.length colors - 1) aliveAdj)
        |> Maybe.withDefault (hex "ccc")
