module Main exposing (main)

import Browser
import Css exposing (backgroundColor, center, color, em, fontFamilies, height, hex, property, textAlign, width)
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, table, td, text, tr)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra exposing (combine)
import Time exposing (Month(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cells : Dict ( Int, Int ) Int
    , width : Int
    , height : Int
    }


initialGrid : List (List Int)
initialGrid =
    [ [ 5, 4, 8, 3, 1, 4, 3, 2, 2, 3 ]
    , [ 2, 7, 4, 5, 8, 5, 4, 7, 1, 1 ]
    , [ 5, 2, 6, 4, 5, 5, 6, 1, 7, 3 ]
    , [ 6, 1, 4, 1, 3, 3, 6, 1, 4, 6 ]
    , [ 6, 3, 5, 7, 3, 8, 5, 4, 7, 8 ]
    , [ 4, 1, 6, 7, 5, 2, 4, 6, 4, 5 ]
    , [ 2, 1, 7, 6, 8, 4, 1, 7, 2, 1 ]
    , [ 6, 8, 8, 2, 8, 8, 1, 1, 3, 4 ]
    , [ 4, 8, 4, 6, 8, 4, 8, 5, 5, 4 ]
    , [ 5, 2, 8, 3, 7, 5, 1, 5, 2, 6 ]
    ]


init : a -> ( Model, Cmd Msg )
init _ =
    let
        assocs =
            List.indexedMap (\row line -> List.indexedMap (\col value -> ( ( row, col ), value )) line) initialGrid |> List.concat
    in
    ( { cells = Dict.fromList assocs
      , width = 10
      , height = 10
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


increment : Model -> Model
increment model =
    { model | cells = Dict.map (\_ n -> n + 1) model.cells }


flashes : Model -> Model
flashes model =
    let
        go grid row col =
            if col == grid.width && row == grid.height then
                grid

            else if col == grid.width then
                go grid (row + 1) 0

            else if Dict.get ( row, col ) grid.cells |> Maybe.map (\v -> v > 9) |> Maybe.withDefault False then
                let
                    updated =
                        flash grid row col
                in
                go updated row (col + 1)

            else
                go grid row (col + 1)
    in
    go model 0 0


flash : Model -> Int -> Int -> Model
flash grid row col =
    let
        updated =
            Dict.insert ( row, col ) 0 grid.cells
    in
    updateNeighbours { grid | cells = updated } row col


updateNeighbours : Model -> Int -> Int -> Model
updateNeighbours model row col =
    List.foldl
        (\( nr, nc ) grid ->
            let
                value =
                    -- The neighbours are guaranteed to be in the cells, so the withDefault here is only to satisfy the compiler
                    Dict.get ( nr, nc ) grid.cells |> Maybe.withDefault 0
            in
            if value == 9 then
                flash grid nr nc

            else if value /= 0 then
                { grid | cells = Dict.insert ( nr, nc ) (value + 1) grid.cells }

            else
                grid
        )
        model
        (neighbours model row col)


neighbours : Model -> Int -> Int -> List ( Int, Int )
neighbours model row col =
    List.map (\( r, c ) -> Dict.get ( r, c ) model.cells |> Maybe.map (\_ -> ( r, c )))
        [ ( row - 1, col - 1 )
        , ( row - 1, col )
        , ( row - 1, col + 1 )
        , ( row, col - 1 )
        , ( row, col + 1 )
        , ( row + 1, col - 1 )
        , ( row + 1, col )
        , ( row + 1, col + 1 )
        ]
        |> Maybe.Extra.values


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    let
        result =
            increment model |> flashes
    in
    ( result, Cmd.none )


view : Model -> Html Msg
view model =
    List.range 0 9
        |> List.map
            (\row ->
                List.range 0 9
                    |> List.map
                        (\col ->
                            Dict.get ( row, col ) model.cells
                                |> Maybe.map
                                    (\n ->
                                        td [ css (styleForValue n ++ [ width (em 1), height (em 1) ]) ] [ text (String.fromInt n) ]
                                    )
                        )
                    |> combine
                    |> Maybe.map (tr [])
            )
        |> combine
        |> Maybe.map
            (table
                [ css
                    [ fontFamilies [ "Arial", "sans" ]
                    , textAlign center
                    , property "font-size" "min(8vh, 8vw)"
                    ]
                ]
            )
        |> Maybe.withDefault (div [] [ text "Error" ])


styleForValue n =
    case n of
        0 ->
            [ backgroundColor (hex "000004"), color (hex "de4968") ]

        1 ->
            [ backgroundColor (hex "140e36"), color (hex "f66e5c") ]

        2 ->
            [ backgroundColor (hex "3b0f70"), color (hex "fe9f6d") ]

        3 ->
            [ backgroundColor (hex "641a80"), color (hex "fecf92") ]

        4 ->
            [ backgroundColor (hex "8c2981"), color (hex "000004") ]

        5 ->
            [ backgroundColor (hex "b5367a"), color (hex "140e36") ]

        6 ->
            [ backgroundColor (hex "de4968"), color (hex "3b0f70") ]

        7 ->
            [ backgroundColor (hex "f66e5c"), color (hex "641a80") ]

        8 ->
            [ backgroundColor (hex "fe9f6d"), color (hex "8c2981") ]

        9 ->
            [ backgroundColor (hex "fecf92"), color (hex "b5367a") ]

        _ ->
            []
