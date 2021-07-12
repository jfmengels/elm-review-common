module RangeDict exposing (RangeDict, empty, fromList, get, insert, isEmpty, member, modify, toList, values)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type alias RangeDict v =
    Dict String v


empty : RangeDict v
empty =
    Dict.empty


isEmpty : RangeDict v -> Bool
isEmpty =
    Dict.isEmpty


insert : Range -> v -> RangeDict v -> RangeDict v
insert range =
    Dict.insert (rangeAsString range)


modify : Range -> (v -> v) -> RangeDict v -> RangeDict v
modify range mapper dict =
    let
        key : String
        key =
            rangeAsString range
    in
    case Dict.get key dict of
        Just value ->
            Dict.insert (rangeAsString range) (mapper value) dict

        Nothing ->
            dict


fromList : List ( Range, v ) -> RangeDict v
fromList entries =
    entries
        |> List.map (Tuple.mapFirst rangeAsString)
        |> Dict.fromList


toList : RangeDict v -> List ( Range, v )
toList rangeDict =
    rangeDict
        |> Dict.toList
        |> List.filterMap
            (\( key, v ) ->
                case undoRange key of
                    Just range ->
                        Just ( range, v )

                    Nothing ->
                        Nothing
            )


values : RangeDict v -> List v
values rangeDict =
    Dict.values rangeDict


get : Range -> RangeDict v -> Maybe v
get range =
    Dict.get (rangeAsString range)


member : Range -> RangeDict v -> Bool
member range =
    Dict.member (rangeAsString range)


rangeAsString : Range -> String
rangeAsString range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "_"


undoRange : String -> Maybe Range
undoRange key =
    case String.split "_" key |> List.filterMap String.toInt of
        [ startRow, startColumn, endRow, endColumn ] ->
            Just
                { start = { row = startRow, column = startColumn }
                , end = { row = endRow, column = endColumn }
                }

        _ ->
            Nothing
