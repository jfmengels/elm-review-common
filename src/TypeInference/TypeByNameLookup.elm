module TypeInference.TypeByNameLookup exposing
    ( TypeByNameLookup
    , addNewScope
    , addToTypeByNameLookup
    , emptyTypeByNameLookup
    , lookupTypeByName
    , popScope
    )

import Dict exposing (Dict)
import Elm.Type


type TypeByNameLookup
    = TypeByNameLookup (Dict String Elm.Type.Type) (List (Dict String Elm.Type.Type))


emptyTypeByNameLookup : TypeByNameLookup
emptyTypeByNameLookup =
    TypeByNameLookup Dict.empty []


addToTypeByNameLookup : List ( String, Elm.Type.Type ) -> TypeByNameLookup -> TypeByNameLookup
addToTypeByNameLookup types (TypeByNameLookup lookup higherLevelLookups) =
    TypeByNameLookup
        (Dict.union
            (Dict.fromList types)
            lookup
        )
        higherLevelLookups


addNewScope : TypeByNameLookup -> TypeByNameLookup
addNewScope (TypeByNameLookup lookup higherLevelLookups) =
    TypeByNameLookup
        Dict.empty
        (lookup :: higherLevelLookups)


popScope : TypeByNameLookup -> TypeByNameLookup
popScope ((TypeByNameLookup _ higherLevelLookups) as originalLookupTable) =
    case higherLevelLookups of
        head :: rest ->
            TypeByNameLookup head rest

        [] ->
            originalLookupTable


lookupTypeByName : TypeByNameLookup -> String -> Maybe Elm.Type.Type
lookupTypeByName (TypeByNameLookup lookup higherLevelLookups) name =
    lookupTypeByNameInternal name (lookup :: higherLevelLookups)


lookupTypeByNameInternal : String -> List (Dict String Elm.Type.Type) -> Maybe Elm.Type.Type
lookupTypeByNameInternal name lookupTables =
    case lookupTables of
        [] ->
            Nothing

        lookupTable :: restOfLookupTables ->
            case Dict.get name lookupTable of
                Just type_ ->
                    Just type_

                Nothing ->
                    lookupTypeByNameInternal name restOfLookupTables
