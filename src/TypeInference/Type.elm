module TypeInference.Type exposing (Type(..), fromMetadataType)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type


type Type
    = Unknown
    | Var String
    | Function Type Type
    | Tuple (List Type)
    | Type ModuleName String (List Type)
    | Record
        { fields : List ( String, Type )
        , generic : Maybe String
        , canHaveMoreFields : Bool
        }


fromMetadataType : Elm.Type.Type -> Type
fromMetadataType type_ =
    case type_ of
        Elm.Type.Var string ->
            Var string

        Elm.Type.Lambda input output ->
            Function (fromMetadataType input) (fromMetadataType output)

        Elm.Type.Tuple types ->
            Tuple (List.map fromMetadataType types)

        Elm.Type.Type name originalTypes ->
            let
                types : List Type
                types =
                    List.map fromMetadataType originalTypes
            in
            case String.split "." name |> List.reverse of
                [] ->
                    Type [] name types

                functionName :: reversedModuleName ->
                    Type (List.reverse reversedModuleName) functionName types

        Elm.Type.Record originalFields generic ->
            Record
                { fields = List.map (Tuple.mapSecond fromMetadataType) originalFields
                , generic = generic
                , canHaveMoreFields = False
                }
