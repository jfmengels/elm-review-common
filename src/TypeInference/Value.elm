module TypeInference.Value exposing
    ( Value
    , comment
    , create
    , fromMetadataUnion
    , fromMetadataValue
    , name
    , relateToModule
    , tipe
    )

import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import TypeInference.Type as Type


type Value
    = Value
        { name : String
        , documentation : String
        , tipe : Type.Type
        }


create : { name : String, documentation : String, tipe : Type.Type } -> Value
create =
    Value


relateToModule : ModuleName -> Value -> Value
relateToModule moduleName (Value value) =
    Value { value | tipe = Type.relateToModule moduleName value.tipe }


fromMetadataValue : Elm.Docs.Value -> Value
fromMetadataValue value =
    Value
        { name = value.name
        , documentation = value.comment
        , tipe = Type.fromMetadataType value.tipe
        }


fromMetadataUnion : ModuleName -> Elm.Docs.Union -> List Value
fromMetadataUnion moduleName value =
    List.map
        (\( name_, types ) ->
            Value
                { name = name_
                , documentation = value.comment
                , tipe =
                    List.foldl
                        (\input output -> Type.Function (Type.fromMetadataType input) output)
                        (Type.Type moduleName value.name (List.map Type.Generic value.args))
                        types
                }
        )
        value.tags


name : Value -> String
name (Value value) =
    value.name


comment : Value -> String
comment (Value value) =
    value.documentation


tipe : Value -> Type.Type
tipe (Value value) =
    value.tipe
