module TypeInference.Value exposing
    ( Value
    , comment
    , create
    , fromMetadata
    , name
    , tipe
    )

import Elm.Docs
import TypeInference.Type as Type


type Value
    = Value
        { name : String
        , comment : String
        , tipe : Type.Type
        }


create : { name : String, comment : String, tipe : Type.Type } -> Value
create =
    Value


fromMetadata : Elm.Docs.Value -> Value
fromMetadata value =
    Value
        { name = value.name
        , comment = value.comment
        , tipe = Type.fromMetadataType value.tipe
        }


name : Value -> String
name (Value value) =
    value.name


comment : Value -> String
comment (Value value) =
    value.comment


tipe : Value -> Type.Type
tipe (Value value) =
    value.tipe
