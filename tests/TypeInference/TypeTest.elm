module TypeInference.TypeTest exposing (suite)

import Elm.Type
import Expect
import Test exposing (Test, test)
import TypeInference.Type as Type


suite : Test
suite =
    Test.describe "TypeInference.Type.fromMetadataType"
        [ Test.test "generic variable" <|
            \() ->
                Elm.Type.Var "a"
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Generic "a")
        , Test.test "Function" <|
            \() ->
                Elm.Type.Lambda (Elm.Type.Var "a") (Elm.Type.Var "b")
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Function (Type.Generic "a") (Type.Generic "b"))
        , Test.test "Function with multiple arguments" <|
            \() ->
                Elm.Type.Lambda
                    (Elm.Type.Var "a")
                    (Elm.Type.Lambda (Elm.Type.Var "b") (Elm.Type.Var "c"))
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Function
                            (Type.Generic "a")
                            (Type.Function (Type.Generic "b") (Type.Generic "c"))
                        )
        , Test.test "Unit" <|
            \() ->
                Elm.Type.Tuple []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [])
        , Test.test "Tuple" <|
            \() ->
                Elm.Type.Tuple [ Elm.Type.Var "b", Elm.Type.Var "c" ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [ Type.Generic "b", Type.Generic "c" ])
        , Test.test "Tuple with 3 elements" <|
            \() ->
                Elm.Type.Tuple [ Elm.Type.Var "b", Elm.Type.Var "c", Elm.Type.Var "d" ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Tuple [ Type.Generic "b", Type.Generic "c", Type.Generic "d" ])
        , Test.test "Type with an empty module name" <|
            \() ->
                Elm.Type.Type "Int" []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [] "Int" [])
        , Test.test "Type with a module name" <|
            \() ->
                Elm.Type.Type "Basics.Some.Thing.Int" []
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [ "Basics", "Some", "Thing" ] "Int" [])
        , Test.test "Type with an argument" <|
            \() ->
                Elm.Type.Type "Basics.List" [ Elm.Type.Type "Basics.Int" [] ]
                    |> Type.fromMetadataType
                    |> Expect.equal (Type.Type [ "Basics" ] "List" [ Type.Type [ "Basics" ] "Int" [] ])
        , Test.test "Empty record" <|
            \() ->
                Elm.Type.Record [] Nothing
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Nothing
                            , fields = []
                            , canHaveMoreFields = False
                            }
                        )
        , Test.test "Record with a field" <|
            \() ->
                Elm.Type.Record [ ( "name", Elm.Type.Type "Basics.String" [] ) ] Nothing
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Nothing
                            , fields = [ ( "name", Type.Type [ "Basics" ] "String" [] ) ]
                            , canHaveMoreFields = False
                            }
                        )
        , Test.test "Record with a generic field" <|
            \() ->
                Elm.Type.Record [ ( "name", Elm.Type.Type "Basics.String" [] ) ] (Just "a")
                    |> Type.fromMetadataType
                    |> Expect.equal
                        (Type.Record
                            { generic = Just "a"
                            , fields = [ ( "name", Type.Type [ "Basics" ] "String" [] ) ]
                            , canHaveMoreFields = False
                            }
                        )
        ]
