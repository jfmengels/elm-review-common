module NoMissingTypeAnnotationInLetInTest exposing (all)

import Dependencies.ElmCore
import NoMissingTypeAnnotationInLetIn exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
    ]


all : Test
all =
    describe "NoMissingTypeAnnotationInLetIn"
        [ reportTests
        , fixTests
        ]


reportTests : Test
reportTests =
    describe "Reports"
        [ test "should not report anything for top-level declarations even if they have no type annotation" <|
            \_ ->
                """module A exposing (..)
hasTypeAnnotation : Int
hasTypeAnnotation = 1

hasNoTypeAnnotation = doSomething
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything when all let in declarations have a type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      b : number
      b = 1

      c : number
      c = 1
    in
    b + c
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a let in declaration has no type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      hasNoTypeAnnotation_1 = foo
      hasNoTypeAnnotation_2 = foo
    in
    d
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_1`"
                            , details = details
                            , under = "hasNoTypeAnnotation_1"
                            }
                        , Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_2`"
                            , details = details
                            , under = "hasNoTypeAnnotation_2"
                            }
                        ]
        , test "should not report anything for let..in destructuring" <|
            \_ ->
                """module A exposing (..)
a = let
      (b, c) = foo
      {e, f} = foo
      (Thing thing) = foo
    in
    d
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        ]


fixTests : Test
fixTests =
    describe "Fixing"
        [ fixTest "when value is a literal string"
            { arguments = ""
            , value = "\"abc\""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal char"
            { arguments = ""
            , value = "'c'"
            , expectedType = "Char"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal integer"
            { arguments = ""
            , value = "1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal hex"
            { arguments = ""
            , value = "0x12"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal float"
            { arguments = ""
            , value = "1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal unit"
            { arguments = ""
            , value = "()"
            , expectedType = "()"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a negation of a number"
            { arguments = ""
            , value = "-1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a negation of a Float"
            { arguments = ""
            , value = "-1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is a negation of something unknown"
            { arguments = ""
            , value = "-someThing"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `True`"
            { arguments = ""
            , value = "False"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `False`"
            { arguments = ""
            , value = "False"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value equals a top-level value: String"
            { arguments = ""
            , value = "someValue"
            , expectedType = "String"
            , topLevelDeclarations = """someValue : String
someValue = "abc\""""
            }
        , fixTest "when value equals a top-level value: Int"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : Int
someValue = 1"""
            }
        , fixTest "when value equals a top-level value: Float"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Float"
            , topLevelDeclarations = """someValue : Float
someValue = 1.0"""
            }
        , fixTest "when value equals a top-level value: unit"
            { arguments = ""
            , value = "someValue"
            , expectedType = "()"
            , topLevelDeclarations = """someValue : ()
someValue = ()"""
            }
        , fixTest "when value equals a top-level value: typeclass"
            { arguments = ""
            , value = "someValue"
            , expectedType = "number"
            , topLevelDeclarations = """someValue : number
someValue = 1"""
            }
        , fixTest "when value equals a top-level value: empty record"
            { arguments = ""
            , value = "someValue"
            , expectedType = "{}"
            , topLevelDeclarations = """someValue : { }
someValue = { a = 1 }"""
            }
        , fixTest "when value equals a top-level value: record"
            { arguments = ""
            , value = "someValue"
            , expectedType = "{ a : Int, b : String }"
            , topLevelDeclarations = """someValue : { a : Int, b:String }
someValue = someThing"""
            }
        , fixTest "when value equals a generic type"
            { arguments = ""
            , value = "someValue"
            , expectedType = "genericType"
            , topLevelDeclarations = """someValue : genericType
someValue = 1"""
            }
        , fixTest "when value equals a function"
            { arguments = ""
            , value = "someValue"
            , expectedType = "String -> Int"
            , topLevelDeclarations = """someValue : String -> Int
someValue = String.length"""
            }
        , fixTest "when value equals a function (multiple arguments)"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Thing -> String -> Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function"
            { arguments = ""
            , value = "someValue thing"
            , expectedType = "String -> Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function (multiple arguments)"
            { arguments = ""
            , value = "someValue thing string"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a call to known top-level function with a function as argument"
            { arguments = ""
            , value = "someValue thing string"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : (Thing -> Thing) -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function where parens are needed for the type variable"
            { arguments = ""
            , value = "someValue thing"
            , expectedType = "List (Attribute msg)"
            , topLevelDeclarations = """someValue : String -> List (Attribute msg)
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function where parens are needed for the underlying function"
            { arguments = ""
            , value = "someValue string"
            , expectedType = "(Thing -> Thing) -> Int"
            , topLevelDeclarations = """someValue : String -> (Thing -> Thing) -> Int
someValue = something"""
            }
        , fixTest "when value is a tuple"
            { arguments = ""
            , value = """( "abc", 1.0 )"""
            , expectedType = "( String, Float )"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a 3-tuple"
            { arguments = ""
            , value = """( "abc", 1.0, 1 )"""
            , expectedType = "( String, Float, number )"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an empty list"
            -- TODO Create another case where this generic type is already in scope
            { arguments = ""
            , value = """[]"""
            , expectedType = "List nothing"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with known values"
            { arguments = ""
            , value = """[ "abc" ]"""
            , expectedType = "List String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typeclasses"
            { arguments = ""
            , value = """[ 1 ]"""
            , expectedType = "List number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list where the first and some elements are unknown"
            { arguments = ""
            , value = """[ thing, "abc", otherThing ]"""
            , expectedType = "List String"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix when value is a list of only unknown values"
            { arguments = ""
            , value = "[ thing, someThing abc, otherThing ]"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typeclasses where one element is very precise"
            { arguments = ""
            , value = "[ 1, 1.1 ]"
            , expectedType = "List Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typeclasses where one element is very precise (in a different order)"
            { arguments = ""
            , value = "[ 1, a, 1.1, 1 ]"
            , expectedType = "List Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typed elements"
            { arguments = ""
            , value = "[ [1], [1.1] ]"
            , expectedType = "List (List Float)"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an empty record"
            { arguments = ""
            , value = "{}"
            , expectedType = "{}"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record with fields"
            { arguments = ""
            , value = """{ a = "abc", b = 1.0 }"""
            , expectedType = "{ a : String, b : Float }"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix when value is a record where some fields are unknown"
            { arguments = ""
            , value = "{ a = someThing, b = 1.0 }"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a parenthesized expression"
            { arguments = ""
            , value = """("abc")"""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record access on a record"
            { arguments = ""
            , value = """{ a = "abc", b = 1.0 }.a"""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record access on a complex value"
            { arguments = ""
            , value = "(someValue thing).a"
            , expectedType = "String"
            , topLevelDeclarations = """someValue : thing -> { a : String, b : Int }
someValue = something"""
            }
        , fixTest "when value is a record access function"
            -- TODO Create another case where this generic type is already in scope
            { arguments = ""
            , value = ".field"
            , expectedType = "{ b | field : a } -> a"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix (for now) when value is a record access function used on an expression"
            -- TODO This should be supported when we infer type variables better
            { arguments = ""
            , value = ".field { field = 1.0 } "
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a custom type variant without arguments"
            { arguments = ""
            , value = "A"
            , expectedType = "CustomType"
            , topLevelDeclarations = "type CustomType = A | B"
            }
        , fixTest "when value is part of a custom type that will use generics"
            { arguments = ""
            , value = "A"
            , expectedType = "a -> CustomType a"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , noFixTest "should not provide a fix (for now) when a custom type's variant uses generics"
            { arguments = ""
            , value = "A 1"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , fixTest "should provide a fix when a custom type's variant does not use the type's generics"
            { arguments = ""
            , value = "B"
            , expectedType = "CustomType a"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , fixTest "should provide a fix when value is a record type alias"
            { arguments = ""
            , value = "TypeAlias"
            , expectedType = "Int -> String -> TypeAlias"
            , topLevelDeclarations = "type alias TypeAlias = { a : Int, b : String }"
            }
        , fixTest "should provide a fix when value is a record type alias with generics"
            { arguments = ""
            , value = "TypeAlias"
            , expectedType = "b -> String -> TypeAlias a b"
            , topLevelDeclarations = "type alias TypeAlias a b = { a | c : b, d : String }"
            }
        , noFixTest "should not provide a fix when value is an alias to something that is not a record"
            { arguments = ""
            , value = "TypeAlias"
            , topLevelDeclarations = "type alias TypeAlias = Something"
            }
        , fixTest "when value is reference to a ingoing port"
            { arguments = ""
            , value = "input"
            , expectedType = "(String -> msg) -> Sub msg"
            , topLevelDeclarations = "port input : (String -> msg) -> Sub msg"
            }
        , fixTest "when value is reference to a outgoing port"
            { arguments = ""
            , value = "output"
            , expectedType = "String -> Cmd msg"
            , topLevelDeclarations = "port output : String -> Cmd msg"
            }
        , fixTest "when value is an if condition"
            { arguments = ""
            , value = """if condition then "abc" else "bcd" """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and true block is not known"
            { arguments = ""
            , value = """if condition then someThing else "bcd" """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and first condition could be more precise"
            { arguments = ""
            , value = """if condition then 1 else 1.0 """
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is an if condition and first condition could be more precise but second is unknown"
            { arguments = ""
            , value = """if condition then 1 else someThing """
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and both conditions use the same type variables"
            { arguments = ""
            , value = """if condition then 1 else 1 """
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , Test.skip <|
            fixTest "when value is an operator function"
                { arguments = ""
                , value = "(+)"
                , expectedType = "number -> number -> number"
                , topLevelDeclarations = ""
                }
        , noFixTest "should not provide a fix (for now) when type variables are found both in the input parameters and output parameters"
            { arguments = ""
            , value = "someValue string"
            , topLevelDeclarations = """someValue : a -> a
someValue = something"""
            }
        , noFixTest "should not provide a fix (for now) when function has arguments"
            { arguments = "thing"
            , value = "someValue thing"
            , topLevelDeclarations = """someValue : String -> Int
someValue = something"""
            }
        ]


fixTest : String -> { arguments : String, value : String, expectedType : String, topLevelDeclarations : String } -> Test
fixTest title { arguments, value, expectedType, topLevelDeclarations } =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                        |> Review.Test.whenFixed ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation : """ ++ expectedType ++ """
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                    ]


noFixTest : String -> { arguments : String, value : String, topLevelDeclarations : String } -> Test
noFixTest title { arguments, value, topLevelDeclarations } =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                    ]


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.ElmCore.dependency
