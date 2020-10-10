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
            { value = "\"abc\""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal integer"
            { value = "1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal float"
            { value = "1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal unit"
            { value = "()"
            , expectedType = "()"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `True`"
            { value = "False"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `False`"
            { value = "False"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value equals a top-level value: String"
            { value = "someValue"
            , expectedType = "String"
            , topLevelDeclarations = """someValue : String
someValue = "abc\""""
            }
        , fixTest "when value equals a top-level value: Int"
            { value = "someValue"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : Int
someValue = 1"""
            }
        , fixTest "when value equals a top-level value: Float"
            { value = "someValue"
            , expectedType = "Float"
            , topLevelDeclarations = """someValue : Float
someValue = 1.0"""
            }
        , fixTest "when value equals a top-level value: unit"
            { value = "someValue"
            , expectedType = "()"
            , topLevelDeclarations = """someValue : ()
someValue = ()"""
            }
        ]


fixTest : String -> { value : String, expectedType : String, topLevelDeclarations : String } -> Test
fixTest title { value, expectedType, topLevelDeclarations } =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation = """ ++ value ++ """
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
      hasNoTypeAnnotation = """ ++ value ++ """
    in
    d
""")
                    ]


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.ElmCore.dependency
