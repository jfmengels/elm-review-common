module NoMissingTypeAnnotationInLetInTest exposing (all)

import NoMissingTypeAnnotationInLetIn exposing (rule)
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
                    |> Review.Test.run rule
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
                    |> Review.Test.run rule
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
                    |> Review.Test.run rule
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
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


fixTests : Test
fixTests =
    describe "Fixing"
        [ fixTest "when value is a literal string"
            { value = "\"abc\""
            , expectedType = "String"
            }
        , fixTest "when value is a literal integer"
            { value = "1"
            , expectedType = "number"
            }
        , fixTest "when value is a literal float"
            { value = "1.0"
            , expectedType = "Float"
            }
        , fixTest "when value is a literal unit"
            { value = "()"
            , expectedType = "()"
            }
        ]


fixTest : String -> { value : String, expectedType : String } -> Test
fixTest title { value, expectedType } =
    test title <|
        \_ ->
            ("""module A exposing (..)
a = let
      hasNoTypeAnnotation = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                        |> Review.Test.whenFixed ("""module A exposing (..)
a = let
      hasNoTypeAnnotation : """ ++ expectedType ++ """
      hasNoTypeAnnotation = """ ++ value ++ """
    in
    d
""")
                    ]
