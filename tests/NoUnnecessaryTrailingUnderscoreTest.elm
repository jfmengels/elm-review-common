module NoUnnecessaryTrailingUnderscoreTest exposing (all)

import NoUnnecessaryTrailingUnderscore exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoUnnecessaryTrailingUnderscore"
        [ test "should report an error when argument has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a value_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value_"
                            }
                        ]
        , test "should not report an error when argument does not have a trailing _" <|
            \() ->
                """module A exposing (..)
a value va_lue = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when another argument with the same name without the _ exists" <|
            \() ->
                """module A exposing (..)
a value_ value = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the name without _ is a reserved keyword" <|
            \() ->
                """module A exposing (..)
a if_ then_ else_ case_ of_ let_ in_ type_ module_ where_ import_ exposing_ as_ port_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
