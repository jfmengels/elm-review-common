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
        , test "should report an error when a top-level variable has a trailing _" <|
            \() ->
                """module A exposing (..)
a_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "a_"
                            }
                        ]
        , test "should not report an error when a top-level variable has a trailing _ whose name is like a reserved keyword" <|
            \() ->
                """module A exposing (..)
if_ = 1
then_ = 1
else_ = 1
case_ = 1
of_ = 1
let_ = 1
in_ = 1
type_ = 1
module_ = 1
where_ = 1
import_ = 1
exposing_ = 1
as_ = 1
port_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when argument in parens has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value_"
                            }
                        ]
        , test "should report an error when arguments in alias has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value1_ as value2_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value2_"
                            }
                        ]
        , test "should report an error when arguments in tuple has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value1_, value2_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value2_"
                            }
                        ]
        , test "should report an error when arguments in uncons pattern has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (Foo value_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value_"
                            }
                        ]
        , test "should report an error when variables from case expression patterns have unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value_"
                            }
                        ]
        , test "should report an error for uncons patterns" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    value1_ :: value2_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value2_"
                            }
                        ]
        , test "should report an error for list patterns" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    [value1_ , value2_] -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "value2_"
                            }
                        ]
        ]
