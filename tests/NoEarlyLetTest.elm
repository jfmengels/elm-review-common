module NoEarlyLetTest exposing (all)

import NoEarlyLet exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Handle destructuring lets with multiple variables
-- TODO Incorporate https://github.com/jfmengels/elm-review/discussions/93 ? As an option?
--      Not needed because this rule currently only targets is only for functions


message : String
message =
    "Let value was declared prematurely"


details : Int -> List String
details letInsertLine =
    [ "This value is only used in some code paths, and it can therefore be computed unnecessarily."
    , "Try moving it closer to where it is needed, I recommend to move it to line " ++ String.fromInt letInsertLine ++ "."
    ]


all : Test
all =
    describe "NoEarlyLet"
        [ test "should report a let declaration that could be computed in a if branch" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z : Int
    z = 1
  in
  if b then
    z
  else
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 8
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  if b then
    let
        z : Int
        z = 1
    in
    z
  else
    1
"""
                        ]
        , test "should report a let declaration that could be computed in a if branch (referenced by record update expression)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = {a = 1}
  in
  if b then
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 7
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  if b then
    let
        z = {a = 1}
    in
    {z | a = 2}
  else
    {a = 3}
"""
                        ]
        , test "should add to an existing let block instead of inserting a new let block" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = {a = 1}
  in
  if b then
    let
      y = 1
    in
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 8
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  if b then
    let
      z = {a = 1}
      y = 1
    in
    {z | a = 2}
  else
    {a = 3}
"""
                        ]
        , test "should not report let functions" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z n = 1
  in
  if b then
    z 1
  else
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report a let declaration is used in multiple if branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  if b then
    z
  else
    z + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report a let declaration if it's unused" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  if b then
    1
  else
    2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report a let declaration is used next to where it was declared" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
    y = z * 2
  in
  if b then
    y
  else
    y + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report a let declaration without branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a let declaration that could be computed in a case branch" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  case b of
    A ->
        1
    B ->
        z
    C ->
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 10
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  case b of
    A ->
        1
    B ->
        let
            z = 1
        in
        z
    C ->
        1
"""
                        ]
        , test "should not report a let declaration is used in multiple case branches" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = 1
  in
  case b of
    A ->
        z
    B ->
        z
    C ->
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should only remove the moved let declaration when there are other let declarations there" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    y = 1
    z = {a = 1}
  in
  if b then
    {z | a = 2}
  else
    {a = 3}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 8
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  let
    y = 1

  in
  if b then
    let
        z = {a = 1}
    in
    {z | a = 2}
  else
    {a = 3}
"""
                        ]
        , test "should not be confused by what happens in other declarations" <|
            \() ->
                """module A exposing (..)
first =
  case A of
    A -> z

second =
    let
        z = let x = 1 in x
        y = z
    in
    y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when a declaration is used in multiple sibling's declarations" <|
            \() ->
                """module A exposing (..)
fun =
    let
        z = 1
        y = z
        x = z
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when a declaration is used in multiple sibling's branches" <|
            \() ->
                """module A exposing (..)
fun =
    let
        z = 1
        y = if True then z else 1
        x = if True then z else 1
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when a declaration is used in a branch of a sibling's declaration" <|
            \() ->
                """module A exposing (..)
a =
    let
        z = 1

        viewInput =
            let
                submitOnEnter =
                    if True then
                        z

                    else
                        1
            in
            submitOnEnter

        viewButtonRow =
                z
    in
    1

a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should move a let declaration from one let to the other" <|
            \() ->
                """module A exposing (..)
a =
  let
    z = 1
  in
  let
    y = 1
    x = 1
  in
  y + z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 7
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  let
    z = 1
    y = 1
    x = 1
  in
  y + z
"""
                        ]
        , test "should properly indent moved let declaration defined on multiple lines" <|
            \() ->
                """module A exposing (..)
a o =
    let
        z =
            foo
                bar
                baz
    in
    thing
        (if o.expDetail then
            let
                y =
                    foo2
                        bar2
                        baz2
            in
            [ y
            , z
            ]

        else
            []
       )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 12
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a o =
    thing
        (if o.expDetail then
            let
                z =
                    foo
                        bar
                        baz
                y =
                    foo2
                        bar2
                        baz2
            in
            [ y
            , z
            ]

        else
            []
       )
"""
                        ]
        , test "should not suggest a fix for let declarations that introduce variables in their implementation (lambda)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z : Int
    z = \\y -> y + 1
  in
  case b of
    A y ->
      if b then
        z
      else
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 10
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        ]
        , test "should suggest a fix for lambda that does not introduce variables" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = \\() _ -> 1
  in
  case b of
    A y ->
      if b then
        z
      else
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 9
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed
                                ("""module A exposing (..)
a b c d =
  case b of
    A y ->
      if b then
        let
            z = \\() _ -> 1
         $
        in
        z
      else
        1
""" |> String.replace "$" " ")
                        ]
        , test "should not suggest a fix for let declarations that introduce variables in their implementation (let block)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z : Int
    z = let y = 1
        in y
  in
  case b of
    A y ->
      if b then
        z
      else
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 11
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        ]
        , test "should not suggest a fix for let declarations that introduce variables in their implementation but still suggest fixes for others" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z : Int
    z = \\y -> y + 1
    x = 1
  in
  case b of
    A y ->
      if b then
        z
      else
        x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 11
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        , Review.Test.error
                            { message = message
                            , details = details 13
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  let
    z : Int
    z = \\y -> y + 1

  in
  case b of
    A y ->
      if b then
        z
      else
        let
            x = 1
        in
        x
"""
                        ]
        , test "should not suggest a fix for let declarations that introduce variables in their implementation (case expression)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z : Int
    z = case c of
      B y -> y + 1
  in
  case b of
    A y ->
      if b then
        z
      else
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 11
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                        ]
        , test "should suggest a fix for let declarations that do not introduce variables in their implementation (case expression)" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    z = case c of
      B -> 1
  in
  case b of
    A y ->
      if b then
        z
      else
        1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 10
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  case b of
    A y ->
      if b then
        let
            z = case c of
              B -> 1
        in
        z
      else
        1
"""
                        ]
        , test "should report a let destructuring with a single value" <|
            \() ->
                """module A exposing (..)
a b c d =
  let
    {z} = {z = 1}
  in
  if b then
    z
  else
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 7
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a b c d =
  if b then
    let
        {z} = {z = 1}
    in
    z
  else
    1
"""
                        ]
        , test "should not report let declaration that would be moved to inside a lambda" <|
            \() ->
                """module A exposing (..)
a =
  let
    z = 1
  in
  (\\b ->
      if b then
        z
      else
        1
  )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should try to move as close as possible to a lambda but not inside" <|
            \() ->
                """module A exposing (..)
a =
  let
    z = 1
  in
  if c then
    (\\b ->
        if b then
          z
        else
          1
    )
  else
    identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details 7
                            , under = "z"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if c then
    let
        z = 1
    in
    (\\b ->
        if b then
          z
        else
          1
    )
  else
    identity
"""
                        ]
        ]
