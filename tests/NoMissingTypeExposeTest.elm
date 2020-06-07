module NoMissingTypeExposeTest exposing (all)

import NoMissingTypeExpose exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingTypeExpose"
        [ describe "exposed functions" functionTests
        ]


functionTests : List Test
functionTests =
    [ test "passes when an exposed function uses an exposed type" <|
        \() ->
            """
module Happiness exposing (Happiness, toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "reports when an exposed function uses a private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Happiness -> String
toString howHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function returns a private type" <|
        \() ->
            """
module Happiness exposing (ecstatic)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


ecstatic : Happiness
ecstatic =
    Ecstatic
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function uses a typed private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function uses an external typed private type" <|
        \() ->
            """
module Happiness exposing (toString)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


toString : Maybe.Maybe Happiness -> String
toString maybeHappy =
    "Very"
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function uses a private type in a tuple" <|
        \() ->
            """
module Happiness exposing (equal)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


equal : ( Happiness, Happiness ) -> Bool
equal ( a, b ) =
    a == b
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function uses a private type in a record" <|
        \() ->
            """
module Happiness exposing (rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { happiness : Happiness } -> Int
rank { happiness } =
    0
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    , test "reports when an exposed function uses a private type in a generic record" <|
        \() ->
            """
module Happiness exposing (rank)


type Happiness
    = Ecstatic
    | FineIGuess
    | Unhappy


rank : { a | happiness : Happiness } -> Int
rank { happiness } =
    0
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Private type `Happiness` used by exposed function"
                        , details =
                            [ "Type `Happiness` is used by an exposed function but is not exposed itself."
                            ]
                        , under = "Happiness"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 15 } }
                    ]
    ]
