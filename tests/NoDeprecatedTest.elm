module NoDeprecatedTest exposing (all)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import NoDeprecated exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoDeprecated"
        [ mainTests
        , dependencyTests
        ]


mainTests : Test
mainTests =
    describe "Main"
        [ test "should not report an error when using a non-deprecated element" <|
            \() ->
                [ """module A exposing (..)
import Other
a = Other.normalFunction
""", """module Other exposing (..)
normalFunction = 1
""" ]
                    |> Review.Test.runOnModules (rule NoDeprecated.checkInName)
                    |> Review.Test.expectNoErrors
        , test "should report an error when referencing a local function whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
somethingDeprecated = 1

a = somethingDeprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "somethingDeprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 24 } }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (qualified import)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule
a = Some.DeprecatedModule.something
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Some.DeprecatedModule.something"
                            }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (unqualifed import)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule as S
a = S.something
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "S.something"
                            }
                        ]
        , test "should report an error when referencing a function from a module whose name contains 'deprecated' (record update)" <|
            \() ->
                """module A exposing (..)
import Some.DeprecatedModule exposing (something)
a = { something | b = 1 }
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "something"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                        ]
        , test "should report an error when referencing a custom type constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type alias constructor whose name contains 'deprecated'" <|
            \() ->
                """module A exposing (..)
type alias Deprecated = {}
a = Deprecated
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a : Deprecated
a = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a (Deprecated value) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 4 }, end = { row = 3, column = 14 } }
                        ]
        , test "should report an error when having a parameter whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a thingDeprecated = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when destructuring a field whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a ({deprecated}) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "deprecated"
                            }
                        ]
        , test "should report an error when using a parameter alias whose name contains 'deprecated' (top-level declaration)" <|
            \() ->
                """module A exposing (..)
a (( x, y ) as deprecated) = 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "deprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let declaration annotation)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Int
a =
    let
        b : Deprecated
        b = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 13 }, end = { row = 5, column = 23 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        b (Deprecated value) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 12 }, end = { row = 5, column = 22 } }
                        ]
        , test "should report an error when having a parameter whose name contains 'deprecated' (let declaration)" <|
            \() ->
                """module A exposing (..)
a =
    let
        b thingDeprecated = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "thingDeprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (let destructuring)" <|
            \() ->
                """module A exposing (..)
type Deprecated = Deprecated Int
a =
    let
        (Deprecated b) = 1
    in
    b
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 10 }, end = { row = 5, column = 20 } }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (case expression)" <|
            \() ->
                """module A exposing (..)
a =
    case x of
        ThingDeprecated b -> 1
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "ThingDeprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (custom type declaration)" <|
            \() ->
                """module A exposing (..)
type A = Thing ( A, { b : Deprecated } )
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (type alias declaration)" <|
            \() ->
                """module A exposing (..)
type alias A = Thing{ b : Deprecated }
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "Deprecated"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (Sub port)" <|
            \() ->
                """module A exposing (..)
port input : (DeprecatedString -> msg) -> Sub msg
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "DeprecatedString"
                            }
                        ]
        , test "should report an error when referencing a type whose name contains 'deprecated' (Cmd port)" <|
            \() ->
                """module A exposing (..)
port output : DeprecatedString -> Cmd msg
"""
                    |> Review.Test.run (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "DeprecatedString"
                            }
                        ]
        ]


dependencyTests : Test
dependencyTests =
    describe "Dependencies"
        [ test "should report an error when referencing a value from a deprecated module" <|
            \() ->
                """module A exposing (..)
import ModuleFromDependency_1
a = ModuleFromDependency_1.something
"""
                    |> Review.Test.runWithProjectData projectWithDeprecations (rule NoDeprecated.checkInName)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Found new usage of deprecated element"
                            , details = [ "REPLACEME" ]
                            , under = "ModuleFromDependency_1.something"
                            }
                        ]
        ]


projectWithDeprecations : Project
projectWithDeprecations =
    Project.addDependency dependency Project.new


dependency : Dependency
dependency =
    Dependency.create
        "author/package"
        (createElmJsonProject dependencyElmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("[elm.json]: " ++ Debug.toString error)


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "ModuleFromDependency_1"
      , comment = "{-| This is deprecated, use X instead. -}"
      , unions =
            [ { name = "CustomType"
              , comment = ""
              , args = []
              , tags = [ ( "Constructor", [] ) ]
              }
            ]
      , aliases =
            [ { name = "Happiness"
              , comment = ""
              , args = []
              , tipe = Elm.Type.Tuple []
              }
            ]
      , values =
            [ { name = "Happiness"
              , comment = ""
              , tipe = Elm.Type.Tuple []
              }
            ]
      , binops = []
      }
    , { name = "ModuleFromDependency_2"
      , comment = ""
      , unions =
            [ { name = "CustomType"
              , comment = "{-| This is deprecated -}"
              , args = []
              , tags = [ ( "Constructor", [] ) ]
              }
            ]
      , aliases =
            [ { name = "Happiness"
              , comment = "{-| This is deprecated -}"
              , args = []
              , tipe = Elm.Type.Tuple []
              }
            ]
      , values =
            [ { name = "Happiness"
              , comment = "{-| This is deprecated -}"
              , tipe = Elm.Type.Tuple []
              }
            ]
      , binops = []
      }
    ]


dependencyElmJson : String
dependencyElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Moods for tests",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
        "ModuleFromDependency_1",
        "ModuleFromDependency_2"
    ],
    "elm-version": "0.19.0 <= v <= 0.20.0",
    "dependencies": {},
    "test-dependencies": {}
}"""
