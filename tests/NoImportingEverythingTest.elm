module NoImportingEverythingTest exposing (all)

import Dependencies.ElmCore
import Dependencies.ElmHtml
import Elm.Project
import Json.Decode as Decode
import NoImportingEverything exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoImportingEverything"
        [ describe "for package" (testsForProject packageProject)
        , describe "for application" (testsForProject applicationProject)
        ]


testsForProject : Project -> List Test
testsForProject project =
    [ test "should not report imports without exposing clause" <|
        \_ ->
            """module A exposing (thing)
import Html
import Html as B
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should not report imports that expose some elements" <|
        \_ ->
            """module A exposing (thing)
import Html exposing (B, c)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should not report imports that expose all constructors of a type" <|
        \_ ->
            """module A exposing (thing)
import Html exposing (B(..))
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectNoErrors
    , test "should report imports that expose everything" <|
        \_ ->
            """module A exposing (thing)
import Html exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (thing)
import Html
"""
                    ]
    , test "should report aliased imports that expose everything" <|
        \_ ->
            """module A exposing (thing)
import Json.Decode as JD exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (thing)
import Json.Decode as JD
"""
                    ]
    , test "should only include used values in the fixed exposing list" <|
        \_ ->
            """module A exposing (view)
import Html exposing (..)
view = p [] [ strong [] [ text "Thing" ] ]
"""
                |> Review.Test.runWithProjectData project (rule [])
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Prefer listing what you wish to import and/or using qualified imports"
                        , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                        , under = "(..)"
                        }
                        |> Review.Test.whenFixed """module A exposing (view)
import Html exposing (p, strong, text)
view = p [] [ strong [] [ text "Thing" ] ]
"""
                    ]
    , test "should not report imports that are in the exceptions list" <|
        \_ ->
            """module A exposing (thing)
import Html exposing (..)
import Thing.Foo as Foo exposing (..)
"""
                |> Review.Test.runWithProjectData project (rule [ "Html", "Thing.Foo" ])
                |> Review.Test.expectNoErrors
    ]


applicationProject : Project
applicationProject =
    Project.new
        |> Project.addElmJson (createElmJson applicationElmJson)
        |> Project.addDependency Dependencies.ElmCore.dependency
        |> Project.addDependency Dependencies.ElmHtml.dependency


packageProject : Project
packageProject =
    Project.new
        |> Project.addElmJson (createElmJson packageElmJson)
        |> Project.addDependency Dependencies.ElmCore.dependency
        |> Project.addDependency Dependencies.ElmHtml.dependency


applicationElmJson : String
applicationElmJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "elm/html": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""


packageElmJson : String
packageElmJson =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0",
        "elm/html": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)
