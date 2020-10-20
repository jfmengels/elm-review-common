module TypeInference.ModuleInformation exposing
    ( ModuleInformation
    , ModuleInformationDict
    , empty
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.Dependency
import TypeInference.Type as Type


type ModuleInformationDict
    = ModuleInformationDict (Dict ModuleName ModuleInformation)


fromDependencies : Dict String Review.Project.Dependency.Dependency -> ModuleInformationDict
fromDependencies dependencies =
    dependencies
        |> Dict.values
        |> List.concatMap Review.Project.Dependency.modules
        |> List.map (\module_ -> ( String.split "." module_.name, ModuleInformation ))
        |> Dict.fromList
        |> ModuleInformationDict


empty : ModuleInformationDict
empty =
    ModuleInformationDict Dict.empty


type ModuleInformation
    = ModuleInformation


binops : ModuleInformation -> Dict String Type.Type
binops moduleInformation =
    Dict.empty
