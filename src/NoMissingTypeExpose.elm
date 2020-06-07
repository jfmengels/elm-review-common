module NoMissingTypeExpose exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTypeExpose" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor (Node _ mod) context =
    ( [], { context | exposes = Module.exposingList mod } )


declarationListVisitor : List (Node Declaration) -> Context -> ( List (Rule.Error {}), Context )
declarationListVisitor nodes context =
    case context.exposes of
        Exposing.All _ ->
            ( [], context )

        _ ->
            ( []
            , List.foldl rememberDeclaration context nodes
            )


rememberDeclaration : Node Declaration -> Context -> Context
rememberDeclaration (Node _ declaration) context =
    case declaration of
        Declaration.CustomTypeDeclaration { name } ->
            { context | declaredTypes = name :: context.declaredTypes }

        Declaration.FunctionDeclaration { signature } ->
            rememberFunctionSignature signature context

        _ ->
            context


rememberFunctionSignature : Maybe (Node Signature) -> Context -> Context
rememberFunctionSignature maybeSignature context =
    case maybeSignature of
        Just (Node _ { name, typeAnnotation }) ->
            if Exposing.exposesFunction (Node.value name) context.exposes then
                rememberTypeAnnotation typeAnnotation context

            else
                context

        Nothing ->
            context


rememberTypeAnnotation : Node TypeAnnotation -> Context -> Context
rememberTypeAnnotation (Node _ typeAnnotation) context =
    case typeAnnotation of
        TypeAnnotation.Typed (Node _ name) list ->
            List.foldl rememberTypeAnnotation context list
                |> rememberExposedSignatureType name

        TypeAnnotation.FunctionTypeAnnotation left right ->
            context
                |> rememberTypeAnnotation left
                |> rememberTypeAnnotation right

        _ ->
            context


rememberExposedSignatureType : ( List String, String ) -> Context -> Context
rememberExposedSignatureType qualifiedName context =
    case qualifiedName of
        ( [], name ) ->
            { context
                | exposedSignatureTypes = name :: context.exposedSignatureTypes
            }

        _ ->
            context


finalEvaluation : Context -> List (Rule.Error {})
finalEvaluation context =
    let
        exposedSignatureTypes : Set String
        exposedSignatureTypes =
            Set.fromList context.exposedSignatureTypes
    in
    context.declaredTypes
        |> List.filter (isTypePrivate context.exposes)
        |> List.filter (\(Node _ name) -> Set.member name exposedSignatureTypes)
        |> List.map makeError


isTypePrivate : Exposing -> Node String -> Bool
isTypePrivate exposes (Node _ name) =
    case exposes of
        Exposing.All _ ->
            False

        Exposing.Explicit list ->
            not (List.any (isExposingATypeNamed name) list)


isExposingATypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingATypeNamed needle (Node _ topLevelExpose) =
    case topLevelExpose of
        Exposing.InfixExpose _ ->
            False

        Exposing.FunctionExpose _ ->
            False

        Exposing.TypeOrAliasExpose name ->
            name == needle

        Exposing.TypeExpose { name } ->
            name == needle


makeError : Node String -> Rule.Error {}
makeError (Node range name) =
    Rule.error
        { message = "Private type `" ++ name ++ "` used by exposed function"
        , details =
            [ "Type `" ++ name ++ "` is used by an exposed function but is not exposed itself."
            ]
        }
        range


initialContext : Context
initialContext =
    { exposes = Exposing.Explicit []
    , exposedSignatureTypes = []
    , declaredTypes = []
    }


type alias Context =
    { exposes : Exposing
    , exposedSignatureTypes : List String
    , declaredTypes : List (Node String)
    }
