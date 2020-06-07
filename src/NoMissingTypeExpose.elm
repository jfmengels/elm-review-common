module NoMissingTypeExpose exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
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
        Declaration.CustomTypeDeclaration { name, constructors } ->
            context
                |> rememberDeclaredType name
                |> rememberValueConstructorList name constructors

        Declaration.FunctionDeclaration { signature } ->
            rememberFunctionSignature signature context

        _ ->
            context


rememberDeclaredType : Node String -> Context -> Context
rememberDeclaredType name context =
    { context | declaredTypes = name :: context.declaredTypes }


rememberValueConstructorList : Node String -> List (Node Type.ValueConstructor) -> Context -> Context
rememberValueConstructorList (Node _ name) list context =
    if isTypeExposedOpen name context.exposes then
        List.foldl rememberValueConstructor context list

    else
        context


isTypeExposedOpen : String -> Exposing -> Bool
isTypeExposedOpen name exposes =
    case exposes of
        Exposing.All _ ->
            True

        Exposing.Explicit list ->
            List.any (isExposingAnOpenTypeNamed name) list


isExposingAnOpenTypeNamed : String -> Node Exposing.TopLevelExpose -> Bool
isExposingAnOpenTypeNamed needle (Node _ expose) =
    case expose of
        Exposing.TypeExpose { name, open } ->
            name == needle && open /= Nothing

        _ ->
            False


rememberValueConstructor : Node Type.ValueConstructor -> Context -> Context
rememberValueConstructor (Node _ { arguments }) context =
    rememberTypeAnnotationList arguments context


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


rememberRecordFieldList : List (Node TypeAnnotation.RecordField) -> Context -> Context
rememberRecordFieldList fields context =
    List.foldl rememberRecordField context fields


rememberRecordField : Node TypeAnnotation.RecordField -> Context -> Context
rememberRecordField (Node _ ( _, typeAnnotation )) context =
    context
        |> rememberTypeAnnotation typeAnnotation


rememberTypeAnnotationList : List (Node TypeAnnotation) -> Context -> Context
rememberTypeAnnotationList list context =
    List.foldl rememberTypeAnnotation context list


rememberTypeAnnotation : Node TypeAnnotation -> Context -> Context
rememberTypeAnnotation (Node _ typeAnnotation) context =
    case typeAnnotation of
        TypeAnnotation.Typed (Node _ name) list ->
            context
                |> rememberExposedSignatureType name
                |> rememberTypeAnnotationList list

        TypeAnnotation.FunctionTypeAnnotation left right ->
            context
                |> rememberTypeAnnotation left
                |> rememberTypeAnnotation right

        TypeAnnotation.Tupled list ->
            context
                |> rememberTypeAnnotationList list

        TypeAnnotation.Record fields ->
            context
                |> rememberRecordFieldList fields

        TypeAnnotation.GenericRecord _ (Node _ fields) ->
            context
                |> rememberRecordFieldList fields

        TypeAnnotation.Unit ->
            context

        TypeAnnotation.GenericType _ ->
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
