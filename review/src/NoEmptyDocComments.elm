module NoEmptyDocComments exposing (..)

import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoEmptyDocComments" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration.Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration declaration ->
            case declaration.documentation of
                Just documentation ->
                    if
                        documentation
                            |> Node.value
                            |> String.dropLeft 3
                            |> String.dropRight 2
                            |> String.trim
                            |> String.isEmpty
                    then
                        [ Rule.error
                            { message = "It's not done until the docs are great. Or at least present."
                            , details =
                                [ "https://twitter.com/czaplic/status/928359227541798912"
                                ]
                            }
                            (Node.range documentation)
                        ]

                    else
                        []

                Nothing ->
                    []

        _ ->
            []
