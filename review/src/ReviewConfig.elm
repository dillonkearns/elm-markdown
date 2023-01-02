module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoEmptyDocComments
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoUnmatchedUnit
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    ([ NoEmptyDocComments.rule
        |> Rule.ignoreErrorsForFiles [ "src/List/Helpers.elm" ]
     , Docs.NoMissing.rule
        { document = onlyExposed
        , from = exposedModules
        }
     , Docs.ReviewLinksAndSections.rule
     , Docs.ReviewAtDocs.rule
     , Docs.UpToDateReadmeLinks.rule
     , NoExposingEverything.rule
     , NoPrematureLetComputation.rule
     , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "known-unoptimized-recursion")
        |> ignoreInTest
     , NoDebug.Log.rule
     , NoDebug.TodoOrToString.rule
        |> ignoreInTest
     , NoMissingTypeAnnotation.rule
     , NoMissingTypeAnnotationInLetIn.rule
     , NoMissingTypeExpose.rule
     , NoUnmatchedUnit.rule
     ]
        ++ (noUnusedRules
                |> List.map
                    (\rule ->
                        rule
                            |> Rule.ignoreErrorsForFiles
                                []
                    )
           )
    )
        |> List.map
            (\rule ->
                rule
                    |> Rule.ignoreErrorsForDirectories
                        []
                    |> Rule.ignoreErrorsForFiles
                        [ -- vendored code
                          "src/HtmlParser.elm"
                        ]
            )


noUnusedRules : List Rule
noUnusedRules =
    [ NoUnused.CustomTypeConstructors.rule []
        |> ignoreInTest
    , NoUnused.CustomTypeConstructorArgs.rule
        |> ignoreInTest
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> ignoreInTest
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]


ignoreInTest : Rule -> Rule
ignoreInTest rule =
    rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
