module Markdown.InlineParser exposing (parse, query, tokenize, walk)

import Dict exposing (Dict)
import HtmlParser
import Markdown.Helpers exposing (Attribute, References, cleanWhitespaces, formatStr, ifError, insideSquareBracketRegex, isEven, lineEndChars, prepareRefLabel, returnFirstJust, titleRegex, whiteSpaceChars)
import Markdown.Inline exposing (..)
import Parser.Advanced as Advanced exposing ((|.), (|=))
import Regex exposing (Regex)
import Url



-- Parser Model & Helpers


type alias Parser =
    { rawText : String
    , tokens : List Token
    , matches : List Match
    , refs : References
    }


addMatch : Parser -> Match -> Parser
addMatch model match =
    { rawText = model.rawText
    , tokens = model.tokens
    , matches = match :: model.matches
    , refs = model.refs
    }


addToken : Parser -> Token -> Parser
addToken model token =
    { rawText = model.rawText
    , tokens = token :: model.tokens
    , matches = model.matches
    , refs = model.refs
    }


filterTokens : (Token -> Bool) -> Parser -> Parser
filterTokens filter model =
    { rawText = model.rawText
    , tokens = List.filter filter model.tokens
    , matches = model.matches
    , refs = model.refs
    }


reverseTokens : Parser -> Parser
reverseTokens model =
    { rawText = model.rawText
    , tokens = List.reverse model.tokens
    , matches = model.matches
    , refs = model.refs
    }


initParser : References -> String -> Parser
initParser refs rawText =
    { rawText = rawText
    , tokens = []
    , matches = []
    , refs = refs
    }



-- Parser


parse : References -> String -> List Inline
parse refs rawText =
    let
        tokens =
            tokenize (String.trim rawText)

        parser =
            { rawText = String.trim rawText
            , tokens = tokens
            , matches = []
            , refs = refs
            }
    in
    parser
        |> tokensToMatches
        |> organizeParserMatches
        |> parseText
        |> .matches
        |> matchesToInlines


parseText : Parser -> Parser
parseText model =
    { matches = parseTextMatches model.rawText [] model.matches
    , rawText = model.rawText
    , tokens = model.tokens
    , refs = model.refs
    }


parseTextMatches : String -> List Match -> List Match -> List Match
parseTextMatches rawText parsedMatches matches =
    case matches of
        [] ->
            case parsedMatches of
                [] ->
                    -- No text to parse
                    if String.isEmpty rawText then
                        []
                        -- No match found

                    else
                        [ normalMatch rawText ]

                -- Add initial normal match
                (Match matchModel) :: _ ->
                    if matchModel.start > 0 then
                        normalMatch (String.left matchModel.start rawText)
                            :: parsedMatches

                    else
                        parsedMatches

        match :: matchesTail ->
            parseTextMatches rawText
                (parseTextMatch rawText match parsedMatches)
                matchesTail


parseTextMatch : String -> Match -> List Match -> List Match
parseTextMatch rawText (Match matchModel) parsedMatches =
    let
        updtMatch : Match
        updtMatch =
            Match
                { type_ = matchModel.type_
                , start = matchModel.start
                , end = matchModel.end
                , textStart = matchModel.textStart
                , textEnd = matchModel.textEnd
                , text = matchModel.text
                , matches = parseTextMatches matchModel.text [] matchModel.matches
                }
    in
    case parsedMatches of
        [] ->
            -- Add final normal match
            let
                finalStr =
                    String.dropLeft matchModel.end rawText
            in
            if String.isEmpty finalStr then
                [ updtMatch ]

            else
                [ updtMatch
                , normalMatch finalStr
                ]

        (Match matchHead) :: matchesTail ->
            case matchHead.type_ of
                NormalType ->
                    updtMatch :: parsedMatches

                _ ->
                    if matchModel.end == matchHead.start then
                        -- New Match
                        updtMatch :: parsedMatches

                    else if matchModel.end < matchHead.start then
                        -- New Match and add in between unmatched string
                        updtMatch
                            :: normalMatch (String.slice matchModel.end matchHead.start rawText)
                            :: parsedMatches

                    else
                        -- Overlaping or inside previous Match
                        parsedMatches



-- TOKEN


type alias Token =
    { index : Int
    , length : Int
    , meaning : Meaning
    }


type Meaning
    = CodeToken Bool -- isEscaped
    | LinkOpenToken Bool -- isActive
    | ImageOpenToken
    | SquareBracketClose
    | AngleBracketOpen
    | AngleBracketClose Bool -- isEscaped
    | HtmlToken Bool HtmlModel -- isOpening
    | EmphasisToken Char ( Int, Int ) -- ( leftFringeRank, rightFringeRank )
    | SoftLineBreakToken
    | HardLineBreakToken


findToken : (Token -> Bool) -> List Token -> Maybe ( Token, List Token, List Token )
findToken isToken tokens =
    findTokenHelp [] isToken tokens


findTokenHelp : List Token -> (Token -> Bool) -> List Token -> Maybe ( Token, List Token, List Token )
findTokenHelp innerTokens isToken tokens =
    case tokens of
        [] ->
            Nothing

        nextToken :: remainingTokens ->
            if isToken nextToken then
                Just
                    ( nextToken
                    , List.reverse innerTokens
                    , remainingTokens
                    )

            else
                findTokenHelp
                    (nextToken :: innerTokens)
                    isToken
                    remainingTokens


tokenPairToMatch : References -> String -> (String -> String) -> Type -> Token -> Token -> List Token -> Match
tokenPairToMatch references rawText processText type_ openToken closeToken innerTokens =
    let
        start =
            openToken.index

        end =
            closeToken.index + closeToken.length

        textStart =
            openToken.index + openToken.length

        textEnd =
            closeToken.index

        text =
            String.slice textStart textEnd rawText
                |> processText

        match : MatchModel
        match =
            { type_ = type_
            , start = start
            , end = end
            , textStart = textStart
            , textEnd = textEnd
            , text = text
            , matches = []
            }

        matches : List Match
        matches =
            let
                initialModel =
                    { tokens = innerTokens
                    , matches = []
                    , rawText = rawText
                    , refs = references
                    }
            in
            tokensToMatches initialModel
                |> .matches
                |> List.map
                    (\(Match matchModel) ->
                        prepareChildMatch match matchModel
                    )
    in
    Match
        { type_ = type_
        , start = start
        , end = end
        , textStart = textStart
        , textEnd = textEnd
        , text = text
        , matches = matches
        }


tokenToMatch : Token -> Type -> Match
tokenToMatch token type_ =
    { type_ = type_
    , start = token.index
    , end = token.index + token.length
    , textStart = 0
    , textEnd = 0
    , text = ""
    , matches = []
    }
        |> Match



-- Find Tokens
-- Scan all tokens from the string


tokenize : String -> List Token
tokenize rawText =
    findCodeTokens rawText
        |> mergeByIndex (findAsteriskEmphasisTokens rawText)
        |> mergeByIndex (findUnderlineEmphasisTokens rawText)
        |> mergeByIndex (findLinkImageOpenTokens rawText)
        |> mergeByIndex (findLinkImageCloseTokens rawText)
        |> mergeByIndex (findHardBreakTokens rawText)
        |> mergeByIndex (findAngleBracketLTokens rawText)
        |> mergeByIndex (findAngleBracketRTokens rawText)


{-| Merges two sorted sequences into a sorted sequence
-}
mergeByIndex : List { a | index : Int } -> List { a | index : Int } -> List { a | index : Int }
mergeByIndex left right =
    case left of
        lfirst :: lrest ->
            case right of
                rfirst :: rrest ->
                    if lfirst.index < rfirst.index then
                        lfirst :: mergeByIndex lrest right

                    else
                        rfirst :: mergeByIndex left rrest

                [] ->
                    left

        [] ->
            right



-- Code Tokens


findCodeTokens : String -> List Token
findCodeTokens str =
    Regex.find codeTokenRegex str
        |> List.filterMap regMatchToCodeToken


codeTokenRegex : Regex
codeTokenRegex =
    Regex.fromString "(\\\\*)(\\`+)"
        |> Maybe.withDefault Regex.never


regMatchToCodeToken : Regex.Match -> Maybe Token
regMatchToCodeToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: (Just backtick) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0
            in
            Just
                { index = regMatch.index + backslashesLength
                , length = String.length backtick
                , meaning = CodeToken (not (isEven backslashesLength))
                }

        _ ->
            Nothing



-- Emphasis Tokens


findAsteriskEmphasisTokens : String -> List Token
findAsteriskEmphasisTokens str =
    Regex.find asteriskEmphasisTokenRegex str
        |> List.filterMap (regMatchToEmphasisToken '*' str)


asteriskEmphasisTokenRegex : Regex
asteriskEmphasisTokenRegex =
    Regex.fromString "(\\\\*)([^*])?(\\*+)([^*])?"
        |> Maybe.withDefault Regex.never


findUnderlineEmphasisTokens : String -> List Token
findUnderlineEmphasisTokens str =
    Regex.find underlineEmphasisTokenRegex str
        |> List.filterMap (regMatchToEmphasisToken '_' str)


underlineEmphasisTokenRegex : Regex
underlineEmphasisTokenRegex =
    Regex.fromString "(\\\\*)([^_])?(\\_+)([^_])?"
        |> Maybe.withDefault Regex.never


regMatchToEmphasisToken : Char -> String -> Regex.Match -> Maybe Token
regMatchToEmphasisToken char rawText regMatch =
    case regMatch.submatches of
        maybeBackslashes :: maybeLeftFringe :: (Just delimiter) :: maybeRightFringe :: _ ->
            let
                backslashesLength : Int
                backslashesLength =
                    case maybeBackslashes of
                        Just backslashes ->
                            String.length backslashes

                        Nothing ->
                            0

                leftFringeLength : Int
                leftFringeLength =
                    case maybeLeftFringe of
                        Just left ->
                            String.length left

                        Nothing ->
                            0

                mLeftFringe : Maybe String
                mLeftFringe =
                    if regMatch.index /= 0 && leftFringeLength == 0 then
                        String.slice (regMatch.index - 1) regMatch.index rawText
                            |> Just

                    else
                        maybeLeftFringe

                isEscaped : Bool
                isEscaped =
                    (not (isEven backslashesLength) && leftFringeLength == 0)
                        || (case mLeftFringe of
                                Just "\\" ->
                                    True

                                _ ->
                                    False
                           )

                rFringeRank : Int
                rFringeRank =
                    getFringeRank maybeRightFringe

                lFringeRank : Int
                lFringeRank =
                    if isEscaped then
                        1

                    else
                        getFringeRank mLeftFringe

                delimiterLength : Int
                delimiterLength =
                    if isEscaped then
                        String.length delimiter - 1

                    else
                        String.length delimiter
            in
            if delimiterLength <= 0 || (char == '_' && lFringeRank == 2 && rFringeRank == 2) then
                Nothing

            else
                let
                    index : Int
                    index =
                        regMatch.index
                            + backslashesLength
                            + leftFringeLength
                            + (if isEscaped then
                                1

                               else
                                0
                              )
                in
                Just
                    { index = index
                    , length = delimiterLength
                    , meaning = EmphasisToken char ( lFringeRank, rFringeRank )
                    }

        _ ->
            Nothing


getFringeRank : Maybe String -> Int
getFringeRank mstring =
    case mstring of
        Just string ->
            if String.isEmpty string || containSpace string then
                0

            else if containPunctuation string then
                1

            else
                2

        Nothing ->
            0


containSpace : String -> Bool
containSpace =
    Regex.contains spaceRegex


spaceRegex : Regex
spaceRegex =
    Regex.fromString "\\s"
        |> Maybe.withDefault Regex.never


containPunctuation : String -> Bool
containPunctuation =
    Regex.contains punctuationRegex


punctuationRegex : Regex
punctuationRegex =
    Regex.fromString "[!-#%-\\*,-/:;\\?@\\[-\\]_\\{\\}]"
        |> Maybe.withDefault Regex.never



-- Link & Image Tokens


findLinkImageOpenTokens : String -> List Token
findLinkImageOpenTokens str =
    Regex.find linkImageOpenTokenRegex str
        |> List.filterMap regMatchToLinkImageOpenToken


linkImageOpenTokenRegex : Regex
linkImageOpenTokenRegex =
    Regex.fromString "(\\\\*)(\\!)?(\\[)"
        |> Maybe.withDefault Regex.never


regMatchToLinkImageOpenToken : Regex.Match -> Maybe Token
regMatchToLinkImageOpenToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: maybeImageOpen :: (Just delimiter) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0

                isEscaped =
                    not (isEven backslashesLength)

                meaning =
                    if isEscaped then
                        maybeImageOpen
                            |> Maybe.map
                                (\_ -> LinkOpenToken True)

                    else
                        maybeImageOpen
                            |> Maybe.map
                                (\_ -> ImageOpenToken)
                            |> Maybe.withDefault
                                (LinkOpenToken True)
                            |> Just

                length =
                    if meaning == Just ImageOpenToken then
                        2

                    else
                        1

                index =
                    regMatch.index
                        + backslashesLength
                        + (if
                            isEscaped
                                && (case maybeImageOpen of
                                        Just "!" ->
                                            True

                                        _ ->
                                            False
                                   )
                           then
                            1

                           else
                            0
                          )

                toModel m =
                    { index = index
                    , length = length
                    , meaning = m
                    }
            in
            Maybe.map toModel meaning

        _ ->
            Nothing


findLinkImageCloseTokens : String -> List Token
findLinkImageCloseTokens str =
    Regex.find linkImageCloseTokenRegex str
        |> List.filterMap regMatchToLinkImageCloseToken


linkImageCloseTokenRegex : Regex
linkImageCloseTokenRegex =
    Regex.fromString "(\\\\*)(\\])"
        |> Maybe.withDefault Regex.never


regMatchToLinkImageCloseToken : Regex.Match -> Maybe Token
regMatchToLinkImageCloseToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: (Just delimiter) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0
            in
            if isEven backslashesLength then
                Just
                    { index = regMatch.index + backslashesLength
                    , length = 1
                    , meaning = SquareBracketClose
                    }

            else
                Nothing

        _ ->
            Nothing



-- Angle Brackets Tokens


findAngleBracketRTokens : String -> List Token
findAngleBracketRTokens str =
    Regex.find angleBracketRTokenRegex str
        |> List.filterMap regMatchToAngleBracketRToken


angleBracketRTokenRegex : Regex
angleBracketRTokenRegex =
    Regex.fromString "(\\\\*)(\\>)"
        |> Maybe.withDefault Regex.never


regMatchToAngleBracketRToken : Regex.Match -> Maybe Token
regMatchToAngleBracketRToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: (Just _) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0
            in
            Just
                { index = regMatch.index + backslashesLength
                , length = 1
                , meaning =
                    AngleBracketClose
                        (not (isEven backslashesLength))
                }

        _ ->
            Nothing


findAngleBracketLTokens : String -> List Token
findAngleBracketLTokens str =
    Regex.find angleBracketLTokenRegex str
        |> List.filterMap regMatchToAngleBracketLToken


angleBracketLTokenRegex : Regex
angleBracketLTokenRegex =
    Regex.fromString "(\\\\*)(\\<)"
        |> Maybe.withDefault Regex.never


regMatchToAngleBracketLToken : Regex.Match -> Maybe Token
regMatchToAngleBracketLToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: (Just delimiter) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0
            in
            if isEven backslashesLength then
                Just
                    { index = regMatch.index + backslashesLength
                    , length = 1
                    , meaning = AngleBracketOpen
                    }

            else
                Nothing

        _ ->
            Nothing



-- Hard Break Tokens


findHardBreakTokens : String -> List Token
findHardBreakTokens str =
    if softAsHardLineBreak then
        Regex.find softAsHardLineBreakTokenRegex str
            |> List.filterMap regMatchToSoftHardBreakToken

    else
        Regex.find hardBreakTokenRegex str
            |> List.filterMap regMatchToHardBreakToken


hardBreakTokenRegex : Regex
hardBreakTokenRegex =
    Regex.fromString "(?:(\\\\+)|( {2,}))\\n"
        |> Maybe.withDefault Regex.never


regMatchToHardBreakToken : Regex.Match -> Maybe Token
regMatchToHardBreakToken regMatch =
    case regMatch.submatches of
        (Just backslashes) :: _ ->
            let
                backslashesLength =
                    String.length backslashes
            in
            if not (isEven backslashesLength) then
                { index =
                    regMatch.index
                        + backslashesLength
                        - 1
                , length = 2
                , meaning = HardLineBreakToken
                }
                    |> Just

            else
                Nothing

        _ :: (Just _) :: _ ->
            { index = regMatch.index
            , length = String.length regMatch.match
            , meaning = HardLineBreakToken
            }
                |> Just

        _ ->
            Nothing


softAsHardLineBreakTokenRegex : Regex
softAsHardLineBreakTokenRegex =
    Regex.fromString "(?:(\\\\+)|( *))\\n"
        |> Maybe.withDefault Regex.never


regMatchToSoftHardBreakToken : Regex.Match -> Maybe Token
regMatchToSoftHardBreakToken regMatch =
    case regMatch.submatches of
        (Just backslashes) :: _ ->
            let
                backslashesLength =
                    String.length backslashes
            in
            if isEven backslashesLength then
                { index =
                    regMatch.index
                        + backslashesLength
                , length = 1
                , meaning = HardLineBreakToken
                }
                    |> Just

            else
                { index =
                    regMatch.index
                        + backslashesLength
                        - 1
                , length = 2
                , meaning = HardLineBreakToken
                }
                    |> Just

        _ :: maybeSpaces :: _ ->
            { index = regMatch.index
            , length = String.length regMatch.match
            , meaning = HardLineBreakToken
            }
                |> Just

        _ ->
            Nothing



-- Match


type Match
    = Match MatchModel


type alias MatchModel =
    { type_ : Type
    , start : Int
    , end : Int
    , textStart : Int
    , textEnd : Int
    , text : String
    , matches : List Match
    }


normalMatch : String -> Match
normalMatch text =
    Match
        { type_ = NormalType
        , start = 0
        , end = 0
        , textStart = 0
        , textEnd = 0
        , text = formatStr text
        , matches = []
        }


type Type
    = NormalType
    | HardLineBreakType
    | CodeType
    | AutolinkType ( String, String ) -- ( Text, Url )
    | LinkType ( String, Maybe String ) -- ( Url, Maybe Title )
    | ImageType ( String, Maybe String ) -- ( Src, Maybe Title )
    | HtmlType HtmlModel
    | EmphasisType Int -- Tag length


organizeParserMatches : Parser -> Parser
organizeParserMatches model =
    { matches = organizeMatches model.matches
    , tokens = model.tokens
    , refs = model.refs
    , rawText = model.rawText
    }


organizeMatches : List Match -> List Match
organizeMatches matches =
    case List.sortBy (\(Match match) -> match.start) matches of
        [] ->
            []

        first :: rest ->
            organizeMatchesHelp rest first []


organizeChildren : Match -> Match
organizeChildren (Match match) =
    Match
        { type_ = match.type_
        , start = match.start
        , end = match.end
        , textStart = match.textStart
        , textEnd = match.textEnd
        , text = match.text
        , matches = organizeMatches match.matches
        }


organizeMatchesHelp : List Match -> Match -> List Match -> List Match
organizeMatchesHelp remaining (Match prevMatch) matchesTail =
    -- NOTE: when a match get pushed on the tail, also organize its children
    case remaining of
        [] ->
            organizeChildren (Match prevMatch) :: matchesTail

        (Match match) :: rest ->
            if prevMatch.end <= match.start then
                -- New Match, add it
                organizeMatchesHelp rest (Match match) (organizeChildren (Match prevMatch) :: matchesTail)

            else if prevMatch.start < match.start && prevMatch.end > match.end then
                -- Inside previous Match, merge it
                organizeMatchesHelp rest (addChild prevMatch match) matchesTail

            else
                -- Overlaping previous Match, ignore it
                organizeMatchesHelp rest (Match prevMatch) matchesTail


organizeMatch : Match -> List Match -> List Match
organizeMatch (Match match) matches =
    case matches of
        [] ->
            [ Match match ]

        (Match prevMatch) :: matchesTail ->
            -- New Match
            if prevMatch.end <= match.start then
                Match match :: matches

            else if prevMatch.start < match.start && prevMatch.end > match.end then
                -- Inside previous Match
                addChild prevMatch match
                    :: matchesTail

            else
                -- Overlaping previous Match
                matches


addChild : MatchModel -> MatchModel -> Match
addChild parentMatch childMatch =
    Match
        { parentMatch
            | matches =
                prepareChildMatch parentMatch childMatch
                    :: parentMatch.matches
        }


prepareChildMatch : MatchModel -> MatchModel -> Match
prepareChildMatch parentMatch childMatch =
    { childMatch
        | start = childMatch.start - parentMatch.textStart
        , end = childMatch.end - parentMatch.textStart
        , textStart = childMatch.textStart - parentMatch.textStart
        , textEnd = childMatch.textEnd - parentMatch.textStart
    }
        |> Match



-- Transform Tokens to Matches (TTM)


tokensToMatches : Parser -> Parser
tokensToMatches =
    applyTTM codeAutolinkTypeHtmlTagTTM
        >> applyTTM htmlElementTTM
        >> applyTTM linkImageTypeTTM
        >> applyTTM2 emphasisTTM
        >> applyTTM lineBreakTTM


applyTTM : (List Token -> Parser -> Parser) -> Parser -> Parser
applyTTM finderFunction model =
    let
        newModel =
            { rawText = model.rawText
            , tokens = []
            , matches = model.matches
            , refs = model.refs
            }
    in
    finderFunction model.tokens newModel


applyTTM2 : (List Token -> List Token -> List Match -> References -> String -> Parser) -> Parser -> Parser
applyTTM2 finderFunction model =
    finderFunction model.tokens [] model.matches model.refs model.rawText



-- CodeType spans, HTML tags, and autolinks Tokens To Matches
-- CodeType spans, HTML tags, and autolinks have the same precedence


codeAutolinkTypeHtmlTagTTM : List Token -> Parser -> Parser
codeAutolinkTypeHtmlTagTTM tokens model =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                CodeToken isEscaped ->
                    codeAutolinkTypeHtmlTagTTM tokensTail <|
                        case findToken (isCodeTokenPair token) model.tokens of
                            Just code ->
                                codeToMatch token model code

                            Nothing ->
                                addToken model token

                AngleBracketClose isEscaped ->
                    codeAutolinkTypeHtmlTagTTM tokensTail <|
                        case findToken (\t -> t.meaning == AngleBracketOpen) model.tokens of
                            Just (( openToken, _, remainTokens ) as found) ->
                                case angleBracketsToMatch token isEscaped model found of
                                    Just newModel ->
                                        newModel
                                            |> filterTokens (\t -> t.meaning /= AngleBracketOpen)

                                    Nothing ->
                                        model
                                            |> filterTokens (\t -> t.meaning /= AngleBracketOpen)

                            Nothing ->
                                model
                                    |> filterTokens (\t -> t.meaning /= AngleBracketOpen)

                _ ->
                    codeAutolinkTypeHtmlTagTTM tokensTail (addToken model token)



-- CodeType Helpers


isCodeTokenPair : Token -> Token -> Bool
isCodeTokenPair closeToken openToken =
    case openToken.meaning of
        CodeToken isEscaped ->
            -- If open token is escaped, ignore first '`'
            if isEscaped then
                openToken.length - 1 == closeToken.length

            else
                openToken.length == closeToken.length

        _ ->
            False


codeToMatch : Token -> Parser -> ( Token, List Token, List Token ) -> Parser
codeToMatch closeToken model ( openToken, _, remainTokens ) =
    let
        -- If open token is escaped, ignore first '`'
        updtOpenToken : Token
        updtOpenToken =
            if openToken.meaning == CodeToken True then
                { openToken
                    | index = openToken.index + 1
                    , length = openToken.length - 1
                }

            else
                openToken
    in
    { model
        | matches =
            tokenPairToMatch
                model.refs
                model.rawText
                cleanWhitespaces
                CodeType
                updtOpenToken
                closeToken
                []
                :: model.matches
        , tokens = remainTokens
    }



-- AutolinkTypes & HTML


angleBracketsToMatch : Token -> Bool -> Parser -> ( Token, List Token, List Token ) -> Maybe Parser
angleBracketsToMatch closeToken isEscaped model ( openToken, _, remainTokens ) =
    let
        result =
            tokenPairToMatch model.refs model.rawText (\s -> s) CodeType openToken closeToken []
                |> autolinkToMatch
                |> ifError emailAutolinkTypeToMatch
    in
    case result of
        Result.Err tempMatch ->
            if not isEscaped then
                case htmlToToken model.rawText tempMatch of
                    Just newToken ->
                        Just
                            { tokens = newToken :: remainTokens
                            , matches = model.matches
                            , refs = model.refs
                            , rawText = model.rawText
                            }

                    Nothing ->
                        Nothing

            else
                Nothing

        Result.Ok newMatch ->
            Just
                { matches = newMatch :: model.matches
                , tokens = remainTokens
                , refs = model.refs
                , rawText = model.rawText
                }



-- AutolinkType Helpers


autolinkToMatch : Match -> Result Match Match
autolinkToMatch (Match match) =
    if Regex.contains urlRegex match.text then
        { match
            | type_ =
                AutolinkType ( match.text, encodeUrl match.text )
        }
            |> Match
            |> Result.Ok

    else
        Result.Err (Match match)



-- From http://spec.commonmark.org/dingus/commonmark.js


urlRegex : Regex
urlRegex =
    Regex.fromString "^([A-Za-z][A-Za-z0-9.+\\-]{1,31}:[^<>\\x00-\\x20]*)$"
        |> Maybe.withDefault Regex.never


emailAutolinkTypeToMatch : Match -> Result Match Match
emailAutolinkTypeToMatch (Match match) =
    if Regex.contains emailRegex match.text then
        { match
            | type_ =
                AutolinkType ( match.text, "mailto:" ++ encodeUrl match.text )
        }
            |> Match
            |> Result.Ok

    else
        Result.Err (Match match)



-- From http://spec.commonmark.org/dingus/commonmark.js


emailRegex : Regex
emailRegex =
    Regex.fromString "^([a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~\\-]+@[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*)$"
        |> Maybe.withDefault Regex.never



-- Html Helpers


type alias HtmlModel =
    HtmlParser.Node



--{ tag : String
--, attributes : List Attribute
--}


softAsHardLineBreak =
    False


htmlToToken : String -> Match -> Maybe Token
htmlToToken rawText (Match match) =
    let
        consumedCharacters =
            Advanced.succeed
                (\startOffset htmlTag endOffset ->
                    { length = endOffset - startOffset
                    , htmlTag = htmlTag
                    }
                )
                |= Advanced.getOffset
                |= HtmlParser.html
                |= Advanced.getOffset

        parsed =
            rawText
                |> String.dropLeft match.start
                |> Advanced.run consumedCharacters
    in
    case parsed of
        Ok { htmlTag, length } ->
            let
                htmlToken =
                    HtmlToken False htmlTag
            in
            Just
                { index = match.start
                , length = length
                , meaning = htmlToken
                }

        Err error ->
            Nothing


htmlElementTTM : List Token -> Parser -> Parser
htmlElementTTM tokens model =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                HtmlToken isOpen htmlModel ->
                    if isVoidTag htmlModel || not isOpen then
                        htmlElementTTM tokensTail
                            (tokenToMatch token (HtmlType htmlModel)
                                |> addMatch model
                            )

                    else
                        case findToken (isCloseToken htmlModel) tokensTail of
                            Nothing ->
                                htmlElementTTM tokensTail (addMatch model (tokenToMatch token (HtmlType htmlModel)))

                            Just ( closeToken, innerTokens, newTail ) ->
                                let
                                    newMatch =
                                        tokenPairToMatch
                                            model.refs
                                            model.rawText
                                            (\s -> s)
                                            (HtmlType htmlModel)
                                            token
                                            closeToken
                                            innerTokens
                                in
                                htmlElementTTM newTail (addMatch model newMatch)

                _ ->
                    htmlElementTTM tokensTail (addToken model token)


isVoidTag : HtmlModel -> Bool
isVoidTag htmlModel =
    -- TODO should I use this later?
    False



--List.member htmlModel.tag voidHtmlTags


voidHtmlTags : List String
voidHtmlTags =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "keygen"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]


isCloseToken : HtmlModel -> Token -> Bool
isCloseToken htmlModel token =
    --case token.meaning of
    --    HtmlToken False htmlModel_ ->
    --        htmlModel.tag == htmlModel_.tag
    --
    --    _ ->
    False



-- LinkType and images Tokens To Matches
-- LinkType, reference link and images have precedence over emphasis


linkImageTypeTTM : List Token -> Parser -> Parser
linkImageTypeTTM tokens model =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                SquareBracketClose ->
                    case findToken isLinkTypeOrImageOpenToken model.tokens of
                        Just found ->
                            case linkOrImageTypeToMatch token tokensTail model found of
                                Just ( x, y ) ->
                                    linkImageTypeTTM x y

                                Nothing ->
                                    linkImageTypeTTM tokensTail model

                        Nothing ->
                            linkImageTypeTTM tokensTail model

                _ ->
                    linkImageTypeTTM tokensTail (addToken model token)


isLinkTypeOrImageOpenToken : Token -> Bool
isLinkTypeOrImageOpenToken token =
    case token.meaning of
        LinkOpenToken _ ->
            True

        ImageOpenToken ->
            True

        _ ->
            False


linkOrImageTypeToMatch : Token -> List Token -> Parser -> ( Token, List Token, List Token ) -> Maybe ( List Token, Parser )
linkOrImageTypeToMatch closeToken tokensTail model ( openToken, innerTokens, remainTokens ) =
    let
        remainText : String
        remainText =
            String.dropLeft (closeToken.index + 1) model.rawText

        findTempMatch : Bool -> Match
        findTempMatch isLinkType =
            tokenPairToMatch
                model.refs
                model.rawText
                (\s -> s)
                (if isLinkType then
                    LinkType ( "", Nothing )

                 else
                    ImageType ( "", Nothing )
                )
                openToken
                closeToken
                (List.reverse innerTokens)

        removeOpenToken : ( List Token, Parser )
        removeOpenToken =
            ( tokensTail
            , { model | tokens = innerTokens ++ remainTokens }
            )

        inactivateLinkOpenToken : Token -> Token
        inactivateLinkOpenToken token =
            case token.meaning of
                LinkOpenToken _ ->
                    { token | meaning = LinkOpenToken False }

                _ ->
                    token

        linkOpenTokenToInactive : Parser -> Parser
        linkOpenTokenToInactive model_ =
            { model_ | tokens = List.map inactivateLinkOpenToken model_.tokens }

        initModel =
            { tokens = remainTokens
            , matches = model.matches
            , refs = model.refs
            , rawText = model.rawText
            }
    in
    case openToken.meaning of
        ImageOpenToken ->
            let
                tempMatch =
                    findTempMatch False
            in
            case checkForInlineLinkTypeOrImageType remainText tempMatch model.refs of
                Nothing ->
                    Just removeOpenToken

                Just match ->
                    case checkParsedAheadOverlapping (addMatch initModel match) of
                        Ok v ->
                            Just (removeParsedAheadTokens tokensTail v)

                        Err _ ->
                            Just removeOpenToken

        -- Active opening: set all before to inactive if found
        LinkOpenToken True ->
            let
                tempMatch =
                    findTempMatch True
            in
            (case checkForInlineLinkTypeOrImageType remainText tempMatch model.refs of
                Nothing ->
                            Just removeOpenToken

                Just match ->
                    case checkParsedAheadOverlapping (addMatch initModel match) of
                        Ok v -> 
                            v 
                                |> linkOpenTokenToInactive
                                |> removeParsedAheadTokens tokensTail
                                |> Just

                        Err _ -> 
                            Just removeOpenToken
            )


        -- Inactive opening: just remove open and close tokens
        LinkOpenToken False ->
            Just removeOpenToken

        _ ->
            Nothing



-- Check if is overlapping previous parsed matches (code, html or autolink)


checkParsedAheadOverlapping : Parser -> Result () Parser
checkParsedAheadOverlapping parser =
    case parser.matches of
        [] ->
            Result.Err ()

        (Match match) :: remainMatches ->
            let
                overlappingMatches : List Match
                overlappingMatches =
                    List.filter
                        (\(Match testMatch) ->
                            match.end
                                > testMatch.start
                                && match.end
                                < testMatch.end
                        )
                        remainMatches
            in
            if
                List.isEmpty remainMatches
                    || List.isEmpty overlappingMatches
            then
                Result.Ok parser

            else
                Result.Err ()



-- Remove tokens inside the parsed ahead regex match


removeParsedAheadTokens : List Token -> Parser -> ( List Token, Parser )
removeParsedAheadTokens tokensTail parser =
    case parser.matches of
        [] ->
            ( tokensTail, parser )

        (Match match) :: _ ->
            ( List.filter
                (\token -> token.index >= match.end)
                tokensTail
            , parser
            )



-- Inline link or image
{-
   checkForInlineLinkTypeOrImageType : String -> Match -> Parser -> Result ( String, Match, Parser ) Parser
   checkForInlineLinkTypeOrImageType remainText (Match tempMatch) model =
       Regex.findAtMost 1 inlineLinkTypeOrImageTypeRegex remainText
           |> List.head
           |> Maybe.andThen (inlineLinkTypeOrImageTypeRegexToMatch tempMatch model)
           |> Maybe.map (addMatch model)
           |> Result.fromMaybe ( remainText, Match tempMatch, model )
-}


checkForInlineLinkTypeOrImageType : String -> Match -> References -> Maybe Match
checkForInlineLinkTypeOrImageType remainText (Match tempMatch) refs =
    let
        foo : References -> Maybe Match
        foo references =
            let
                matches =
                    Regex.findAtMost 1 refLabelRegex remainText
            in
            refRegexToMatch tempMatch references (List.head matches)
    in
    case Regex.findAtMost 1 inlineLinkTypeOrImageTypeRegex remainText of
        first :: _ ->
            case inlineLinkTypeOrImageTypeRegexToMatch tempMatch first of
                Just match ->
                    Just match

                Nothing ->
                    foo refs

        [] ->
            foo refs


inlineLinkTypeOrImageTypeRegex : Regex
inlineLinkTypeOrImageTypeRegex =
    Regex.fromString ("^\\(\\s*" ++ hrefRegex ++ titleRegex ++ "\\s*\\)")
        |> Maybe.withDefault Regex.never


hrefRegex : String
hrefRegex =
    "(?:<([^<>"
        ++ lineEndChars
        ++ "]*)>|([^"
        ++ whiteSpaceChars
        ++ "\\(\\)\\\\]*(?:\\\\.[^"
        ++ whiteSpaceChars
        ++ "\\(\\)\\\\]*)*))"


inlineLinkTypeOrImageTypeRegexToMatch : MatchModel -> Regex.Match -> Maybe Match
inlineLinkTypeOrImageTypeRegexToMatch matchModel regexMatch =
    case regexMatch.submatches of
        maybeRawUrlAngleBrackets :: maybeRawUrlWithoutBrackets :: maybeTitleSingleQuotes :: maybeTitleDoubleQuotes :: maybeTitleParenthesis :: _ ->
            let
                maybeRawUrl : Maybe String
                maybeRawUrl =
                    returnFirstJust
                        [ maybeRawUrlAngleBrackets
                        , maybeRawUrlWithoutBrackets
                        ]

                maybeTitle : Maybe String
                maybeTitle =
                    returnFirstJust
                        [ maybeTitleSingleQuotes
                        , maybeTitleDoubleQuotes
                        , maybeTitleParenthesis
                        ]

                toMatch : String -> Match
                toMatch rawUrl =
                    { matchModel
                        | type_ =
                            prepareUrlAndTitle rawUrl maybeTitle
                                |> (case matchModel.type_ of
                                        ImageType _ ->
                                            ImageType

                                        _ ->
                                            LinkType
                                   )
                        , end = matchModel.end + String.length regexMatch.match
                    }
                        |> Match
            in
            maybeRawUrl
                |> Maybe.withDefault ""
                |> toMatch
                |> Just

        _ ->
            Nothing


prepareUrlAndTitle : String -> Maybe String -> ( String, Maybe String )
prepareUrlAndTitle rawUrl maybeTitle =
    ( encodeUrl (formatStr rawUrl)
    , Maybe.map formatStr maybeTitle
    )



-- Reference link or image


refLabelRegex : Regex
refLabelRegex =
    Regex.fromString ("^\\[\\s*(" ++ insideSquareBracketRegex ++ ")\\s*\\]")
        |> Maybe.withDefault Regex.never


refRegexToMatch : MatchModel -> References -> Maybe Regex.Match -> Maybe Match
refRegexToMatch matchModel references maybeRegexMatch =
    let
        refLabel : String
        refLabel =
            maybeRegexMatch
                |> Maybe.andThen (.submatches >> List.head)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault matchModel.text
                |> (\str ->
                        if String.isEmpty str then
                            matchModel.text

                        else
                            str
                   )
    in
    case Dict.get (prepareRefLabel refLabel) references of
        Nothing ->
            Nothing

        Just ( rawUrl, maybeTitle ) ->
            let
                regexMatchLength : Int
                regexMatchLength =
                    case maybeRegexMatch of
                        Just { match } ->
                            String.length match

                        Nothing ->
                            0

                type_ =
                    case matchModel.type_ of
                        ImageType _ ->
                            ImageType (prepareUrlAndTitle rawUrl maybeTitle)

                        _ ->
                            LinkType (prepareUrlAndTitle rawUrl maybeTitle)
            in
            Just <|
                Match
                    { matchModel
                        | type_ = type_
                        , end = matchModel.end + regexMatchLength
                    }


encodeUrl : String -> String
encodeUrl =
    Url.percentEncode
        >> Regex.replace
            decodeUrlRegex
            (\match ->
                Url.percentDecode match.match
                    |> Maybe.withDefault match.match
            )



-- Decode the following chars: ;,/?:@&=+$#%


decodeUrlRegex : Regex
decodeUrlRegex =
    Regex.fromString "%(?:3B|2C|2F|3F|3A|40|26|3D|2B|24|23|25)"
        |> Maybe.withDefault Regex.never



-- EmphasisType Tokens To Matches


emphasisTTM : List Token -> List Token -> List Match -> References -> String -> Parser
emphasisTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            { tokens = List.reverse tokens, matches = matches, refs = references, rawText = rawText }

        token :: tokensTail ->
            case token.meaning of
                EmphasisToken char ( leftRank, rightRank ) ->
                    -- Close or opening token
                    if leftRank == rightRank then
                        -- If 1) is not surrounded by whitespace and
                        --    2) is not '_' or is surronded by puntuaction
                        -- is a close or opening tag
                        if rightRank /= 0 && (char /= '_' || rightRank == 1) then
                            -- Search for opening tag and add
                            -- match if the sum of lengths
                            -- is not multiple of 3, otherwise add
                            -- opening tag
                            case findToken (isOpenEmphasisToken token) tokens of
                                Just found ->
                                    case emphasisToMatch references rawText token tokensTail found of
                                        ( x, y, z ) ->
                                            emphasisTTM x z (y :: matches) references rawText

                                Nothing ->
                                    emphasisTTM tokensTail (token :: tokens) matches references rawText

                        else
                            emphasisTTM tokensTail tokens matches references rawText

                    else if leftRank < rightRank then
                        -- Opening token
                        emphasisTTM tokensTail (token :: tokens) matches references rawText

                    else
                        -- Closing token
                        case findToken (isOpenEmphasisToken token) tokens of
                            Just found ->
                                case emphasisToMatch references rawText token tokensTail found of
                                    ( x, y, z ) ->
                                        emphasisTTM x z (y :: matches) references rawText

                            Nothing ->
                                emphasisTTM tokensTail tokens matches references rawText

                _ ->
                    emphasisTTM tokensTail (token :: tokens) matches references rawText


isOpenEmphasisToken : Token -> Token -> Bool
isOpenEmphasisToken closeToken openToken =
    case openToken.meaning of
        EmphasisToken openChar ( openLR, openRR ) ->
            case closeToken.meaning of
                EmphasisToken closeChar ( closeLR, closeRR ) ->
                    if openChar == closeChar then
                        if openLR == openRR || closeLR == closeRR then
                            -- if the sum of lengths
                            -- is not multiple of 3
                            -- is Open emphasis
                            modBy 3 (closeToken.length + openToken.length) /= 0

                        else
                            True

                    else
                        False

                _ ->
                    False

        _ ->
            False


emphasisToMatch : References -> String -> Token -> List Token -> ( Token, List Token, List Token ) -> ( List Token, Match, List Token )
emphasisToMatch references rawText closeToken tokensTail ( openToken, innerTokens, remainTokens ) =
    let
        remainLength : Int
        remainLength =
            openToken.length - closeToken.length

        updt =
            if remainLength == 0 then
                -- Perfect match
                { openToken = openToken
                , closeToken = closeToken
                , remainTokens = remainTokens
                , tokensTail = tokensTail
                }

            else if remainLength > 0 then
                -- Still has opening token
                { openToken =
                    { openToken
                        | index = openToken.index + remainLength
                        , length = closeToken.length
                    }
                , closeToken = closeToken
                , remainTokens =
                    { openToken | length = remainLength }
                        :: remainTokens
                , tokensTail = tokensTail
                }

            else
                -- Still has closing token
                { openToken = openToken
                , closeToken = { closeToken | length = openToken.length }
                , remainTokens = remainTokens
                , tokensTail =
                    { closeToken
                        | index = closeToken.index + openToken.length
                        , length = -remainLength
                    }
                        :: tokensTail
                }

        match : Match
        match =
            tokenPairToMatch
                references
                rawText
                (\s -> s)
                (EmphasisType updt.openToken.length)
                updt.openToken
                updt.closeToken
                (List.reverse innerTokens)
    in
    ( updt.tokensTail
    , match
    , updt.remainTokens
    )



-- Line Break Tokens To Matches


lineBreakTTM : List Token -> Parser -> Parser
lineBreakTTM tokens model =
    lineBreakTTMHelp tokens model model.tokens model.matches


lineBreakTTMHelp remaining model tokens matches =
    case remaining of
        [] ->
            { tokens = List.reverse tokens
            , matches = matches
            , refs = model.refs
            , rawText = model.rawText
            }

        token :: tokensTail ->
            if token.meaning == HardLineBreakToken then
                -- NOTE: the origiginal also moved into this branch when
                -- (token.meaning == SoftLineBreakToken && softAsHardLineBreak
                lineBreakTTMHelp tokensTail model tokens (tokenToMatch token HardLineBreakType :: matches)

            else
                lineBreakTTMHelp tokensTail model (token :: tokens) matches



-- Matches to Inline


matchesToInlines : List Match -> List Inline
matchesToInlines matches =
    List.map matchToInline matches


matchToInline : Match -> Inline
matchToInline (Match match) =
    case match.type_ of
        NormalType ->
            Text match.text

        HardLineBreakType ->
            HardLineBreak

        CodeType ->
            CodeInline match.text

        AutolinkType ( text, url ) ->
            Link url Nothing [ Text text ]

        LinkType ( url, maybeTitle ) ->
            Link url
                maybeTitle
                (matchesToInlines match.matches)

        ImageType ( url, maybeTitle ) ->
            Image url
                maybeTitle
                (matchesToInlines match.matches)

        HtmlType model ->
            HtmlInline model

        EmphasisType length ->
            Emphasis length
                (matchesToInlines match.matches)



-- Helpers


{-| Apply a function to every inline recursively.

Example of converting all text in **headings** to **ALL CAPS**:

    import Html exposing (Html, article)
    import Markdown.Block as Block exposing (Block(..))
    import Markdown.Inline as Inline exposing (Inline(..))

    view : Html msg
    view =
        myMarkdownString
            |> Block.parse Nothing
            |> List.map (Block.walk modHeader)
            |> List.map Block.toHtml
            |> List.concat
            |> article []

    modHeader : Block b i -> Block b i
    modHeader block =
        case block of
            Heading rawText level inlines ->
                List.map (Inline.walk upperText) inlines
                    |> Heading rawText level

            _ ->
                block

    upperText : Inline -> Inline
    upperText inline =
        case inline of
            Text str ->
                Text (String.toUpper str)

            _ ->
                inline

**Note:** In this example, `Block.walkInlines` could be used instead.

-}
walk : (Inline -> Inline) -> Inline -> Inline
walk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (walk function) inlines
                |> Link url maybeTitle
                |> function

        Image url maybeTitle inlines ->
            List.map (walk function) inlines
                |> Image url maybeTitle
                |> function

        HtmlInline html ->
            --List.map (walk function) inlines
            --    |> HtmlInline tag attrs
            function inline

        Emphasis length inlines ->
            List.map (walk function) inlines
                |> Emphasis length
                |> function

        _ ->
            function inline


query : (Inline -> List a) -> Inline -> List a
query function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (query function) inlines
                |> List.concat
                |> (++) (function (Link url maybeTitle inlines))

        Image url maybeTitle inlines ->
            List.map (query function) inlines
                |> List.concat
                |> (++) (function (Image url maybeTitle inlines))

        HtmlInline html ->
            --List.map (query function) inlines
            --    |> List.concat
            --    |> (++) (function (HtmlInline tag attrs inlines))
            function inline

        Emphasis length inlines ->
            List.map (query function) inlines
                |> List.concat
                |> (++) (function (Emphasis length inlines))

        _ ->
            function inline
