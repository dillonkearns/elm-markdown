module Markdown.InlineParser exposing (parse, query, tokenize, walk)

import Dict
import HtmlParser
import Markdown.Helpers exposing (References, cleanWhitespaces, formatStr, ifError, insideSquareBracketRegex, isEven, lineEndChars, prepareRefLabel, returnFirstJust, titleRegex, whiteSpaceChars)
import Markdown.Inline exposing (Inline(..))
import Parser.Advanced as Advanced exposing ((|=))
import Regex exposing (Regex)
import Url



-- Parser


parse : References -> String -> List Inline
parse refs rawText_ =
    let
        rawText =
            String.trim rawText_

        tokens =
            tokenize rawText
    in
    tokensToMatches tokens [] refs rawText
        |> organizeMatches
        |> parseTextMatches rawText []
        |> matchesToInlines


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


type Escaped
    = Escaped
    | NotEscaped


type Active
    = Active
    | Inactive


type Opening
    = Opening
    | NotOpening


type Meaning
    = CodeToken Escaped
    | LinkOpenToken Active
    | ImageOpenToken
    | SquareBracketClose
    | AngleBracketOpen
    | AngleBracketClose Escaped
    | HtmlToken Opening HtmlModel
    | EmphasisToken Char { leftFringeRank : Int, rightFringeRank : Int }
    | SoftLineBreakToken
    | HardLineBreakToken
    | StrikethroughToken Escaped


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
            tokensToMatches innerTokens [] references rawText
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
    Match
        { type_ = type_
        , start = token.index
        , end = token.index + token.length
        , textStart = 0
        , textEnd = 0
        , text = ""
        , matches = []
        }



-- Find Tokens
-- Scan all tokens from the string


tokenize : String -> List Token
tokenize rawText =
    findCodeTokens rawText
        |> mergeByIndex (findAsteriskEmphasisTokens rawText)
        |> mergeByIndex (findUnderlineEmphasisTokens rawText)
        |> mergeByIndex (findStrikethroughTokens rawText)
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
                , meaning =
                    if isEven backslashesLength then
                        CodeToken NotEscaped

                    else
                        CodeToken Escaped
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
        -- NOTE: the fringes are always at most 1 character!
        -- Therefore we can use simple matching functions. Regex has too much overhead.
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
                    , meaning = EmphasisToken char { leftFringeRank = lFringeRank, rightFringeRank = rFringeRank }
                    }

        _ ->
            Nothing

-- Strikethrough Tokens


findStrikethroughTokens : String -> List Token
findStrikethroughTokens str =
    Regex.find strikethroughTokenRegex str
        |> List.filterMap regMatchToStrikethroughToken


strikethroughTokenRegex : Regex
strikethroughTokenRegex =
    Regex.fromString "(\\\\*)(~{2,})([^~])?"
        |> Maybe.withDefault Regex.never


regMatchToStrikethroughToken : Regex.Match -> Maybe Token
regMatchToStrikethroughToken regMatch =
    case regMatch.submatches of
        maybeBackslashes :: (Just tilde) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0

                (length, meaning) =
                  if isEven backslashesLength then
                      (String.length tilde, StrikethroughToken NotEscaped)

                  else
                      (String.length tilde, StrikethroughToken Escaped)
            in
            Just
                { index = regMatch.index + backslashesLength
                , length = length
                , meaning = meaning
                }

        _ ->
            Nothing





{-| Whitespace characters matched by the `\\s` regex
-}
isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' ' ->
            True

        '\u{000C}' ->
            True

        '\n' ->
            True

        '\r' ->
            True

        '\t' ->
            True

        '\u{000B}' ->
            True

        '\u{00A0}' ->
            True

        '\u{2028}' ->
            True

        '\u{2029}' ->
            True

        _ ->
            False


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
    -- NOTE: this is faster than a `\\s` regex for small strings
    String.foldl (\c accum -> accum || isWhitespace c) False


{-| Punctuation as matched by the regex `[!-#%-\\*,-/:;\\?@\\[-\\]_\\{\\}~]`
-}
containPunctuation : String -> Bool
containPunctuation =
    -- NOTE: a foldl over a 1-char string is much faster than String.uncons
    -- because of the allocation cost
    String.foldl (\c accum -> accum || isPunctuation c) False


isPunctuation : Char -> Bool
isPunctuation c =
    case c of
        -- 33 - 35
        '!' ->
            True

        '"' ->
            True

        '#' ->
            True

        -- 37 - 42
        '%' ->
            True

        '&' ->
            True

        '\'' ->
            True

        '(' ->
            True

        ')' ->
            True

        '*' ->
            True

        -- 44 - 47
        ',' ->
            True

        '-' ->
            True

        '.' ->
            True

        '/' ->
            True

        -- 58 - 59
        ':' ->
            True

        ';' ->
            True

        -- 63 - 64
        '?' ->
            True

        '@' ->
            True

        -- 91, 93
        '[' ->
            True

        ']' ->
            True

        -- 95
        '_' ->
            True

        -- 123, 125
        '{' ->
            True

        '}' ->
            True

        -- 126
        '~' ->
            True

        _ ->
            False



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
        maybeBackslashes :: maybeImageOpen :: (Just _) :: _ ->
            let
                backslashesLength =
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0

                isEscaped =
                    not (isEven backslashesLength)

                index =
                    if isEscaped then
                        regMatch.index + backslashesLength + 1

                    else
                        regMatch.index + backslashesLength
            in
            if isEscaped then
                case maybeImageOpen of
                    Just _ ->
                        Just
                            { index = index
                            , length = 1
                            , meaning = LinkOpenToken Active
                            }

                    Nothing ->
                        Nothing

            else
                case maybeImageOpen of
                    Just _ ->
                        Just
                            { index = index
                            , length = 2
                            , meaning = ImageOpenToken
                            }

                    Nothing ->
                        Just
                            { index = index
                            , length = 1
                            , meaning = LinkOpenToken Active
                            }

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
        maybeBackslashes :: (Just _) :: _ ->
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
                    if isEven backslashesLength then
                        AngleBracketClose NotEscaped

                    else
                        AngleBracketClose Escaped
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
    | StrikethroughType


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


addChild : MatchModel -> MatchModel -> Match
addChild parentMatch childMatch =
    Match
        { matches = prepareChildMatch parentMatch childMatch :: parentMatch.matches

        -- copy other fields
        , start = parentMatch.start
        , end = parentMatch.end
        , textStart = parentMatch.textStart
        , textEnd = parentMatch.textEnd
        , type_ = parentMatch.type_
        , text = parentMatch.text
        }


prepareChildMatch : MatchModel -> MatchModel -> Match
prepareChildMatch parentMatch childMatch =
    Match
        { start = childMatch.start - parentMatch.textStart
        , end = childMatch.end - parentMatch.textStart
        , textStart = childMatch.textStart - parentMatch.textStart
        , textEnd = childMatch.textEnd - parentMatch.textStart

        -- copy other fiels
        , type_ = childMatch.type_
        , text = childMatch.text
        , matches = childMatch.matches
        }



-- Transform Tokens to Matches (TTM)


tokensToMatches : List Token -> List Match -> References -> String -> List Match
tokensToMatches tokens matches references rawText =
    codeAutolinkTypeHtmlTagTTM tokens [] matches references rawText



-- CodeType spans, HTML tags, and autolinks Tokens To Matches
-- CodeType spans, HTML tags, and autolinks have the same precedence


codeAutolinkTypeHtmlTagTTM : List Token -> List Token -> List Match -> References -> String -> List Match
codeAutolinkTypeHtmlTagTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            htmlElementTTM (List.reverse tokens) [] matches references rawText

        token :: tokensTail ->
            case token.meaning of
                CodeToken isEscaped ->
                    case findToken (isCodeTokenPair token) tokens of
                        Just code ->
                            let
                                ( newTokens, newMatches ) =
                                    codeToMatch token matches references rawText code
                            in
                            codeAutolinkTypeHtmlTagTTM tokensTail newTokens newMatches references rawText

                        Nothing ->
                            codeAutolinkTypeHtmlTagTTM tokensTail (token :: tokens) matches references rawText

                AngleBracketClose isEscaped ->
                    let
                        isAngleBracketOpen { meaning } =
                            case meaning of
                                AngleBracketOpen ->
                                    True

                                _ ->
                                    False
                    in
                    case findToken isAngleBracketOpen tokens of
                        Just found ->
                            case angleBracketsToMatch token isEscaped matches references rawText found of
                                Just ( newTokens, newMatches ) ->
                                    codeAutolinkTypeHtmlTagTTM tokensTail
                                        (List.filter (not << isAngleBracketOpen) newTokens)
                                        newMatches
                                        references
                                        rawText

                                Nothing ->
                                    codeAutolinkTypeHtmlTagTTM tokensTail
                                        (List.filter (not << isAngleBracketOpen) tokens)
                                        matches
                                        references
                                        rawText

                        Nothing ->
                            codeAutolinkTypeHtmlTagTTM tokensTail
                                (List.filter (not << isAngleBracketOpen) tokens)
                                matches
                                references
                                rawText

                _ ->
                    codeAutolinkTypeHtmlTagTTM tokensTail (token :: tokens) matches references rawText



-- CodeType Helpers


isCodeTokenPair : Token -> Token -> Bool
isCodeTokenPair closeToken openToken =
    case openToken.meaning of
        -- If open token is escaped, ignore first '`'
        CodeToken Escaped ->
            openToken.length - 1 == closeToken.length

        CodeToken NotEscaped ->
            openToken.length == closeToken.length

        _ ->
            False


codeToMatch : Token -> List Match -> References -> String -> ( Token, List Token, List Token ) -> ( List Token, List Match )
codeToMatch closeToken matches references rawText ( openToken, _, remainTokens ) =
    let
        -- If open token is escaped, ignore first '`'
        updatedOpenToken : Token
        updatedOpenToken =
            case openToken.meaning of
                CodeToken Escaped ->
                    { openToken
                        | index = openToken.index + 1
                        , length = openToken.length - 1
                    }

                _ ->
                    openToken

        match =
            tokenPairToMatch
                references
                rawText
                cleanWhitespaces
                CodeType
                updatedOpenToken
                closeToken
                []
    in
    ( remainTokens
    , match :: matches
    )



-- AutolinkTypes & HTML


angleBracketsToMatch : Token -> Escaped -> List Match -> References -> String -> ( Token, List Token, List Token ) -> Maybe ( List Token, List Match )
angleBracketsToMatch closeToken escaped matches references rawText ( openToken, _, remainTokens ) =
    let
        result =
            tokenPairToMatch references rawText (\s -> s) CodeType openToken closeToken []
                |> autolinkToMatch
                |> ifError emailAutolinkTypeToMatch
    in
    case result of
        Result.Err tempMatch ->
            case escaped of
                NotEscaped ->
                    case htmlToToken rawText tempMatch of
                        Just newToken ->
                            Just ( newToken :: remainTokens, matches )

                        Nothing ->
                            Nothing

                Escaped ->
                    Nothing

        Result.Ok newMatch ->
            Just ( remainTokens, newMatch :: matches )



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
                    HtmlToken NotOpening htmlTag
            in
            Just
                { index = match.start
                , length = length
                , meaning = htmlToken
                }

        Err _ ->
            Nothing


htmlElementTTM : List Token -> List Token -> List Match -> References -> String -> List Match
htmlElementTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            linkImageTypeTTM (List.reverse tokens) [] matches references rawText

        token :: tokensTail ->
            case token.meaning of
                HtmlToken isOpen htmlModel ->
                    -- NOTE: should we also entered this branch for void tags?
                    case isOpen of
                        NotOpening ->
                            htmlElementTTM tokensTail
                                tokens
                                (tokenToMatch token (HtmlType htmlModel) :: matches)
                                references
                                rawText

                        Opening ->
                            case findToken (isCloseToken htmlModel) tokensTail of
                                Nothing ->
                                    htmlElementTTM tokensTail tokens (tokenToMatch token (HtmlType htmlModel) :: matches) references rawText

                                Just ( closeToken, innerTokens, newTail ) ->
                                    let
                                        newMatch =
                                            tokenPairToMatch
                                                references
                                                rawText
                                                (\s -> s)
                                                (HtmlType htmlModel)
                                                token
                                                closeToken
                                                innerTokens
                                    in
                                    htmlElementTTM newTail tokens (newMatch :: matches) references rawText

                _ ->
                    htmlElementTTM tokensTail (token :: tokens) matches references rawText


isVoidTag : HtmlModel -> Bool
isVoidTag htmlModel =
    -- TODO should I use this later?
    --List.member htmlModel.tag voidHtmlTags
    False


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


linkImageTypeTTM : List Token -> List Token -> List Match -> References -> String -> List Match
linkImageTypeTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            emphasisTTM (List.reverse tokens) [] matches references rawText

        token :: tokensTail ->
            case token.meaning of
                SquareBracketClose ->
                    case findToken isLinkTypeOrImageOpenToken tokens of
                        Just found ->
                            case linkOrImageTypeToMatch token tokensTail matches references rawText found of
                                Just ( x, newMatches, newTokens ) ->
                                    linkImageTypeTTM x newTokens newMatches references rawText

                                Nothing ->
                                    linkImageTypeTTM tokensTail tokens matches references rawText

                        Nothing ->
                            linkImageTypeTTM tokensTail tokens matches references rawText

                _ ->
                    linkImageTypeTTM tokensTail (token :: tokens) matches references rawText


isLinkTypeOrImageOpenToken : Token -> Bool
isLinkTypeOrImageOpenToken token =
    case token.meaning of
        LinkOpenToken _ ->
            True

        ImageOpenToken ->
            True

        _ ->
            False


linkOrImageTypeToMatch : Token -> List Token -> List Match -> References -> String -> ( Token, List Token, List Token ) -> Maybe ( List Token, List Match, List Token )
linkOrImageTypeToMatch closeToken tokensTail oldMatches references rawText ( openToken, innerTokens, remainTokens ) =
    let
        remainText : String
        remainText =
            String.dropLeft (closeToken.index + 1) rawText

        findTempMatch : Bool -> Match
        findTempMatch isLinkType =
            tokenPairToMatch
                references
                rawText
                (\s -> s)
                (if isLinkType then
                    LinkType ( "", Nothing )

                 else
                    ImageType ( "", Nothing )
                )
                openToken
                closeToken
                (List.reverse innerTokens)

        removeOpenToken : ( List Token, List Match, List Token )
        removeOpenToken =
            ( tokensTail
            , oldMatches
            , innerTokens ++ remainTokens
            )

        inactivateLinkOpenToken : Token -> Token
        inactivateLinkOpenToken token =
            case token.meaning of
                LinkOpenToken _ ->
                    { token | meaning = LinkOpenToken Inactive }

                _ ->
                    token
    in
    case openToken.meaning of
        ImageOpenToken ->
            let
                tempMatch =
                    findTempMatch False
            in
            case checkForInlineLinkTypeOrImageType remainText tempMatch references of
                Nothing ->
                    Just removeOpenToken

                Just match ->
                    case checkParsedAheadOverlapping match oldMatches of
                        Just matches ->
                            Just ( removeParsedAheadTokens match tokensTail, matches, remainTokens )

                        Nothing ->
                            Just removeOpenToken

        -- Active opening: set all before to inactive if found
        LinkOpenToken Active ->
            let
                tempMatch =
                    findTempMatch True
            in
            case checkForInlineLinkTypeOrImageType remainText tempMatch references of
                Nothing ->
                    Just removeOpenToken

                Just match ->
                    case checkParsedAheadOverlapping match oldMatches of
                        Just matches ->
                            Just ( removeParsedAheadTokens match tokensTail, matches, List.map inactivateLinkOpenToken remainTokens )

                        Nothing ->
                            Just removeOpenToken

        -- Inactive opening: just remove open and close tokens
        LinkOpenToken Inactive ->
            Just removeOpenToken

        _ ->
            Nothing



-- Check if is overlapping previous parsed matches (code, html or autolink)


checkParsedAheadOverlapping : Match -> List Match -> Maybe (List Match)
checkParsedAheadOverlapping (Match match) remainMatches =
    let
        overlappingMatches : List Match -> List Match
        overlappingMatches =
            List.filter (\(Match testMatch) -> match.end > testMatch.start && match.end < testMatch.end)
    in
    if List.isEmpty remainMatches || List.isEmpty (overlappingMatches remainMatches) then
        Just (Match match :: remainMatches)

    else
        Nothing



-- Remove tokens inside the parsed ahead regex match


removeParsedAheadTokens : Match -> List Token -> List Token
removeParsedAheadTokens (Match match) tokensTail =
    List.filter (\token -> token.index >= match.end) tokensTail



-- Inline link or image


checkForInlineLinkTypeOrImageType : String -> Match -> References -> Maybe Match
checkForInlineLinkTypeOrImageType remainText (Match tempMatch) refs =
    case Regex.findAtMost 1 inlineLinkTypeOrImageTypeRegex remainText of
        first :: _ ->
            case inlineLinkTypeOrImageTypeRegexToMatch tempMatch first of
                Just match ->
                    Just match

                Nothing ->
                    checkForInlineReferences remainText (Match tempMatch) refs

        [] ->
            checkForInlineReferences remainText (Match tempMatch) refs


checkForInlineReferences : String -> Match -> References -> Maybe Match
checkForInlineReferences remainText (Match tempMatch) references =
    let
        matches =
            Regex.findAtMost 1 refLabelRegex remainText
    in
    refRegexToMatch tempMatch references (List.head matches)


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
            Just (
                Match
                    { matchModel
                        | type_ = type_
                        , end = matchModel.end + regexMatchLength
                    }
                    )


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


emphasisTTM : List Token -> List Token -> List Match -> References -> String -> List Match
emphasisTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            strikethroughTTM (List.reverse tokens) [] matches references rawText

        token :: tokensTail ->
            case token.meaning of
                EmphasisToken char { leftFringeRank, rightFringeRank } ->
                    -- Close or opening token
                    if leftFringeRank == rightFringeRank then
                        -- If 1) is not surrounded by whitespace and
                        --    2) is not '_' or is surronded by puntuaction
                        -- is a close or opening tag
                        if rightFringeRank /= 0 && (char /= '_' || rightFringeRank == 1) then
                            -- Search for opening tag and add
                            -- match if the sum of lengths
                            -- is not multiple of 3, otherwise add
                            -- opening tag
                            case findToken (isOpenEmphasisToken token) tokens of
                                Just found ->
                                    let
                                        ( newRemaining, match, newTokens ) =
                                            emphasisToMatch references rawText token tokensTail found
                                    in
                                    emphasisTTM newRemaining newTokens (match :: matches) references rawText

                                Nothing ->
                                    emphasisTTM tokensTail (token :: tokens) matches references rawText

                        else
                            emphasisTTM tokensTail tokens matches references rawText

                    else if leftFringeRank < rightFringeRank then
                        -- Opening token
                        emphasisTTM tokensTail (token :: tokens) matches references rawText

                    else
                        -- Closing token
                        case findToken (isOpenEmphasisToken token) tokens of
                            Just found ->
                                let
                                    ( newRemaining, match, newTokens ) =
                                        emphasisToMatch references rawText token tokensTail found
                                in
                                emphasisTTM newRemaining newTokens (match :: matches) references rawText

                            Nothing ->
                                emphasisTTM tokensTail tokens matches references rawText

                _ ->
                    emphasisTTM tokensTail (token :: tokens) matches references rawText


isOpenEmphasisToken : Token -> Token -> Bool
isOpenEmphasisToken closeToken openToken =
    case openToken.meaning of
        EmphasisToken openChar open ->
            case closeToken.meaning of
                EmphasisToken closeChar close ->
                    if openChar == closeChar then
                        if open.leftFringeRank == open.rightFringeRank || close.leftFringeRank == close.rightFringeRank then
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


lineBreakTTM : List Token -> List Token -> List Match -> References -> String -> List Match
lineBreakTTM remaining tokens matches refs rawText =
    case remaining of
        [] ->
            matches

        token :: tokensTail ->
            case token.meaning of
                HardLineBreakToken ->
                    -- NOTE: the origiginal also moved into this branch when
                    -- (token.meaning == SoftLineBreakToken && softAsHardLineBreak
                    lineBreakTTM tokensTail tokens (tokenToMatch token HardLineBreakType :: matches) refs rawText

                _ ->
                    lineBreakTTM tokensTail (token :: tokens) matches refs rawText


-- StrikethroughType Tokens To Matches

isStrikethroughTokenPair : Token -> Token -> Bool
isStrikethroughTokenPair closeToken openToken =
    let
        (openTokenIsStrikethrough, openTokenLength) =
            case openToken.meaning of
                -- If open token is escaped, ignore first '~'
                StrikethroughToken Escaped ->
                    (True, openToken.length - 1)
                StrikethroughToken NotEscaped ->
                    (True, openToken.length)

                _ ->
                    (False, 0)

        (closeTokenIsStrikethrough, closeTokenLength) =
            case closeToken.meaning of
                -- If close token is escaped, ignore first '~'
                StrikethroughToken Escaped ->
                    (True, closeToken.length - 1)
                StrikethroughToken NotEscaped ->
                    (True, closeToken.length)

                _ ->
                    (False, 0)
    in
        closeTokenIsStrikethrough && openTokenIsStrikethrough && closeTokenLength == openTokenLength

strikethroughToMatch : Token -> List Match -> References -> String -> ( Token, List Token, List Token ) -> ( List Token, List Match )
strikethroughToMatch closeToken matches references rawText ( openToken, _, remainTokens ) =
    let
        -- If open token is escaped, ignore first '~'
        updatedOpenToken : Token
        updatedOpenToken =
            case openToken.meaning of
                StrikethroughToken Escaped ->
                    { openToken
                        | index = openToken.index + 1
                        , length = openToken.length - 1
                    }

                _ ->
                    openToken

        match =
            tokenPairToMatch
                references
                rawText
                cleanWhitespaces
                StrikethroughType
                updatedOpenToken
                closeToken
                []
    in
    ( remainTokens
    , match :: matches
    )

strikethroughTTM : List Token -> List Token -> List Match -> References -> String -> List Match
strikethroughTTM remaining tokens matches references rawText =
    case remaining of
        [] ->
            lineBreakTTM (List.reverse tokens) [] matches references rawText

        token :: tokensTail ->
            case token.meaning of
                StrikethroughToken isEscaped ->
                  case findToken (isStrikethroughTokenPair token) tokens of
                      Just content ->
                          let
                              ( newTokens, newMatches ) =
                                  strikethroughToMatch token matches references rawText content
                          in
                          strikethroughTTM tokensTail newTokens newMatches references rawText


                      Nothing ->
                          strikethroughTTM tokensTail (token :: tokens) matches references rawText

                _ ->
                    strikethroughTTM tokensTail (token :: tokens) matches references rawText




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

        StrikethroughType ->
            Strikethrough
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
