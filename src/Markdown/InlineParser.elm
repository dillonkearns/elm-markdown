module Markdown.InlineParser exposing (parse, query, walk)

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


initParser : References -> String -> Parser
initParser refs rawText =
    { rawText = rawText
    , tokens = []
    , matches = []
    , refs = refs
    }


addMatch : Parser -> Match -> Parser
addMatch model match =
    { model
        | matches =
            match :: model.matches
    }


addToken : Parser -> Token -> Parser
addToken model token =
    { model
        | tokens =
            token :: model.tokens
    }


filterTokens : (Token -> Bool) -> Parser -> Parser
filterTokens filter model =
    { model
        | tokens =
            List.filter filter model.tokens
    }


reverseTokens : Parser -> Parser
reverseTokens model =
    { rawText = model.rawText
    , tokens = List.reverse model.tokens
    , matches = model.matches
    , refs = model.refs
    }



-- Parser


parse : References -> String -> List Inline
parse refs rawText =
    rawText
        |> String.trim
        |> initParser refs
        |> tokenize
        |> tokensToMatches
        |> organizeParserMatches
        |> parseText
        |> .matches
        |> matchesToInlines


parseText : Parser -> Parser
parseText model =
    { model
        | matches =
            parseTextMatches model.rawText [] model.matches
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
                { matchModel
                    | matches =
                        parseTextMatches matchModel.text [] matchModel.matches
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
            if matchHead.type_ == NormalType then
                updtMatch :: parsedMatches
                -- New Match

            else if matchModel.end == matchHead.start then
                updtMatch :: parsedMatches
                -- New Match and add in between unmatched string

            else if matchModel.end < matchHead.start then
                updtMatch
                    :: normalMatch (String.slice matchModel.end matchHead.start rawText)
                    :: parsedMatches
                -- Overlaping or inside previous Match

            else
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
    | CharToken Char
    | RightAngleBracket Bool -- isEscaped
    | HtmlToken Bool HtmlModel -- isOpening
    | EmphasisToken Char ( Int, Int ) -- ( leftFringeRank, rightFringeRank )
    | SoftLineBreakToken
    | HardLineBreakToken


findToken : (Token -> Bool) -> List Token -> Maybe ( Token, List Token, List Token )
findToken isToken tokens =
    findTokenHelp ( Nothing, [], [] ) isToken tokens


findTokenHelp : ( Maybe Token, List Token, List Token ) -> (Token -> Bool) -> List Token -> Maybe ( Token, List Token, List Token )
findTokenHelp ( maybeToken_, innerTokens_, remainTokens_ ) isToken tokens =
    let
        return : ( Maybe Token, List Token, List Token ) -> Maybe ( Token, List Token, List Token )
        return ( maybeToken, innerTokens, remainTokens ) =
            maybeToken
                |> Maybe.map
                    (\token ->
                        ( token
                        , List.reverse innerTokens
                        , List.reverse remainTokens
                        )
                    )
    in
    case tokens of
        [] ->
            return ( maybeToken_, innerTokens_, remainTokens_ )

        nextToken :: remainingTokens ->
            if isToken nextToken then
                return
                    ( Just nextToken
                    , innerTokens_
                    , List.reverse remainingTokens
                    )

            else
                findTokenHelp
                    ( Nothing
                    , nextToken :: innerTokens_
                    , []
                    )
                    isToken
                    remainingTokens


tokenPairToMatch : Parser -> (String -> String) -> Type -> Token -> Token -> List Token -> Match
tokenPairToMatch model processText type_ openToken closeToken innerTokens =
    let
        start =
            openToken.index

        end =
            closeToken.index + closeToken.length

        textStart =
            openToken.index + openToken.length

        textEnd =
            closeToken.index

        match : MatchModel
        match =
            { type_ = type_
            , start = start
            , end = end
            , textStart = textStart
            , textEnd = textEnd
            , text =
                String.slice textStart textEnd model.rawText
                    |> processText
            , matches = []
            }

        matches : List Match
        matches =
            { model
                | tokens = innerTokens
                , matches = []
            }
                |> tokensToMatches
                |> .matches
                |> List.map
                    (\(Match matchModel) ->
                        prepareChildMatch match matchModel
                    )
    in
    Match { match | matches = matches }


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


tokenize : Parser -> Parser
tokenize model =
    { model
        | tokens =
            findCodeTokens model.rawText
                |> (++) (findAsteriskEmphasisTokens model.rawText)
                |> (++) (findUnderlineEmphasisTokens model.rawText)
                |> (++) (findLinkImageOpenTokens model.rawText)
                |> (++) (findLinkImageCloseTokens model.rawText)
                |> (++)
                    (findHardBreakTokens
                        model.rawText
                    )
                |> (++) (findAngleBracketLTokens model.rawText)
                |> (++) (findAngleBracketRTokens model.rawText)
                |> List.sortBy .index
    }



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
                    Maybe.map String.length maybeBackslashes
                        |> Maybe.withDefault 0

                leftFringeLength : Int
                leftFringeLength =
                    maybeLeftFringe
                        |> Maybe.map String.length
                        |> Maybe.withDefault 0

                mLeftFringe : Maybe String
                mLeftFringe =
                    if
                        regMatch.index
                            /= 0
                            && leftFringeLength
                            == 0
                    then
                        String.slice
                            (regMatch.index - 1)
                            regMatch.index
                            rawText
                            |> Just

                    else
                        maybeLeftFringe

                isEscaped : Bool
                isEscaped =
                    not (isEven backslashesLength)
                        && leftFringeLength
                        == 0
                        || mLeftFringe
                        == Just "\\"

                fringeRank : ( Int, Int )
                fringeRank =
                    ( if isEscaped then
                        1

                      else
                        getFringeRank mLeftFringe
                    , getFringeRank maybeRightFringe
                    )

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

                delimiterLength : Int
                delimiterLength =
                    if isEscaped then
                        String.length delimiter - 1

                    else
                        String.length delimiter
            in
            if
                delimiterLength
                    <= 0
                    || (char == '_' && fringeRank == ( 2, 2 ))
            then
                Nothing

            else
                Just
                    { index = index
                    , length = delimiterLength
                    , meaning =
                        EmphasisToken char fringeRank
                    }

        _ ->
            Nothing


getFringeRank : Maybe String -> Int
getFringeRank =
    Maybe.map
        (String.uncons
            >> Maybe.map Tuple.first
            >> maybeCharFringeRank
        )
        >> Maybe.withDefault 0


maybeCharFringeRank : Maybe Char -> Int
maybeCharFringeRank maybeChar =
    maybeChar
        |> Maybe.map charFringeRank
        |> Maybe.withDefault 0


charFringeRank : Char -> Int
charFringeRank char =
    let
        string =
            String.fromChar char
    in
    if containSpace string then
        0

    else if containPunctuation string then
        1

    else
        2


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
                                && maybeImageOpen
                                == Just "!"
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
                    , meaning = CharToken ']'
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
                    RightAngleBracket
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
                    , meaning = CharToken '<'
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
    { model | matches = organizeMatches model.matches }


organizeMatches : List Match -> List Match
organizeMatches =
    List.sortBy (\(Match match) -> match.start)
        >> List.foldl organizeMatch []
        >> List.map
            (\(Match match) ->
                Match
                    { match
                        | matches =
                            organizeMatches match.matches
                    }
            )


organizeMatch : Match -> List Match -> List Match
organizeMatch (Match match) matches =
    case matches of
        [] ->
            [ Match match ]

        (Match prevMatch) :: matchesTail ->
            -- New Match
            if prevMatch.end <= match.start then
                Match match :: matches
                -- Inside previous Match

            else if
                prevMatch.start
                    < match.start
                    && prevMatch.end
                    > match.end
            then
                addChild prevMatch match
                    :: matchesTail
                -- Overlaping previous Match

            else
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
    -- TODO make sure all these TTM functions are actually tail-recursive
    -- note that `|>` breaks tail-recursion.
    applyTTM codeAutolinkTypeHtmlTagTTM
        >> applyTTM htmlElementTTM
        >> applyTTM linkImageTypeTTM
        >> applyTTM emphasisTTM
        >> applyTTM lineBreakTTM


applyTTM : (( List Token, Parser ) -> Parser) -> Parser -> Parser
applyTTM finderFunction model =
    -- TODO don't use a tuple here
    let
        newModel =
            { rawText = model.rawText
            , tokens = []
            , matches = model.matches
            , refs = model.refs
            }
    in
    finderFunction ( model.tokens, newModel )



-- CodeType spans, HTML tags, and autolinks Tokens To Matches
-- CodeType spans, HTML tags, and autolinks have the same precedence


codeAutolinkTypeHtmlTagTTM : ( List Token, Parser ) -> Parser
codeAutolinkTypeHtmlTagTTM ( tokens, model ) =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                CodeToken isEscaped ->
                    codeAutolinkTypeHtmlTagTTM
                        (model.tokens
                            |> findToken (isCodeTokenPair token)
                            |> Maybe.map (codeToMatch token model)
                            |> Maybe.withDefault (addToken model token)
                            |> (\b -> ( tokensTail, b ))
                        )

                RightAngleBracket isEscaped ->
                    codeAutolinkTypeHtmlTagTTM
                        (model.tokens
                            |> findToken
                                (.meaning >> (==) (CharToken '<'))
                            |> Maybe.andThen
                                (angleBracketsToMatch token
                                    isEscaped
                                    model
                                )
                            |> Maybe.withDefault model
                            |> filterTokens
                                (.meaning >> (/=) (CharToken '<'))
                            |> (\b -> ( tokensTail, b ))
                        )

                _ ->
                    codeAutolinkTypeHtmlTagTTM
                        ( tokensTail
                        , addToken model token
                        )



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
                model
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
    tokenPairToMatch model (\s -> s) CodeType openToken closeToken []
        |> autolinkToMatch
        |> ifError emailAutolinkTypeToMatch
        |> Result.map
            (\newMatch ->
                { model
                    | matches = newMatch :: model.matches
                    , tokens = remainTokens
                }
            )
        |> (\result ->
                case result of
                    Result.Err tempMatch ->
                        if not isEscaped then
                            htmlToToken
                                { model | tokens = remainTokens }
                                tempMatch

                        else
                            Result.toMaybe result

                    Result.Ok _ ->
                        Result.toMaybe result
           )



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


htmlToToken : Parser -> Match -> Maybe Parser
htmlToToken model (Match match) =
    --case model.options.rawHtml of
    --    DontParse ->
    --        Nothing
    --
    --    _ ->
    --Regex.findAtMost 1 htmlRegex match.text
    --    |> List.head
    --    |> Maybe.andThen (\_ -> htmlFromRegex model match)
    htmlFromRegex model match


htmlRegex : Regex
htmlRegex =
    Regex.fromString "^(\\/)?([a-zA-Z][a-zA-Z0-9\\-]*)(?:\\s+([^<>]*?))?(\\/)?$"
        |> Maybe.withDefault Regex.never


htmlFromRegex : Parser -> MatchModel -> Maybe Parser
htmlFromRegex model match =
    let
        --Advanced.andThen
        --    (\xmlNode ->
        --        case xmlNode of
        --            HtmlParser.Text innerText ->
        --                -- TODO is this right?
        --                Body
        --                    (UnparsedInlines innerText)
        --                    |> Advanced.succeed
        --
        --            HtmlParser.Element tag attributes children ->
        --                Advanced.andThen
        --                    (\parsedChildren ->
        --                        Advanced.succeed
        --                            (Html tag
        --                                attributes
        --                                parsedChildren
        --                            )
        --                    )
        --                    (nodesToBlocksParser children)
        --    )
        --    parser
        --consumedCharacters : HtmlParser.Parser HtmlParser.Xml
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

        _ =
            log "match" match

        parsed =
            model.rawText
                |> log "rawText"
                |> String.dropLeft match.start
                |> log "dropped"
                --|> HtmlParser.parse
                |> Advanced.run consumedCharacters

        --_ =
        --    log "model"
        --        { model = model
        --        , match = match
        --        , parsed = parsed
        --        }
    in
    case parsed of
        Ok { htmlTag, length } ->
            let
                htmlToken =
                    HtmlToken False htmlTag

                --(case htmlTag of
                --    HtmlParser.Element tag attributes _ ->
                --        { tag = tag
                --        , attributes = attributes
                --        }
                --
                --    HtmlParser.Comment comment ->
                --        { tag = "TODO handle comment", attributes = [] }
                --
                --    _ ->
                --        { tag = "TODO", attributes = [] }
                --)
            in
            { index = match.start
            , length = length
            , meaning = htmlToken
            }
                |> addToken model
                |> Just

        --|> log "Just"
        --                { index = match.start
        --                , length = match.end - match.start
        --                , meaning =
        --                    HtmlToken
        --                        (maybeClose
        --                            == Nothing
        --                            && maybeSelfClosing
        --                            == Nothing
        --                        )
        --                        (HtmlModel tag attrs)
        --                }
        --                    |> addToken model
        Err error ->
            let
                _ =
                    log "error" error
            in
            Nothing


log label value =
    --Debug.log label value
    value



-- TODO use HTML parser here
--case regexMatch.submatches of
--    maybeClose :: (Just tag) :: maybeAttributes :: maybeSelfClosing :: _ ->
--        let
--            updateModel : List Attribute -> Parser
--            updateModel attrs =
--                { index = match.start
--                , length = match.end - match.start
--                , meaning =
--                    HtmlToken
--                        (maybeClose
--                            == Nothing
--                            && maybeSelfClosing
--                            == Nothing
--                        )
--                        (HtmlModel tag attrs)
--                }
--                    |> addToken model
--
--            attributes : List Attribute
--            attributes =
--                Maybe.map applyAttributesRegex maybeAttributes
--                    |> Maybe.withDefault []
--
--            filterAttributes : List Attribute -> List String -> List Attribute
--            filterAttributes attrs allowed =
--                List.filter
--                    (\attr ->
--                        List.member (Tuple.first attr) allowed
--                    )
--                    attrs
--
--            noAttributesInCloseTag : Bool
--            noAttributesInCloseTag =
--                maybeClose
--                    == Nothing
--                    || maybeClose
--                    /= Nothing
--                    && attributes
--                    == []
--        in
--        --case model.options.rawHtml of
--        --ParseUnsafe ->
--        if noAttributesInCloseTag then
--            Just (updateModel attributes)
--
--        else
--            Nothing
--_ ->
--    Nothing
--Sanitize { allowedHtmlElements, allowedHtmlAttributes } ->
--    if
--        List.member tag allowedHtmlElements
--            && noAttributesInCloseTag
--    then
--        filterAttributes attributes allowedHtmlAttributes
--            |> updateModel
--            |> Just
--
--    else
--        Nothing
--
--DontParse ->
--    Nothing
--applyAttributesRegex : String -> List Attribute
--applyAttributesRegex =
--    Regex.find htmlAttributesRegex
--        >> List.filterMap attributesFromRegex


htmlAttributesRegex : Regex
htmlAttributesRegex =
    Regex.fromString "([a-zA-Z:_][a-zA-Z0-9\\-_.:]*)(?: ?= ?(?:\"([^\"]*)\"|'([^']*)'|([^\\s\"'=<>`]*)))?"
        |> Maybe.withDefault Regex.never



--attributesFromRegex : Regex.Match -> Maybe Attribute
--attributesFromRegex regexMatch =
--    case regexMatch.submatches of
--        (Just "") :: _ ->
--            Nothing
--
--        (Just name) :: maybeDoubleQuotes :: maybeSingleQuotes :: maybeUnquoted :: _ ->
--            let
--                maybeValue : Maybe String
--                maybeValue =
--                    returnFirstJust
--                        [ maybeDoubleQuotes
--                        , maybeSingleQuotes
--                        , maybeUnquoted
--                        ]
--            in
--            Just ( { name = name, maybeValue )
--
--        _ ->
--            Nothing


htmlElementTTM : ( List Token, Parser ) -> Parser
htmlElementTTM ( tokens, model ) =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                HtmlToken isOpen htmlModel ->
                    if isVoidTag htmlModel || not isOpen then
                        htmlElementTTM
                            (tokenToMatch token (HtmlType htmlModel)
                                |> addMatch model
                                |> (\b -> ( tokensTail, b ))
                            )

                    else
                        htmlElementTTM
                            (tokensTail
                                |> findToken (isCloseToken htmlModel)
                                |> Maybe.map (htmlElementToMatch token model htmlModel)
                                |> Maybe.withDefault
                                    (tokenToMatch token (HtmlType htmlModel)
                                        |> addMatch model
                                        |> (\b -> ( tokensTail, b ))
                                    )
                            )

                _ ->
                    htmlElementTTM
                        ( tokensTail
                        , addToken model token
                        )


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


htmlElementToMatch : Token -> Parser -> HtmlModel -> ( Token, List Token, List Token ) -> ( List Token, Parser )
htmlElementToMatch openToken model htmlModel ( closeToken, innerTokens, remainTokens ) =
    ( remainTokens
    , { model
        | matches =
            tokenPairToMatch
                model
                (\s -> s)
                (HtmlType htmlModel)
                openToken
                closeToken
                innerTokens
                :: model.matches
      }
    )



-- LinkType and images Tokens To Matches
-- LinkType, reference link and images have precedence over emphasis


linkImageTypeTTM : ( List Token, Parser ) -> Parser
linkImageTypeTTM ( tokens, model ) =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                CharToken ']' ->
                    linkImageTypeTTM
                        (model.tokens
                            |> findToken isLinkTypeOrImageOpenToken
                            |> Maybe.andThen
                                (linkOrImageTypeToMatch token tokensTail model)
                            |> Maybe.withDefault ( tokensTail, model )
                        )

                _ ->
                    linkImageTypeTTM
                        ( tokensTail
                        , addToken model token
                        )


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
        args : Bool -> ( String, Match, Parser )
        args isLinkType =
            ( remainText
            , tempMatch isLinkType
            , { model | tokens = remainTokens }
            )

        remainText : String
        remainText =
            String.dropLeft (closeToken.index + 1) model.rawText

        tempMatch : Bool -> Match
        tempMatch isLinkType =
            tokenPairToMatch
                model
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

        linkOpenTokenToInactive : Parser -> Parser
        linkOpenTokenToInactive model_ =
            let
                process : Token -> Token
                process token =
                    case token.meaning of
                        LinkOpenToken _ ->
                            { token | meaning = LinkOpenToken False }

                        _ ->
                            token
            in
            { model_ | tokens = List.map process model_.tokens }
    in
    case openToken.meaning of
        ImageOpenToken ->
            checkForInlineLinkTypeOrImageType (args False)
                |> ifError checkForRefLinkTypeOrImageType
                |> Result.mapError (\_ -> ())
                |> Result.andThen checkParsedAheadOverlapping
                |> Result.map (removeParsedAheadTokens tokensTail)
                |> ifError (\_ -> Result.Ok removeOpenToken)
                |> Result.toMaybe

        -- Active opening: set all before to inactive if found
        LinkOpenToken True ->
            checkForInlineLinkTypeOrImageType (args True)
                |> ifError checkForRefLinkTypeOrImageType
                |> Result.mapError (\_ -> ())
                |> Result.andThen checkParsedAheadOverlapping
                |> Result.map linkOpenTokenToInactive
                |> Result.map (removeParsedAheadTokens tokensTail)
                |> ifError (\_ -> Result.Ok removeOpenToken)
                |> Result.toMaybe

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


checkForInlineLinkTypeOrImageType : ( String, Match, Parser ) -> Result ( String, Match, Parser ) Parser
checkForInlineLinkTypeOrImageType ( remainText, Match tempMatch, model ) =
    Regex.findAtMost 1 inlineLinkTypeOrImageTypeRegex remainText
        |> List.head
        |> Maybe.andThen (inlineLinkTypeOrImageTypeRegexToMatch tempMatch model)
        |> Maybe.map (addMatch model)
        |> Result.fromMaybe ( remainText, Match tempMatch, model )


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


inlineLinkTypeOrImageTypeRegexToMatch : MatchModel -> Parser -> Regex.Match -> Maybe Match
inlineLinkTypeOrImageTypeRegexToMatch matchModel model regexMatch =
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
                            ( rawUrl, maybeTitle )
                                |> prepareUrlAndTitle
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


prepareUrlAndTitle : ( String, Maybe String ) -> ( String, Maybe String )
prepareUrlAndTitle ( rawUrl, maybeTitle ) =
    ( encodeUrl (formatStr rawUrl)
    , Maybe.map formatStr maybeTitle
    )



-- Reference link or image


checkForRefLinkTypeOrImageType : ( String, Match, Parser ) -> Result ( String, Match, Parser ) Parser
checkForRefLinkTypeOrImageType ( remainText, Match tempMatch, model ) =
    Regex.findAtMost 1 refLabelRegex remainText
        |> List.head
        |> refRegexToMatch tempMatch model
        |> Maybe.map (addMatch model)
        |> Result.fromMaybe ( remainText, Match tempMatch, model )


refLabelRegex : Regex
refLabelRegex =
    Regex.fromString ("^\\[\\s*(" ++ insideSquareBracketRegex ++ ")\\s*\\]")
        |> Maybe.withDefault Regex.never


refRegexToMatch : MatchModel -> Parser -> Maybe Regex.Match -> Maybe Match
refRegexToMatch matchModel model maybeRegexMatch =
    let
        maybeRefItem : Maybe ( String, Maybe String )
        maybeRefItem =
            Dict.get (prepareRefLabel refLabel) model.refs

        refLabel : String
        refLabel =
            maybeRegexMatch
                |> Maybe.map (.submatches >> List.head)
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault matchModel.text
                |> (\str ->
                        if String.isEmpty str then
                            matchModel.text

                        else
                            str
                   )

        toMatch : ( String, Maybe String ) -> Match
        toMatch urlTitle =
            { matchModel
                | type_ =
                    prepareUrlAndTitle urlTitle
                        |> (case matchModel.type_ of
                                ImageType _ ->
                                    ImageType

                                _ ->
                                    LinkType
                           )
                , end = matchModel.end + regexMatchLength
            }
                |> Match

        regexMatchLength : Int
        regexMatchLength =
            maybeRegexMatch
                |> Maybe.map (.match >> String.length)
                |> Maybe.withDefault 0
    in
    Maybe.map toMatch maybeRefItem


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


emphasisTTM : ( List Token, Parser ) -> Parser
emphasisTTM ( tokens, model ) =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            case token.meaning of
                EmphasisToken char ( leftRank, rightRank ) ->
                    -- Close or opening token
                    if leftRank == rightRank then
                        -- If 1) is not surrounded by whitespace and
                        --    2) is not '_' or is surronded by puntuaction
                        -- is a close or opening tag
                        if
                            rightRank
                                /= 0
                                && (char /= '_' || rightRank == 1)
                        then
                            -- Search for opening tag and add
                            -- match if the sum of lengths
                            -- is not multiple of 3, otherwise add
                            -- opening tag
                            emphasisTTM
                                (model.tokens
                                    |> findToken (isOpenEmphasisToken token)
                                    |> Maybe.map
                                        (emphasisToMatch token
                                            tokensTail
                                            model
                                        )
                                    |> Maybe.withDefault
                                        ( tokensTail
                                        , addToken model token
                                        )
                                )

                        else
                            emphasisTTM ( tokensTail, model )
                        -- Opening token

                    else if leftRank < rightRank then
                        emphasisTTM
                            ( tokensTail
                            , addToken model token
                            )
                        -- Closing token

                    else
                        emphasisTTM
                            (model.tokens
                                |> findToken (isOpenEmphasisToken token)
                                |> Maybe.map
                                    (emphasisToMatch token
                                        tokensTail
                                        model
                                    )
                                |> Maybe.withDefault ( tokensTail, model )
                            )

                _ ->
                    emphasisTTM
                        ( tokensTail
                        , addToken model token
                        )


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


emphasisToMatch : Token -> List Token -> Parser -> ( Token, List Token, List Token ) -> ( List Token, Parser )
emphasisToMatch closeToken tokensTail model ( openToken, innerTokens, remainTokens ) =
    let
        remainLength : Int
        remainLength =
            openToken.length - closeToken.length

        updt =
            -- Perfect match
            if remainLength == 0 then
                { openToken = openToken
                , closeToken = closeToken
                , remainTokens = remainTokens
                , tokensTail = tokensTail
                }
                -- Still has opening token

            else if remainLength > 0 then
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
                -- Still has closing token

            else
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
                model
                (\s -> s)
                (EmphasisType updt.openToken.length)
                updt.openToken
                updt.closeToken
                (List.reverse innerTokens)
    in
    ( updt.tokensTail
    , { model
        | matches = match :: model.matches
        , tokens = updt.remainTokens
      }
    )



-- Line Break Tokens To Matches


lineBreakTTM : ( List Token, Parser ) -> Parser
lineBreakTTM ( tokens, model ) =
    case tokens of
        [] ->
            reverseTokens model

        token :: tokensTail ->
            if
                token.meaning
                    == HardLineBreakToken
                    || (token.meaning
                            == SoftLineBreakToken
                            && softAsHardLineBreak
                       )
            then
                lineBreakTTM
                    ({ model
                        | matches =
                            tokenToMatch token HardLineBreakType
                                :: model.matches
                     }
                        |> (\b -> ( tokensTail, b ))
                    )

            else
                lineBreakTTM
                    ( tokensTail
                    , addToken model token
                    )



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
