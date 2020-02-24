module Markdown.LinkReferenceDefinition exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser LinkReferenceDefinition
parser =
    succeed ( "foo", { destination = "/url", title = Just "title" } )


type alias LinkReferenceDefinition =
    ( String
    , { destination : String
      , title : Maybe String
      }
    )
