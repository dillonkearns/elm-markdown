module Markdown.Table exposing (HeaderCell, Table(..), TableDelimiterRow(..), TableHeader(..))

import Markdown.Block


type Table cell
    = Table (List (HeaderCell cell)) (List (List cell))


type TableHeader cell
    = TableHeader (List (HeaderCell cell))


type TableDelimiterRow
    = TableDelimiterRow { raw : String, trimmed : String } (List (Maybe Markdown.Block.Alignment))


type alias HeaderCell cell =
    { label : cell
    , alignment : Maybe Markdown.Block.Alignment
    }
