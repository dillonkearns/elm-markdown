module Markdown.Table exposing (HeaderCell, Table(..), TableHeader(..))

import Markdown.Block


type Table cell
    = Table (List (HeaderCell cell)) (List (List cell))


type TableHeader cell
    = TableHeader (List (HeaderCell cell))


type alias HeaderCell cell =
    { label : cell
    , alignment : Maybe Markdown.Block.Alignment
    }
