module Markdown.Table exposing (HeaderCell, Table(..))

import Markdown.Block


type Table cell
    = Table (List (HeaderCell cell)) (List (List cell))


type alias HeaderCell cell =
    { label : cell
    , alignment : Maybe Markdown.Block.Alignment
    }
