module Markdown.Table exposing (..)


type Table cell
    = Table (List (HeaderCell cell)) (List (List cell))


type alias HeaderCell cell =
    { label : cell
    , alignment : Maybe Alignment
    }


type Alignment
    = Left
    | Right
    | Center
