module Edda.Refine.Utils

import Edda.Model
import Edda.Model.Utils
import Edda.Squash

treatPunc : Char -> Maybe (Inline Prime)
treatPunc c = case c of
    ' '   => Just Space
    '\n'  => Just Newline
    '\t'  => Just Tab
    '<'   => Just LAngle
    '>'   => Just RAngle
    ':'   => Just Colon
    ';'   => Just Semi
    '/'   => Just FSlash
    '\\'  => Just BSlash
    '\''  => Just Apostrophe
    '-'   => Just Hyphen
    ','   => Just Comma
    '+'   => Just Plus
    '!'   => Just Bang
    '.'   => Just Period
    '?'   => Just QMark
    '#'   => Just Hash
    '='   => Just Equals
    '$'   => Just Dollar
    '|'   => Just Pipe

    '{' => Just LBrace
    '}' => Just RBrace
    '(' => Just LParen
    ')' => Just RParen
    '[' => Just LBrack
    ']' => Just RBrack
    '"' => Just SMark
    otherwise => Nothing
