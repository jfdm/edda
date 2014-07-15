module Test.OrgReader

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

import Edda.Model
import Edda.Reader.Org

import Test.Data
import Test.Utils

orgReaderTests : List (TestRes String)
orgReaderTests = [
      parseTest (many $ lexeme inline) testFormatting
    , parseTest (many inline)          testLinks
    , parseTest (many inline)          testInlineLinks
    , parseTest (many inline)          testLinks
    , parseTest (many inline)          testFNote
    , parseTest (many $ lexeme inline) testInline
    , parseTest (lex $ property "TITLE")     testTitle
    , parseTest (lex $ property "AUTHOR")    testAuthor
    , parseTest (lex $ property "DATE")      testDate
    , parseTest (many header)          testHeaders
    , parseTest (many block)           testTheo
    , parseTest (many block)           testCode
    , parseTest (block)                testQuote
    , parseTest (block)                testFig
    , parseTest (many block)           testEq
    , parseTest (many block)           testBlock
    ]
