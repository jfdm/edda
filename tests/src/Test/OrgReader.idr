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
    parseTest (property "TITLE")       testTitle
    , parseTest (property "AUTHOR")    testAuthor
    , parseTest (property "DATE")      testDate
    , parseTest (many header)          testHeaders
    , parseTest (many $ lexeme inline) testFormatting
    , parseTest (many inline)          testLinks
    , parseTest (many inline)          testInlineLinks
    , parseTest (many inline)          testLinks
    , parseTest (many $ lexeme inline) testInline
    , parseTest (many block)           testTheo
    , parseTest (many block)           testCode
    , parseTest (block)                testQuote
    , parseTest (block)                testFig
    , parseTest (many block)           testEq
    , parseTest (many block)           testBlock
    ]
