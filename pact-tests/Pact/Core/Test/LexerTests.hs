-- |

module Pact.Core.Test.LexerTests where
import Test.Tasty
import Test.Tasty.HUnit
import Pact.Core.Syntax.Lexer (lexer)
import Pact.Core.Syntax.LexUtils
import Pact.Core.Info (SpanInfo(..))


tests :: TestTree
tests = testGroup "Lexer spans"
  [ testCase "single line expression" $ let
      expected =
        [PosToken TokenOpenParens (SpanInfo 0 0 0 1)
        ,PosToken (TokenIdent "test") (SpanInfo 0 1 0 5)
        ,PosToken TokenColon (SpanInfo 0 6 0 7)
        ,PosToken (TokenIdent "X") (SpanInfo 0 7 0 8)
        ,PosToken TokenOpenParens (SpanInfo 0 9 0 10)
        ,PosToken (TokenIdent "x") (SpanInfo 0 10 0 11)
        ,PosToken TokenColon (SpanInfo 0 11 0 12)
        ,PosToken (TokenIdent "integer") (SpanInfo 0 13 0 20)
        ,PosToken TokenCloseParens (SpanInfo 0 20 0 21)
        ,PosToken TokenTrue (SpanInfo 0 22 0 26)
        ,PosToken TokenCloseParens (SpanInfo 0 26 0 27)]
      in either (const (assertFailure "")) (@?= expected) (lexer "(test :X (x: integer) true)")
  , testCase "single line expression (spaces)" $ let
      expected =
        [PosToken TokenOpenParens (SpanInfo 0 0 0 1)
        ,PosToken (TokenIdent "test") (SpanInfo 0 1 0 5)
        ,PosToken TokenColon (SpanInfo 0 8 0 9)
        ,PosToken (TokenIdent "X") (SpanInfo 0 9 0 10)
        ,PosToken TokenOpenParens (SpanInfo 0 13 0 14)
        ,PosToken (TokenIdent "x") (SpanInfo 0 14 0 15)
        ,PosToken TokenColon (SpanInfo 0 15 0 16)
        ,PosToken (TokenIdent "integer") (SpanInfo 0 19 0 26)
        ,PosToken TokenCloseParens (SpanInfo 0 26 0 27)
        ,PosToken TokenTrue (SpanInfo 0 28 0 32)
        ,PosToken TokenCloseParens (SpanInfo 0 34 0 35)]
      in either (const (assertFailure "")) (@?= expected) (lexer "(test   :X   (x:   integer) true  )")
  , testCase "multiline expression" $ let
      expected =
        [PosToken TokenOpenParens (SpanInfo 0 0 0 1)
        ,PosToken (TokenIdent "test") (SpanInfo 0 1 0 5)
        ,PosToken TokenColon (SpanInfo 1 1 1 2)
        ,PosToken (TokenIdent "X") (SpanInfo 1 2 1 3)
        ,PosToken TokenOpenParens (SpanInfo 1 4 1 5)
        ,PosToken (TokenIdent "x") (SpanInfo 1 5 1 6)
        ,PosToken TokenColon (SpanInfo 1 6 1 7)
        ,PosToken (TokenIdent "integer") (SpanInfo 3 1 3 8)
        ,PosToken TokenCloseParens (SpanInfo 3 8 3 9)
        ,PosToken TokenTrue (SpanInfo 3 10 3 14)
        ,PosToken TokenCloseParens (SpanInfo 4 0 4 1)]
      in either (const (assertFailure "")) (@?= expected) (lexer "(test\n :X (x:\n\n integer) true\n)")
  ]
