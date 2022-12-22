module Pact.Core.Test.LexerParserTests where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

import Data.Text.Prettyprint.Doc
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as BS 
--import Data.String (fromString)

import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.LexUtils as Lisp
import Pact.Core.Syntax.Lisp.LexUtils (Token(..))
--import qualified Pact.Core.Syntax.Lisp.Parser as Lisp


showPretty :: Pretty a => a -> BS.ByteString 
showPretty = BS.pack . show . pretty 

tokenToSrc :: Token -> BS.ByteString
tokenToSrc = \case 
  TokenString s -> "\"" <> showPretty s <> "\""
  TokenIdent n  -> encodeUtf8 n
  TokenNumber n -> encodeUtf8 n
  tok           -> showPretty tok

token_gen :: Gen Token
token_gen = Gen.choice $ unary ++ [ ident, number, string]
  where
    string = TokenString <$> (Gen.text (Range.linear 0 16) Gen.alphaNum)
    number = do
      n <- Gen.int $ Range.linear (-1000) (1000)
      pure . TokenNumber $ T.pack $ show n
    ident = do
      pref <- Gen.alpha
      suff <- Gen.string (Range.constant 0 16) Gen.alphaNum  
      pure . TokenIdent . T.pack $ pref : suff
    unary = Gen.constant 
      <$> [ TokenLet
          , TokenIf
          , TokenLambda
          , TokenTry
          , TokenError
          , TokenModule
          , TokenKeyGov
          , TokenCapGov
          , TokenInterface
          , TokenImport
          , TokenDefun
          , TokenDefConst
          , TokenDefCap
          , TokenDefPact
          , TokenDefSchema
          , TokenDefTable
          , TokenBless
          , TokenImplements
          -- delimiters
          , TokenOpenBrace -- {
          , TokenCloseBrace -- }
          , TokenOpenParens -- (
          , TokenCloseParens -- )
          , TokenOpenBracket
          , TokenCloseBracket
          , TokenComma
          , TokenColon
          , TokenDot
          -- Types
          , TokenTyList
          , TokenTyTable
          , TokenTyInteger
          , TokenTyDecimal
          , TokenTyString
          , TokenTyBool
          , TokenTyUnit
          , TokenTyArrow
          -- Operators
          , TokenEq
          , TokenNeq
          , TokenGT
          , TokenGEQ
          , TokenLT
          , TokenLEQ
          , TokenPlus
          , TokenMinus
          , TokenMult
          , TokenDiv
          , TokenPow
          --, TokenObjAccess
          --, TokenObjRemove
          , TokenBitAnd
          , TokenBitOr
          , TokenBitComplement
          , TokenAnd
          , TokenOr
          , TokenTrue
          , TokenFalse
          , TokenBlockIntro
          , TokenSuspend
          -- Repl-specific tokens
          -- , TokenLoad
          -- , TokenTypechecks
          -- , TokenTypecheckFailure
          -- Layout
          --, TokenEOF
          ]

lexer :: Property
lexer = property $ do
  toks <- forAll $ Gen.list (Range.constant 0 10) token_gen
  ptoks <- evalEither $ Lisp.lexer (BS.unlines $ (tokenToSrc <$> toks))
  toks === (Lisp._ptToken <$> ptoks)

tests :: TestTree
tests = testGroup "Lexer and Parser Tests"
  [ testProperty "lexer" lexer ]
