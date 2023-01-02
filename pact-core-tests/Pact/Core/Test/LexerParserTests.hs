{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pact.Core.Test.LexerParserTests where

import Debug.Trace -- rs (2022-12-23): Remove


import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Text.Prettyprint.Doc
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Default (def)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as BS 
--import Data.String (fromString)
import Data.Decimal(DecimalRaw(..))

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Syntax.Common
import qualified Pact.Core.Type as Core
import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.LexUtils as Lisp
import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp
import Pact.Core.Syntax.Lisp.LexUtils (Token(..))
import Pact.Core.Literal


showPretty :: Pretty a => a -> BS.ByteString 
showPretty = BS.pack . show . pretty 

tokenToSrc :: Token -> BS.ByteString
tokenToSrc = \case 
  TokenString s -> "\"" <> showPretty s <> "\""
  TokenIdent n  -> encodeUtf8 n
  TokenNumber n -> encodeUtf8 n
  tok           -> showPretty tok

identGen :: Gen T.Text
identGen = do
  pref <- Gen.alpha
  suff <- Gen.string (Range.constant 0 16) (Gen.constant '-' <|> Gen.alphaNum)
  pure $ T.pack (pref : suff)

tokenGen :: Gen Token
tokenGen = Gen.choice $ unary ++ [ TokenIdent <$> identGen, number, string]
  where
    string = TokenString <$> Gen.text (Range.linear 0 16) Gen.alphaNum
    number = do
      n <- Gen.int $ Range.linear (-1000) (1000)
      pure . TokenNumber $ T.pack $ show n
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
          , TokenLoad
          , TokenTypechecks
          , TokenTypecheckFailure
          ]

lexer_roundtrip :: Property
lexer_roundtrip = property $ do
  toks <- forAll $ Gen.list (Range.constant 0 10) tokenGen
  ptoks <- evalEither $ Lisp.lexer (BS.unlines $ (tokenToSrc <$> toks))
  toks === (Lisp._ptToken <$> ptoks)


type ParserGen = Gen (Lisp.Expr ())

toUnitExpr :: Lisp.ParsedExpr -> Lisp.Expr ()
toUnitExpr = \case
    Lisp.Var n _ -> Lisp.Var n ()
    Lisp.LetIn nel e _ -> Lisp.LetIn (binderToUnit <$> nel) (toUnitExpr e) ()
    Lisp.Lam n ty expr _ -> Lisp.Lam n ty (toUnitExpr expr) ()
    Lisp.Conditional c _ -> Lisp.Conditional (toUnitExpr <$> c) ()
    Lisp.App a xs _ -> Lisp.App (toUnitExpr a) (toUnitExpr <$> xs) ()
    Lisp.Block xs _ -> Lisp.Block (toUnitExpr <$> xs) ()
    Lisp.Operator op _ -> Lisp.Operator op ()
    Lisp.List xs _ -> Lisp.List (toUnitExpr <$> xs) ()
    Lisp.Constant lit _ -> Lisp.Constant lit ()
    Lisp.Try a b _ -> Lisp.Try (toUnitExpr a) (toUnitExpr b) ()
    Lisp.Suspend a _ -> Lisp.Suspend (toUnitExpr a) ()
    Lisp.Error t _ -> Lisp.Error t ()
    where
      binderToUnit (Lisp.Binder t ty e) = Lisp.Binder t ty (toUnitExpr e)


parsedExprToSrc :: Lisp.Expr () -> BS.ByteString
parsedExprToSrc = BS.pack . show . pretty

varGen :: ParserGen
varGen = Gen.choice [bn, qn]
  where
    bn = (\n -> Lisp.Var (BN $ BareName n) ()) <$> identGen
    qn = do
      modName <- identGen
      name <- identGen
      modNs <- Gen.maybe (NamespaceName <$> identGen)
      let qname = QualifiedName name (ModuleName modName modNs)
      pure $ Lisp.Var (QN qname) ()

constantGen :: ParserGen
constantGen = (`Lisp.Constant` ()) <$> Gen.choice
  [ LString <$> Gen.text (Range.constant 1 64) Gen.alphaNum
  , LInteger <$> Gen.integral_(Range.constant (-10000) 10000)
  , decimalGen
  , Gen.constant LUnit
  , LBool <$> Gen.bool
  ]
  where
    decimalGen = do
      i <- Gen.integral (Range.constant 0 255)
      m <- Gen.integral (Range.constant 0 255)
      pure $ LDecimal (Decimal i m)


operatorGen :: ParserGen
operatorGen = Gen.choice $ (\x -> pure (Lisp.Operator x ())) <$>
  [ AddOp
  , SubOp
  , MultOp
  , DivOp
  , GTOp
  , GEQOp
  , LTOp
  , LEQOp
  --, EQOp
  , NEQOp
  , BitAndOp
  , BitOrOp
  , BitComplementOp
  , AndOp
  , OrOp
  , PowOp
  --, NegateOp
  ]
  
exprGen :: ParserGen
exprGen = Gen.recursive Gen.choice
  [ varGen
  , (`Lisp.Error` ()) <$> Gen.text (Range.constant 1 64) Gen.alphaNum
  , constantGen
  , operatorGen
  ]
  -- recursive ones
  [ Gen.subterm exprGen (`Lisp.Suspend` ())
  , Gen.subterm2 exprGen exprGen (\x y -> Lisp.Try x y ())
  --, Gen.subtermM exprGen letGen
  --, Gen.subtermM3 exprGen exprGen exprGen (\a b c -> condGen a b c >>= (\x -> pure $ Lisp.Conditional x ()))
  , Gen.subtermM exprGen $ \x -> do
      xs <- Gen.list (Range.linear 0 8) exprGen
      pure $ Lisp.App x xs ()
  , (`Lisp.Block` ()) <$> Gen.nonEmpty (Range.linear 1 8) (Gen.subterm exprGen id)
  , (`Lisp.List` ()) <$> Gen.list (Range.linear 1 8) (Gen.subterm exprGen id)
  , lamGen
  ]
  where
    lamGen = do
      (Lisp.Var n _) <- varGen
      par <- Gen.list (Range.linear 0 8) $ do
        i <- identGen
        ty <- Gen.maybe typeGen
        pure $ (i, ty)
      expr <- Gen.subterm exprGen id
      pure $ Lisp.Lam n par expr ()

    letGen inner = do
      binders <- Gen.nonEmpty (Range.constant 1 8) binderGen
      pure $ Lisp.LetIn binders inner ()

    typeGen :: Gen Type
    typeGen = Gen.choice [pt, ft, lt]
      where
        pt = Gen.choice $ Gen.constant . TyPrim <$>
          [ Core.PrimInt
          , Core.PrimDecimal
          , Core.PrimTime
          , Core.PrimBool
          , Core.PrimString
          , Core.PrimUnit
          ]
        ft = Gen.subterm2 typeGen typeGen TyFun
        lt = Gen.subterm typeGen TyList
    
    binderGen = do
      name <- identGen
      ty <- Gen.maybe typeGen
      expr <- Gen.subterm exprGen id
      pure $ Lisp.Binder name ty expr

    condGen a b c = Gen.choice [pure $ Lisp.CEAnd a b, pure $ Lisp.CEOr a b, pure $ Lisp.CEIf a b c] 
      

parser_roundtrip :: Property
parser_roundtrip = property $ do
  ptok <- forAll exprGen
  res <- evalEither $ Lisp.parseExpr =<< Lisp.lexer (parsedExprToSrc ptok)
  ptok === toUnitExpr res
  
tests :: TestTree
tests = testGroup "Lexer and Parser Tests"
  [ testProperty "lexer roundtrip" lexer_roundtrip
  , testProperty "parser roundtrip" $ withTests (1000 :: TestLimit) parser_roundtrip
  ]
