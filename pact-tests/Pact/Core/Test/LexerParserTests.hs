module Pact.Core.Test.LexerParserTests where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as T
import Data.Decimal(DecimalRaw(..))

import Pact.Core.Gen(identGen)
import Pact.Core.Names
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.LexUtils as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import Pact.Core.Syntax.LexUtils (Token(..))
import Pact.Core.Literal
import Pact.Core.Pretty
import Pact.Core.Syntax.ParseTree (LetForm(LFLetNormal))

showPretty :: Pretty a => a -> T.Text
showPretty = T.pack . show . pretty

tokenToSrc :: Token -> T.Text
tokenToSrc = \case
  TokenString s -> "\"" <> showPretty s <> "\""
  TokenIdent n  -> n
  TokenNumber n -> n
  tok           -> showPretty tok

tokenGen :: Gen Token
tokenGen = Gen.choice $ unary ++ [ TokenIdent <$> identGen, number, string]
  where
    string = TokenString <$> Gen.text (Range.linear 0 16) Gen.alphaNum
    number = do
      n <- Gen.int $ Range.linear (-1000) 1000
      pure . TokenNumber $ T.pack $ show n
    -- Todo: maybe we separate into keyword + ident
    -- and num and turn this into an enum bounded call
    unary = Gen.constant
      <$> [ TokenLet
          , TokenLetStar
          , TokenLambda
          , TokenModule
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
          -- Operators
          , TokenTrue
          , TokenFalse
          ]

lexerRoundtrip :: Property
lexerRoundtrip = property $ do
  toks <- forAll $ Gen.list (Range.constant 0 10) tokenGen
  ptoks <- evalEither $ Lisp.lexer (T.unlines (tokenToSrc <$> toks))
  toks === (Lisp._ptToken <$> ptoks)


type ParserGen = Gen (Lisp.Expr ())

toUnitExpr :: Lisp.ParsedExpr -> Lisp.Expr ()
toUnitExpr = fmap $ const ()

parsedExprToSrc :: Lisp.Expr () -> T.Text
parsedExprToSrc = T.pack . show . pretty

varGen :: ParserGen
varGen = (`Lisp.Var` ()) <$> parsedNameGen

parsedNameGen :: Gen ParsedName
parsedNameGen = Gen.choice [qn, bn]
  where
    bn = BN . BareName <$> identGen
    qn = do
      name <- identGen
      mn   <- moduleNameGen
      let qname = QualifiedName name mn
      pure (QN qname)
parsedTyNameGen :: Gen ParsedTyName
parsedTyNameGen = Gen.choice [qn, bn]
  where
    bn = TBN . BareName <$> identGen
    qn = do
      name <- identGen
      mn   <- moduleNameGen
      let qname = QualifiedName name mn
      pure (TQN qname)

moduleNameGen :: Gen ModuleName
moduleNameGen = do
  modName <- identGen
  modNS   <- Gen.maybe (NamespaceName <$> identGen)
  pure (ModuleName modName modNS)

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


exprGen :: ParserGen
exprGen = Gen.recursive Gen.choice
  [ varGen
  , constantGen
  ]
  -- recursive ones
  [ Gen.subtermM exprGen $ \x -> do
      xs <- Gen.list (Range.linear 0 8) exprGen
      pure $ Lisp.App x xs ()
  , (`Lisp.List` ()) <$> Gen.list (Range.linear 1 8) (Gen.subterm exprGen id)
  , lamGen
  , letGen
  ]
  where
    lamGen = do
      par <- Gen.list (Range.linear 0 8) $ do
        i <- identGen
        ty <- Gen.maybe typeGen
        pure (Lisp.MArg i ty ())
      Lisp.Lam par <$> Gen.nonEmpty (Range.linear 1 8) exprGen <*> pure ()

    letGen = do
      binders <- Gen.nonEmpty (Range.constant 1 8) binderGen
      inner <- Gen.nonEmpty (Range.linear 1 8) exprGen
      pure $ Lisp.Let LFLetNormal binders inner ()

    typeGen :: Gen Lisp.Type
    typeGen = Gen.recursive Gen.choice
      (Gen.constant . Lisp.TyPrim <$> [minBound ..])
      [Lisp.TyList <$> typeGen
      ,pure Lisp.TyPolyList
      ,Lisp.TyModRef <$> Gen.list (Range.constant 1 5) moduleNameGen
      ,pure Lisp.TyGuard
      ,pure Lisp.TyKeyset
      ,Lisp.TyObject <$> parsedTyNameGen
      ,pure Lisp.TyTime
      ,pure Lisp.TyPolyObject]

    binderGen = do
      name <- identGen
      ty <- Gen.maybe typeGen
      expr <- Gen.subterm exprGen id
      pure $ Lisp.Binder name ty expr


parserRoundtrip :: Property
parserRoundtrip = property $ do
  ptok <- forAll exprGen
  res <- evalEither $ Lisp.parseExpr =<< Lisp.lexer (parsedExprToSrc ptok)
  ptok === toUnitExpr res

-- sliceRoundtrip :: Property
-- sliceRoundtrip = property $ do
--   ptok <- forAll exprGen
--   let sourceLoc = parsedExprToSrc ptok
--   res <- evalEither $ Lisp.parseExpr =<< Lisp.lexer exprGen
--   ptok === toUnitExpr res

tests :: TestTree
tests = testGroup "Lexer and Parser Tests"
  [ testProperty "lexer roundtrip" lexerRoundtrip
  , testProperty "parser roundtrip" $ withTests (1000 :: TestLimit) parserRoundtrip
  -- , testProperty "slices roundtrip" $ withTests (1000 :: TestLimit) parserRoundtrip
  ]
