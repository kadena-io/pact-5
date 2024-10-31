{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
module Pact.Core.Test.LexerParserTests where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Data.Text(Text)
import Data.List(intersperse)
import Control.Monad
import Control.Lens
import Data.List.NonEmpty(NonEmpty(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Text as T

import Pact.Core.Gen hiding
 (typeGen, argGen, defunGen, importGen, defGen, ifDefGen, stepGen)
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.LexUtils as Lisp
import Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import Pact.Core.Syntax.LexUtils (Token(..))
import Pact.Core.Literal
import Pact.Core.Pretty
import Pact.Core.Hash
import Pact.Core.Info
import Data.Typeable
import Pact.Core.Names

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

constantGen :: ParserGen
constantGen = (`Lisp.Constant` ()) <$> Gen.choice
  [ LString <$> Gen.text (Range.constant 1 64) Gen.alphaNum
  , LInteger <$> Gen.integral_(Range.constant (-10000) 10000)
  , LDecimal <$> decimalGen
  , Gen.constant LUnit
  , LBool <$> Gen.bool
  ]

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

    binderGen = do
      name <- identGen
      ty <- Gen.maybe typeGen
      expr <- Gen.subterm exprGen id
      pure $ Lisp.Binder name ty expr

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

margGen :: Gen (Lisp.MArg ())
margGen =
  MArg
   <$> identGen
   <*> Gen.maybe typeGen
   <*> pure ()

argGen :: Gen (Lisp.Arg ())
argGen =
  Arg
   <$> identGen
   <*> typeGen
   <*> pure ()

-- Todo in a followup: generate models
annGen :: Gen [PactAnn ()]
annGen =
  Gen.list (Range.constant 0 1) genDoc
  where
  genDoc =
    PactDoc
      -- Note: we don't want to generate PactDocStrings, because
      -- we can very easily hit our known parser ambiguity case and we just
      -- don't wanna deal with that.
      <$> pure PactDocAnn
      <*> docStringGen

docStringGen :: Gen Text
docStringGen = Gen.text (Range.constant 0 20) Gen.alphaNum


defunGen :: Gen (Defun ())
defunGen =
  Defun
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> defBodyGen
    <*> annGen
    <*> pure ()

-- NOTE:
-- We can't just use `exprGen` everywhere for things like
-- defuns and defcaps, because, unfortunately, there is parser ambiguity carried over
-- from legacy pact due to docstrings and string literals in expressions.
-- See [Docstring ambiguity] in Parser.y
defBodyGen :: Gen (NonEmpty (Expr ()))
defBodyGen = do
  h <- exprGenNoStringLit
  li <- Gen.list (Range.constant 0 5) exprGen
  pure (h :| li)

isNotStringLit :: Expr i -> Bool
isNotStringLit = \case
  Constant (LString _) _ -> False
  _ -> True

-- NOTE:
-- We can't just use `exprGen` everywhere for things like
-- defuns and defcaps, because, unfortunately, there is parser ambiguity carried over
-- from legacy pact due to docstrings and string literals in expressions.
-- See [Docstring ambiguity] in Parser.y
exprGenNoStringLit :: Gen (Expr ())
exprGenNoStringLit = do
  e <- exprGen
  e <$ guard (isNotStringLit e)


defcapGen :: Gen (DefCap ())
defcapGen =
  DefCap
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> defBodyGen
    <*> annGen
    <*> Gen.maybe metaGen
    <*> pure ()

metaGen :: Gen DCapMeta
metaGen = Gen.choice [pure DefEvent, managedGen]
  where
  managedGen =
    -- Todo: managed annotations have
    -- parser ambiguity this needs to be `Just` for this reason
    DefManaged . Just <$> ((,) <$> identGen <*> parsedNameGen)

defpactGen :: Gen (DefPact ())
defpactGen =
  DefPact
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> Gen.nonEmpty (Range.constant 1 5) stepGen
    <*> annGen
    <*> pure ()
  where
  stepGen =
    Gen.choice [regularStepGen, stepWithRbGen]
  regularStepGen =
    -- Todo: models
    Step <$> exprGen <*> pure Nothing
  stepWithRbGen =
    -- todo: models
    StepWithRollback <$> exprGen <*> exprGen <*> pure Nothing

defschemaGen :: Gen (DefSchema ())
defschemaGen =
  DefSchema
    <$> identGen
    <*> Gen.list (Range.constant 1 5) argGen
    <*> annGen
    <*> pure ()

deftableGen :: Gen (DefTable ())
deftableGen =
  DefTable
    <$> identGen
    <*> parsedNameGen
    <*> Gen.maybe ((,PactDocAnn) <$> docStringGen)
    <*> pure ()

defconstGen :: Gen (DefConst ())
defconstGen =
  DefConst
    <$> margGen
    <*> exprGen
    <*> Gen.maybe ((,PactDocAnn) <$> docStringGen)
    <*> pure ()

defGen :: Gen (Lisp.Def ())
defGen =
  Gen.choice
    [ Dfun <$> defunGen
    , DCap <$> defcapGen
    , DPact <$> defpactGen
    , DSchema <$> defschemaGen
    , DTable <$> deftableGen
    , DConst <$> defconstGen
    ]

importGen :: Gen (Import ())
importGen = do
  mn <- moduleNameGen
  mh <- Gen.maybe hashTextGen
  imp <- Gen.maybe (Gen.list (Range.linear 0 5) identGen)
  pure (Import mn mh imp ())

hashTextGen :: Gen Text
hashTextGen =
  moduleHashToText <$> moduleHashGen

extDeclGen :: Gen (ExtDecl ())
extDeclGen =
  Gen.choice [
    ExtBless <$> hashTextGen <*> pure (),
    ExtImport <$> importGen,
    ExtImplements <$> moduleNameGen <*> pure ()
  ]

moduleGen :: Gen (Module ())
moduleGen =
  Module
    <$> identGen
    <*> govGen
    <*> Gen.list (Range.constant 1 5) extDeclGen
    <*> Gen.nonEmpty (Range.constant 1 5) defGen
    <*> annGen
    <*> pure ()
  where
  govGen = Gen.choice
    [ KeyGov <$> identGen
    , CapGov <$> identGen
    ]

ifdefunGen :: Gen (IfDefun ())
ifdefunGen =
  IfDefun
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> annGen
    <*> pure ()

ifdefcapGen :: Gen (IfDefCap ())
ifdefcapGen =
  IfDefCap
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> annGen
    <*> Gen.maybe metaGen
    <*> pure ()

ifdefpactGen :: Gen (IfDefPact ())
ifdefpactGen =
  IfDefPact
    <$> margGen
    <*> Gen.list (Range.constant 1 5) margGen
    <*> annGen
    <*> pure ()

ifDefGen :: Gen (IfDef ())
ifDefGen =
  Gen.choice
  [ IfDfun <$> ifdefunGen
  , IfDCap <$> ifdefcapGen
  , IfDSchema <$> defschemaGen
  , IfDPact <$> ifdefpactGen
  , IfDConst <$> defconstGen
  ]

interfaceGen :: Gen (Interface ())
interfaceGen =
  Interface
    <$> identGen
    <*> Gen.list (Range.constant 1 5) ifDefGen
    <*> Gen.list (Range.constant 0 5) importGen
    <*> annGen
    <*> pure ()

topLevelGen :: Gen (TopLevel ())
topLevelGen =
  Gen.choice
  [ TLTerm <$> exprGen
  , TLModule <$> moduleGen
  , TLInterface <$> interfaceGen
  ]

exprParserRoundtrip :: Property
exprParserRoundtrip = property $ do
  ptok <- forAll exprGen
  res <- evalEither $ Lisp.parseExpr =<< Lisp.lexer (renderCompactText ptok)
  ptok === void res

parserRoundtrip :: Property
parserRoundtrip = property $ do
  ptok <- forAll topLevelGen
  res <- evalEither $ Lisp.parseProgram =<< Lisp.lexer (renderCompactText ptok)
  [ptok] === (void <$> res)

data RenderTest =
  forall a. (Eq a, Show a, Typeable a) => MkRenderTest
  { _renderGen :: Gen a
  , _renderText :: a -> Text
  , _renderParse :: Text -> Maybe a
  , _renderProxy :: Proxy a
  }

renderTest :: (Eq a, Typeable a, Show a) => Gen a -> (a -> Text) -> (Text -> Maybe a) -> RenderTest
renderTest gen render parse = MkRenderTest gen render parse Proxy

parseExprMaybe :: Text -> Maybe (Lisp.Expr ())
parseExprMaybe = fmap void . either (const Nothing) Just . (Lisp.lexer >=> Lisp.parseExpr)

runRenderTest :: RenderTest -> TestTree
runRenderTest (MkRenderTest gen render parse prx) =
  testProperty ("Render/parse test for: " <> show (typeRep prx)) $ withTests (1000 :: TestLimit) $
    property $ do
    v <- forAll gen
    parse (render v) === Just v

renderTests :: [RenderTest]
renderTests =
    -- Render test litmus using expr
  [ renderTest exprGen renderCompactText parseExprMaybe
  , renderTest fullyQualifiedNameGen renderFullyQualName parseFullyQualifiedName
  , renderTest moduleNameGen renderModuleName parseModuleName
  , renderTest tableNameGen jsonSafeRenderTableName parseJsonSafeTableName
  , renderTest parsedTyNameGen renderParsedTyName parseParsedTyName
  , renderTest hashedModuleNameGen renderHashedModuleName parseHashedModuleName
  ]

-- | Here we will test that slicing from generated source
--   will produce accurate source locations
--
--   If our parser + lexer generate correct source locations for toplevels, then
--   it also means we should be able to parse, then slice the locations, then concatenate them again
--   to yield the original source
--
sliceRoundtrip :: Property
sliceRoundtrip = property $ do
  toplevels <- forAll (Gen.list (Range.constant 1 3) topLevelGen)
  let srcText = T.concat $ intersperse " " $ renderCompactText <$> toplevels
  res <- evalEither $ Lisp.parseProgram =<< Lisp.lexer srcText
  let slices = T.concat $ intersperse " " $ sliceFromSource srcText . view topLevelInfo <$> res
  srcText === slices

tests :: TestTree
tests = testGroup "Lexer and Parser Tests"
  [ testProperty "lexer roundtrip" lexerRoundtrip
  , testProperty "parser roundtrips for all toplevels" $ withTests (1000 :: TestLimit) parserRoundtrip
  , testProperty "source slices correspond with their source code locations" $ withTests (1000 :: TestLimit) sliceRoundtrip
  , testGroup "Render/parse tests" $ runRenderTest <$> renderTests
  ]
