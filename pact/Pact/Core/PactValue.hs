{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.PactValue
 ( PactValue(..)
 , _PLiteral
 , _PList
 , _PGuard
 , _PCapToken
 , _PObject
 , ObjectData(..)
 , envMap
 , FQCapToken
 , pattern PInteger
 , _PInteger
 , pattern PDecimal
 , _PDecimal
 , pattern PString
 , _PString
 , pattern PBool
 , _PBool
 , pattern PUnit
 , _PUnit
 , synthesizePvType
 , pactValueToText
 ) where

import Control.Lens
import Data.Vector(Vector)
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Decimal

import Control.DeepSeq
import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Pact.Time as PactTime
import qualified Data.Set as S
import qualified Data.Text as T

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Literal
import Pact.Core.Pretty
import Pact.Core.ModRefs
import Pact.Core.Capabilities


data PactValue
  = PLiteral !Literal
  | PList !(Vector PactValue)
  | PGuard !(Guard QualifiedName PactValue)
  | PObject !(Map Field PactValue)
  | PModRef !ModRef
  | PCapToken !(CapToken FullyQualifiedName PactValue)
  | PTime !PactTime.UTCTime
  -- Note:
  -- This ord instance is dangerous. Be careful of comparisons with it
  deriving (Eq, Show, Ord, Generic)

instance NFData PactValue

makePrisms ''PactValue

pattern PInteger :: Integer -> PactValue
pattern PInteger i = PLiteral (LInteger i)
_PInteger :: Prism' PactValue Integer
_PInteger = _PLiteral . _LInteger

pattern PDecimal :: Decimal -> PactValue
pattern PDecimal d = PLiteral (LDecimal d)
_PDecimal :: Prism' PactValue Decimal
_PDecimal = _PLiteral . _LDecimal

pattern PString :: Text -> PactValue
pattern PString s = PLiteral (LString s)
_PString :: Prism' PactValue Text
_PString = _PLiteral . _LString

pattern PBool :: Bool -> PactValue
pattern PBool b = PLiteral (LBool b)
_PBool :: Prism' PactValue Bool
_PBool = _PLiteral . _LBool

pattern PUnit :: PactValue
pattern PUnit = PLiteral LUnit
_PUnit :: Prism' PactValue ()
_PUnit = _PLiteral . _LUnit

type FQCapToken = CapToken FullyQualifiedName PactValue

-- | ISO8601 Thyme format
simpleISO8601 :: String
simpleISO8601 = "%Y-%m-%dT%H:%M:%SZ"

formatLTime :: PactTime.UTCTime -> Text
formatLTime = T.pack . PactTime.formatTime simpleISO8601
{-# INLINE formatLTime #-}

instance Pretty PactValue where
  pretty = \case
    PLiteral lit -> pretty lit
    PList p -> brackets $ hsep $ (V.toList (pretty <$> p))
    PGuard g -> pretty g
    PObject o -> pretty (ObjectData o)
    PModRef md -> pretty md
    PCapToken (CapToken fqn args) ->
      "CapToken" <> pretty (CapToken (fqnToQualName fqn) args)
    PTime t -> dquotes $ pretty (formatLTime t)


pactValueToText :: PactValue -> Text
pactValueToText = \case
  PLiteral lit -> case lit of
    LString s -> tdquotes s
    LInteger i -> tshow i
    LDecimal d ->
      if roundTo 0 d == d then
        tshow (roundTo 0 d) <> ".0"
      else tshow d
    LUnit -> "()"
    LBool b -> if b then "true" else "false"
  PList l -> let
    l' = pactValueToText <$> V.toList l
    in tlist l'
  PGuard g -> case g of
    GKeyset (KeySet ks f) -> let
      keys = tlist (fmap _pubKey $ S.toList ks)
      p = predicateToText f
      in tcurly "KeySet" [("keys", keys), ("pred", p)]
    GKeySetRef ksn -> T.concat ["'", renderKeySetName ksn]
    GUserGuard (UserGuard f args) -> let
      f' = renderQualName f
      args' = tlist (pactValueToText <$> args)
      in tcurly "UserGuard" [("fun", f'), ("args", args')]
    GCapabilityGuard (CapabilityGuard n args pid) -> let
      mpid = [("pactId", maybe mempty _defPactId pid)]
      pvs = pactValueToText <$> args
      elems = [("name", renderQualName n), ("args", tlist pvs)] ++ mpid
      in tcurly "CapabilityGuard" elems
    GModuleGuard (ModuleGuard mn n) ->
      tcurly "ModuleGuard" [("module", renderModuleName mn), ("name", n)]
    GDefPactGuard (DefPactGuard pid n) ->
      tcurly "PactGuard" [("pactId", _defPactId pid), ("name", n)]
  PObject o -> let
    o' = fmap (\(Field f, pv) -> T.concat [tdquotes f, ": ", pactValueToText pv]) $ M.toList o
    in T.concat ["{",T.intercalate "," o', "}"]
  PModRef (ModRef mn _) ->
    renderModuleName mn
  PCapToken (CapToken qn args) -> let
    args' = if null args then mempty else " " <> T.intercalate " " (pactValueToText <$> args)
    qualName = fqnToQualName qn
    in T.concat ["CapToken(", renderQualName qualName, args',")"] -- Todo: check
  PTime t -> tdquotes $ formatLTime t
  where
    tdquotes x = T.concat ["\"",x,"\""]
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    tlist l = T.concat ["[",T.intercalate ", " l, "]"]
    tcurly :: Text -> [(Text, Text)] -> Text
    tcurly n l = let
      l' = fmap (\(k, v) -> T.concat [k, ": ", v]) l
      in T.concat [n, " {", T.intercalate "," l', "}"]

instance Pretty (AbbrevPretty PactValue) where
  pretty (AbbrevPretty plit) = case plit of
    PLiteral lit -> pretty (AbbrevPretty lit)
    PGuard g -> pretty (AbbrevPretty <$> g)
    PObject o ->
      braces $ prettyAbbrevText' 15 $ mconcat $ punctuate comma (objPair <$> M.toList o)
        where
        objPair (f, t) = dquotes (pretty f) <> ":" <+> pretty t
    PModRef md -> pretty md
    PCapToken (CapToken fqn args) ->
      pretty (CapToken fqn (AbbrevPretty <$> args))
    PTime t -> pretty (PactTime.formatTime "%Y-%m-%d %H:%M:%S%Q %Z" t)
    PList l ->
      brackets (prettyAbbrevText' 15 (hsep (pretty . AbbrevPretty <$> V.toList l)))

synthesizePvType :: PactValue -> Type
synthesizePvType = \case
  PLiteral l -> typeOfLit l
  PList _ -> TyAnyList
  PGuard _ -> TyGuard
  PModRef mr -> TyModRef (_mrImplemented mr)
  PObject _ -> TyAnyObject
  PCapToken {} -> TyCapToken
  PTime _ -> TyTime



newtype ObjectData term
  = ObjectData { _objectData :: Map Field term }
  deriving (Eq, Show, NFData)

envMap
  :: Lens (ObjectData term)
          (ObjectData term')
          (Map Field term)
          (Map Field term')
envMap f (ObjectData m) = fmap ObjectData (f m)

instance Pretty term => Pretty (ObjectData term) where
  pretty (ObjectData o) =
    braces $ mconcat $ punctuate comma (objPair <$> M.toList o)
      where
      -- SEMANTIC NOTE: this specific formatting matters, since it's what we use
      -- to pretty print and thus makes it into hashes
      objPair (f, t) = dquotes (pretty f) <> ":" <+> pretty t
