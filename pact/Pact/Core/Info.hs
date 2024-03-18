module Pact.Core.Info
 ( SpanInfo(..)
 , Position(..)
 , combineSpan
 , inside
 , termAt
 ) where

import Control.Applicative
import Control.Lens hiding (inside)
import Data.Default
import Data.Monoid
import GHC.Generics

import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.IR.Term

data Position
  = Position
  { _pLine :: !Int
  , _pCol :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance Default Position where
  def = Position 0 0

data SpanInfo
  = SpanInfo
  { _liStart :: !Position
  , _liEnd :: !Position
  } deriving (Eq, Ord, Show, Generic)

instance Default SpanInfo where
  def = SpanInfo def def

-- | Combine two Span infos
-- and spit out how far down the expression spans.
combineSpan :: SpanInfo -> SpanInfo -> SpanInfo
combineSpan (SpanInfo s _) (SpanInfo _ e) = SpanInfo s e

inside :: Position -> SpanInfo -> Bool
inside p (SpanInfo s e) = s <= p && p < e

termAt
  :: Position
  -> Term n t b SpanInfo
  -> Maybe (Term n t b SpanInfo)
termAt p term
  | p `inside` view termInfo term = case term of
      t@(Lam _ b _) -> termAt p b <|> Just t
      t@(App tm1 tm2 _) ->
        termAt p tm1 <|> getAlt (foldMap (Alt . termAt p) tm2) <|> Just t
      t@(Let _ tm1 tm2 _) -> termAt p tm1 <|> termAt p tm2 <|> Just t
      t@(Sequence tm1 tm2 _) -> termAt p tm1 <|> termAt p tm2 <|> Just t
      t@(Conditional op' _) ->
        case op' of
          CAnd a b  -> termAt p a <|> termAt p b
          COr a b   -> termAt p a <|> termAt p b
          CIf a b c -> termAt p a <|> termAt p b <|> termAt p c
          CEnforceOne a bs -> termAt p a <|> getAlt (foldMap (Alt . termAt p) bs)
          CEnforce a b -> termAt p a <|> termAt p b
        <|> Just t
      t@(ListLit tms _) -> getAlt (foldMap (Alt . termAt p) tms) <|> Just t
      t@(Try tm1 tm2 _) -> termAt p tm1 <|> termAt p tm2 <|> Just t
      t@(Nullary tm _) -> termAt p tm <|> Just t
      t@(ObjectLit l _) -> getAlt (foldMap (\(_, tm) -> Alt (termAt p tm)) l) <|> Just t
      t@(CapabilityForm cf _) -> termAtCapForm cf <|> Just t
      t -> Just t
  | otherwise = Nothing
  where
    termAtCapForm = \case
      WithCapability tm1 tm2 -> termAt p tm1 <|> termAt p tm2
      CreateUserGuard _ tms -> getAlt (foldMap (Alt . termAt p) tms)
