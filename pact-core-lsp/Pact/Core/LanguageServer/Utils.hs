-- | 

module Pact.Core.LanguageServer.Utils where

import Control.Applicative ((<|>))
import Data.Monoid (Alt(..))

import Language.LSP.Protocol.Types
import Pact.Core.Info
import Pact.Core.Syntax.ParseTree
-- import Pact.Core.IR.Term
-- import Pact.Core.Builtin
import Control.Lens hiding (inside)

termAt
  :: Position
  -> ReplSpecialTL SpanInfo
  -> Maybe (ReplSpecialTL SpanInfo)
termAt p = \case
  RTL (RTLModule m) -> goModule m
  RTL (RTLInterface _inf) ->  Nothing
  RTL (RTLUse _imp _) -> Nothing
  RTL (RTLDefun df) -> goDefun df
  RTL (RTLDefConst dc) -> goDefConst dc
  RTL (RTLTerm term) -> RTL . RTLTerm <$> goTerm term
  RTLReplSpecial _ -> Nothing
  RTL _ -> Nothing
  where
    goTerm :: Expr SpanInfo -> Maybe (Expr SpanInfo)
    goTerm term = if p `inside` view termInfo term
                  then case term of
                         t@(Lam _ b _) -> goTerm b <|> Just t
                         t@(App tm1 tm2 _) ->
                           goTerm tm1 <|> getAlt (foldMap (Alt . goTerm) tm2) <|> Just t
                         t@(LetIn _ tm2 _) -> goTerm tm2 <|> Just t
                         -- t@(TyApp tm _ _) -> goTerm tm <|> Just t
                         -- t@(TyAbs _ tm _) -> goTerm tm <|> Just t
                         -- t@(Sequence tm1 tm2 _) -> goTerm tm1 <|> goTerm tm2 <|> Just t
                         _t@(Operator _op _) -> Nothing
                           -- (case op of
                           --     CAnd a b  -> goTerm a <|> goTerm b
                           --     COr a b   -> goTerm a <|> goTerm b
                           --     CIf a b c -> goTerm a <|> goTerm b <|> goTerm c) <|> Just t
                         -- t@(List _ tms _) -> getAlt (foldMap (Alt . goTerm) tms) <|> Just t 
                         -- t@(DynInvoke tm _ _) -> goTerm tm <|> Just t
                         t@(Try tm1 tm2 _) -> goTerm tm1 <|> goTerm tm2 <|> Just t
                         t -> Just t
                  else Nothing
    goDefun = undefined
    goDefConst = undefined
    goModule = undefined
    -- goDefun = \case
    --   t@(Defun _ _ tm i)
    --     | p `inside` i -> (RTLTerm <$> goTerm tm) <|> Just (RTLDefun t)
    --   _otherwise -> Nothing
    -- goDefConst = \case
    --   t@(DefConst _ _ tm i)
    --     | p `inside` i -> (RTLTerm <$> goTerm tm) <|> Just (RTLDefConst t)
    --   _otherwise -> Nothing
    -- goModule (Module _ defs _ _ _ _) = let
    --   goDef = \case
    --     Dfun t -> goDefun t
    --     DConst t -> goDefConst t
      -- in getAlt (foldMap (Alt . goDef) defs)

-- | Check if a `Position` is contained within a `Span`
inside :: Position -> SpanInfo -> Bool
inside pos (SpanInfo sl sc el ec) = sPos <= pos && pos < ePos
  where
    sPos = Position (fromIntegral sl) (fromIntegral sc)
    ePos = Position (fromIntegral el) (fromIntegral ec)
