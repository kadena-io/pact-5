-- |

module Pact.Core.LanguageServer.Utils where

import Control.Applicative ((<|>))
import Data.Monoid (Alt(..))

import Language.LSP.Protocol.Types
import Pact.Core.Info (SpanInfo(..), inside)
import qualified Pact.Core.Info as Pact
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.Imports

termAt
  :: Position
  -> EvalTerm ReplCoreBuiltin SpanInfo
  -> Maybe (EvalTerm ReplCoreBuiltin SpanInfo)
termAt p = Pact.termAt (toPactPosition p)

data PositionMatch b i
  = ModuleMatch (EvalModule b i)
  | InterfaceMatch (EvalInterface b i)
  | TermMatch (EvalTerm b i)
  | UseMatch Import i
  | DefunMatch (EvalDefun b i)
  | ConstMatch (EvalDefConst b i)
  | SchemaMatch (EvalSchema i)
  | TableMatch (EvalTable i)
  | DefPactMatch (EvalDefPact b i)
  | DefCapMatch (EvalDefCap b i)
  deriving Show

topLevelTermAt
  :: Position
  -> EvalTopLevel ReplCoreBuiltin SpanInfo
  -> Maybe (PositionMatch ReplCoreBuiltin SpanInfo)
topLevelTermAt lspPos = \case
  TLModule m -> goModule m
  TLInterface i -> goInterface i
  TLTerm t  -> TermMatch <$> Pact.termAt p t
  TLUse imp i
    | p `inside` i -> Just (UseMatch imp i)
    | otherwise -> Nothing
  where
    goInterface iface@(Interface _ _idefs _ _ i)
      | p `inside` i = Just (InterfaceMatch iface) -- TODO add interace defs
      | otherwise = Nothing
    goDefs = \case
      Dfun d@(Defun _ _ _ tm i)
        | p `inside` i -> TermMatch <$> Pact.termAt p tm <|> Just (DefunMatch d)
        | otherwise -> Nothing
      DConst d@(DefConst _ _ tc i)
        | p `inside` i -> (case tc of
                             TermConst tm -> TermMatch <$> Pact.termAt p tm
                             _ -> Nothing) <|> Just (ConstMatch d)
        | otherwise -> Nothing
      DCap dc@(DefCap _ _ _ tm _ i)
        | p `inside` i -> TermMatch <$> Pact.termAt p tm <|> Just (DefCapMatch dc)
        | otherwise -> Nothing
      DSchema ds@(DefSchema _ _ i)
        | p `inside` i -> Just (SchemaMatch ds)
        | otherwise -> Nothing
      DTable dt@(DefTable _ _ i)
        | p `inside` i -> Just (TableMatch dt)
        | otherwise -> Nothing
      DPact dp@(DefPact _ _ _ steps i)
        | p `inside` i -> getAlt (foldMap (Alt . goStep) steps) <|> Just (DefPactMatch dp)
        | otherwise -> Nothing
    goModule m@(Module _ _ defs _ _ _ _ i)
      | p `inside` i = getAlt (foldMap (Alt . goDefs) defs) <|> Just (ModuleMatch m)
      | otherwise = Nothing

    goStep = \case
      Step tm -> TermMatch <$> Pact.termAt p tm
      StepWithRollback tm1 tm2 -> TermMatch <$> (Pact.termAt p tm1 <|> Pact.termAt p tm2)

    p = toPactPosition lspPos

toPactPosition :: Position -> Pact.Position
toPactPosition (Position l ch) = Pact.Position (fromIntegral l) (fromIntegral ch)
