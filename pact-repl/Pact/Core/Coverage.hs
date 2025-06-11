{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Pact.Core.Coverage where

import Control.Monad

import Control.Lens
import Data.Text(Text)

import Pact.Core.Names
import Pact.Core.Coverage.Types
import Pact.Core.Environment
import Pact.Core.Info
import Pact.Core.Repl.Utils
import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.IR.Term (defInfo)

import Data.Map.Strict as M
import Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Kind as K

data CoverageBranch
  = CIfBranch
  | CElseBranch
  deriving (Show, Eq, Bounded, Enum)

class CoverageTick (i :: K.Type) (e :: RuntimeMode) where
  tickLine :: i -> EvalM e b i ()
  tickLineEnd :: i -> EvalM e b i ()
  tickFunctionStart :: FullyQualifiedName -> i -> EvalM e b i ()
  tickBranch :: CoverageBranch -> i -> EvalM e b i ()

whenCoverageEnabled :: ReplM b () -> ReplM b ()
whenCoverageEnabled action = do
  ReplCoverage enabled _ <- useReplState replCoverage
  when enabled action

mergeLineReport :: LineReport -> LineReport -> LineReport
mergeLineReport (LineReport line hits) (LineReport _ hits2) =
  LineReport line (hits + hits2)

mergeFunctionReport :: FunctionReport -> FunctionReport -> FunctionReport
mergeFunctionReport (FunctionReport name line hits) (FunctionReport _ _ hits2) =
  FunctionReport name line (hits + hits2)

mergeBranchReport :: BranchReport -> BranchReport -> BranchReport
mergeBranchReport (BranchReport line trueHits falseHits) (BranchReport _ trueHits2 falseHits2) =
  BranchReport line (trueHits + trueHits2) (falseHits + falseHits2)

mergeFileReport :: FileReport -> FileReport -> FileReport
mergeFileReport (FileReport file lines_ functions branches) (FileReport _ lines2 functions2 branches2) =
  FileReport file (IM.unionWith mergeLineReport lines_ lines2)
                  (M.unionWith mergeFunctionReport functions functions2)
                  (IM.unionWith mergeBranchReport branches branches2)

checkOrCreateFileReport :: FilePath -> ReplM b ()
checkOrCreateFileReport file = do
  reportExists <- M.member file <$> useReplState (replCoverage . covReport . lcovReport)
  unless reportExists $
    replCoverage . covReport . lcovReport %== M.insertWith mergeFileReport file (FileReport file IM.empty M.empty IM.empty)

instance CoverageTick i ExecRuntime where
  tickLine = const (pure ())
  tickLineEnd = const (pure ())
  tickFunctionStart _ _ = pure ()
  tickBranch _ _ = pure ()

tickLine' :: (SpanInfo -> Int) -> FileLocSpanInfo -> ReplM b ()
tickLine' acc (FileLocSpanInfo file info) = whenCoverageEnabled $ do
    let line = acc info
    let lineReport = LineReport line 1
        updateLine f = f & fileReportLines %~ IM.insertWith mergeLineReport line lineReport
    replCoverage . covReport . lcovReport %== M.adjust updateLine file

instance CoverageTick FileLocSpanInfo ReplRuntime where
  tickLine = tickLine' _liStartLine
  tickLineEnd = tickLine' _liEndLine
  tickFunctionStart fqn callInfo = whenCoverageEnabled $ do
    (FileLocSpanInfo file info) <- defInfo <$> lookupFqNameOrFail callInfo fqn
    let line = _liStartLine info
        functionReport = FunctionReport fqn line 1
        updateFunction f = f & fileReportFunctions %~ M.adjust (mergeFunctionReport functionReport) fqn
    replCoverage . covReport . lcovReport %== M.adjust updateFunction file
  tickBranch branch (FileLocSpanInfo file info) = whenCoverageEnabled $ do
    let line = _liStartLine info
        branchReport = case branch of
          CIfBranch -> BranchReport line 1 0
          CElseBranch -> BranchReport line 0 1
        updateBranch f = f & fileReportBranches %~ IM.insertWith mergeBranchReport line branchReport
    replCoverage . covReport . lcovReport %== M.adjust updateBranch file

-- | Ported from pact 4
showReport :: LcovReport -> Text
showReport (LcovReport fileReports) = T.unlines $ concatMap generateFileReport fileReports
  where
    generateFileReport (FileReport reportFile reportLines reportFunctions reportBranches) = concat
      [ [line "TN" []]
      , [line "SF" [T.pack reportFile]]
      , mmap tShowFunctionDefinition reportFunctions
      , mmap tShowFunctionHits reportFunctions
      , [line "FNF" [tShow $ length reportFunctions]]
      , [line "FNH" [countHits _frHits reportFunctions]]
      , concatMap generateBranchReport reportBranches
      , [line "BRF" [tShow $ length reportBranches * 2]] -- multiplying by 2 for true and false branches
      , [line "BRH" [countHitsIM branchReportHits reportBranches]]
      , fmap tShowLineReport (IM.elems reportLines)
      , [line "LF" [tShow $ length reportLines]]
      , [line "LH" [countHitsIM _lrHits reportLines]]
      , ["end_of_record"]
      ]

    mmap f = fmap (f . snd) . M.toList

    tShowFunctionDefinition (FunctionReport fqn frline _hits) = line "FN" [tShow frline, renderFullyQualName fqn ]

    tShowFunctionHits (FunctionReport fqn _ hits) = line "FNDA" [tShow hits, renderFullyQualName fqn]

    generateBranchReport (BranchReport brline trueHits falseHits) =
      let mkBranchLine branchNum hits = line "BRDA"
            [ tShow brline
            , tShow brline -- Todo: why do we care about branchReportHash?
            , tShow (branchNum :: Int)
            , tShow hits
            ]
      in [mkBranchLine 0 trueHits, mkBranchLine 1 falseHits]

    tShowLineReport (LineReport line_ hits) = line "DA" [tShow line_, tShow hits]

    {- Helpers -}
    tShow = T.pack . show

    line :: Text -> [Text] -> Text
    line label info = label <> ":" <> T.intercalate "," info

    countHits f = T.pack . show . length . M.filter ((> 0) . f)
    countHitsIM f = T.pack . show . length . IM.filter ((> 0) . f)

    branchReportHits (BranchReport _ brTrue brFalse) = brTrue + brFalse
