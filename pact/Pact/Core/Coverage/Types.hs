{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Coverage.Types
  ( LineReport(..)
  , lrLine, lrHits
  , FunctionReport(..)
  , frName, frLine, frHits
  , BranchReport(..)
  , brLine, brTrueHits, brFalseHits
  , FileReport(..)
  , fileReportFile, fileReportLines, fileReportFunctions, fileReportBranches
  , LcovReport(..)
  , lcovReport
  ) where

import Control.Lens
import Data.Map.Strict as M
import Data.IntMap.Strict as IM

import Pact.Core.Names


data LineReport
  = LineReport
  { _lrLine :: Int
  , _lrHits :: Int
  } deriving (Show, Eq)

data FunctionReport
  = FunctionReport
  { _frName :: FullyQualifiedName
  , _frLine :: Int
  , _frHits :: Int
  } deriving (Show, Eq)

data BranchReport
  = BranchReport
  { _brLine :: Int
  , _brTrueHits :: Int
  , _brFalseHits :: Int
  } deriving (Show, Eq)

data FileReport
  = FileReport
  { _fileReportFile :: FilePath
  , _fileReportLines :: IM.IntMap LineReport
  , _fileReportFunctions :: M.Map FullyQualifiedName FunctionReport
  , _fileReportBranches :: IM.IntMap BranchReport
  } deriving (Show, Eq)

newtype LcovReport = LcovReport
  { _lcovReport :: M.Map FilePath FileReport }
  deriving (Show, Eq)

makeLenses ''LineReport
makeLenses ''FunctionReport
makeLenses ''BranchReport
makeLenses ''FileReport
makeLenses ''LcovReport