{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Pact.Core.Debug
 ( DebugFlag(..)
 , DebugPrint(..)
 , debugPrint
 ) where

import Control.Monad.Reader
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Syntax.LexUtils(PosToken)
import Pact.Core.Environment
import qualified Pact.Core.Syntax.ParseTree as Syntax
import qualified Pact.Core.IR.Term as Term
import Pact.Core.Repl.Utils
import Pact.Core.Pretty

data DebugPrint b i term where
  DPLexer :: DebugPrint b i [PosToken]
  DPParser :: DebugPrint b i (Syntax.TopLevel i)
  DPDesugar :: DebugPrint b i (Term.TopLevel Name Type b i)

data DebugFlag
  = DFLexer
  | DFParser
  | DFDesugar
  deriving (Show, Eq, Ord, Enum, Bounded)

debugPrint :: (Pretty b) => DebugPrint b i term -> term -> EvalM e b i ()
debugPrint dp term =
  ask >>= \case
    ExecEnv _ -> pure ()
    ReplEnv _ -> do
      case dp of
        DPLexer -> whenReplFlagSet ReplDebugLexer $ liftIO $ do
          putStrLn "----------- Lexer output -----------------"
          print (pretty term)
        DPParser -> whenReplFlagSet ReplDebugParser $ case term of
          Syntax.TLTerm t ->
            liftIO $ do
              putStrLn "----------- Parser output ----------------"
              print (pretty t)
          _ -> pure ()
        DPDesugar -> whenReplFlagSet ReplDebugDesugar $ case term of
          Term.TLTerm t ->
            liftIO $ do
              putStrLn "----------- Desugar output ---------------"
              print (pretty t)
          _ -> pure ()

