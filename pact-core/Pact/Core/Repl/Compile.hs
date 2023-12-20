{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}



module Pact.Core.Repl.Compile
 ( ReplCompileValue(..)
 , interpretReplProgram
 , interpretReplProgramSmallStep
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import Data.Default
import System.FilePath(takeFileName)


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Info
import Pact.Core.Serialise (serialisePact_repl_spaninfo)


import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK(CEKEval)
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

type Repl = ReplM ReplRawBuiltin

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue SpanInfo)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  deriving Show

loadFile
  :: FilePath
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
loadFile loc display = do
  source <- SourceCode (takeFileName loc) <$> liftIO (T.readFile loc)
  replCurrSource .= source
  interpretReplProgram source display


-- replEnv :: BuiltinEnv Runtime.CEKBigStep ReplRawBuiltin SpanInfo (ReplM ReplRawBuiltin)
-- replEnv = replBuiltinEnv @Runtime.CEKBigStep

interpretReplProgram
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgram = interpretReplProgram' (replBuiltinEnv @CEKBigStep)

interpretReplProgramSmallStep
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgramSmallStep = interpretReplProgram' (replBuiltinEnv @CEKSmallStep)


interpretReplProgram'
  :: (CEKEval step ReplRawBuiltin SpanInfo Repl)
  => BuiltinEnv step ReplRawBuiltin SpanInfo Repl
  -> SourceCode
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgram' replEnv (SourceCode _ source) display = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse pipe parsed
  where
  displayValue p = p <$ display p
  pipe = \case
    Lisp.RTL rtl ->
      pure <$> pipe' rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _
        | b -> do
          oldSrc <- use replCurrSource
          evalState .= def
          pactdb <- liftIO (mockPactDb serialisePact_repl_spaninfo)
          replPactDb .= pactdb
          replEvalEnv .= defaultEvalEnv pactdb replRawBuiltinMap
          out <- loadFile (T.unpack txt) display
          replCurrSource .= oldSrc
          pure out
        | otherwise -> do
          oldSrc <- use replCurrSource
          oldEs <- use evalState
          oldEE <- use replEvalEnv
          when b $ evalState .= def
          out <- loadFile (T.unpack txt) display
          replEvalEnv .= oldEE
          evalState .= oldEs
          replCurrSource .= oldSrc
          pure out
  pipe' tl = case tl of
    Lisp.RTLTopLevel toplevel -> do
      v <- interpretTopLevel replEnv toplevel
      displayValue (RCompileValue v)
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue $ RLoadedDefun $ _dfunName df
      RTLDefConst dc -> do
        let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (DConst dc)
        displayValue $ RLoadedDefConst $ _dcName dc
