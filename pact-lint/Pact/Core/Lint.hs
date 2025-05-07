{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Pact.Core.Lint where

import Control.Monad.State
import Pact.Core.Builtin qualified as Core
-- import Pact.Core.Capabilities qualified as Core
import Pact.Core.IR.Term qualified as Core
import Pact.Core.Literal qualified as Core
import Pact.Core.Names qualified as Core
import Pact.Core.Type qualified as Core

type CoreName = Either Core.DeBruijn Core.ParsedName

-- jww (2024-02-02): Once I have Jose's new typechecking code, this will go
-- away.
data ExtType a
  = ExtType a
  | ExtFun [a] a
  deriving (Eq, Show)

type CoreTerm = Core.Term CoreName (ExtType Core.Type) Core.CoreBuiltin

data LintingState = LintingState

lintTerm ::
  CoreTerm (i, ExtType Core.Type) ->
  State LintingState ()
lintTerm = \case
  -- Need to check the type of the variable
  Core.Var name (_, _ty) ->
    case name of
      Left _idx -> undefined -- local
      Right _free -> undefined -- global
  Core.Nullary body _ -> lintTerm body
  Core.Lam _ body _ -> lintTerm body
  Core.Let _ value body _ -> do
    lintTerm value -- jww (2025-05-07): implement lexical scoping
    lintTerm body
  Core.App func args _ -> do
    mapM_ lintTerm args
    lintTerm func
  Core.BuiltinForm _bf _ ->
    undefined
  Core.Builtin _b _ ->
    undefined
  Core.Constant lit _ -> case lit of
    Core.LString _str -> undefined
    Core.LInteger _int -> undefined
    Core.LDecimal _dec -> undefined
    Core.LUnit -> undefined
    Core.LBool _bool -> undefined
  Core.Sequence t1 t2 _ -> do
    lintTerm t1
    lintTerm t2
  Core.ListLit xs _ ->
    mapM_ lintTerm xs
  Core.ObjectLit _obj _ ->
    undefined -- mapM lintTerm obj
  Core.InlineValue _v _ ->
    undefined
  -- Core.Try t1 t2 _ ->
  --   lintTerm e t1 $ \t1' ->
  --     lintTerm e t2 $ \t2' ->
  --       withEqTypes (reflectTerm t1') (reflectTerm t2') $
  --         k $
  --           Try t1' t2'
  -- Core.Conditional bf _ -> case bf of
  --   Core.CAnd o1 o2 ->
  --     lintTerm e o1 $ \(o1' :: LTerm e1 t1) ->
  --       lintTerm e o2 $ \(o2' :: LTerm e2 t2) ->
  --         withEqTypes (reflectTerm o1') SSTBool $
  --           withEqTypes (reflectTerm o2') SSTBool $
  --             k $
  --               Conditional (LCAnd o1' o2')
  --   Core.COr o1 o2 ->
  --     lintTerm e o1 $ \(o1' :: LTerm e1 t1) ->
  --       lintTerm e o2 $ \(o2' :: LTerm e2 t2) ->
  --         withEqTypes (reflectTerm o1') SSTBool $
  --           withEqTypes (reflectTerm o2') SSTBool $
  --             k $
  --               Conditional (LCOr o1' o2')
  --   Core.CIf obool otrue ofalse ->
  --     lintTerm e obool $ \(obool' :: LTerm e1 t1) ->
  --       withEqTypes SSTBool (reflectTerm obool') $
  --         lintTerm e otrue $ \(otrue' :: LTerm e2 t2) ->
  --           lintTerm e ofalse $ \(ofalse' :: LTerm e3 t3) ->
  --             withEqTypes (reflectTerm otrue') (reflectTerm ofalse') $
  --               k $
  --                 Conditional (LCIf obool' otrue' ofalse')
  --   Core.CEnforceOne msg bs ->
  --     lintTerm e msg $ \(msg' :: LTerm e1 t1) ->
  --       withEqTypes (reflectTerm msg') SSTStr $ do
  --         bs' <-
  --           mapM
  --             ( \x -> lintTerm e x $ \(x' :: LTerm e2 t2) ->
  --                 withEqTypes (reflectTerm x') SSTBool $
  --                   pure x'
  --             )
  --             bs
  --         k $ Conditional (LCEnforceOne msg' bs')
  --   Core.CEnforce b msg ->
  --     lintTerm e b $ \(b' :: LTerm e1 t1) ->
  --       lintTerm e msg $ \(msg' :: LTerm e2 t2) ->
  --         withEqTypes (reflectTerm b') SSTBool $
  --           withEqTypes (reflectTerm msg') SSTStr $
  --             k $
  --               Conditional (LCEnforce b' msg')
  -- Core.CapabilityForm cf _ ->
  --   case cf of
  --     Core.WithCapability c body ->
  --       lintTerm e c $ \c' ->
  --         case reflectTerm c' of
  --           ct@(SSTCapToken _nm) ->
  --             lintTerm e body $ \body' ->
  --               withEqTypes ct (reflectTerm body') $
  --                 k $
  --                   CapabilityForm $
  --                     WithCapability c' body'
  --           _ -> fail "Core.WithCapability passed a non-capability"
  --     Core.CreateUserGuard n _args -> case n of
  --       Right _name ->
  --         fail "lintTerm: NYI Core.CreateUserGuard"
  --       _ -> fail "Core.CreateUserGuard given index or non-bare name"
