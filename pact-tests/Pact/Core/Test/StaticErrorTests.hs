{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.Test.StaticErrorTests(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Control.Lens
import Data.IORef
import Data.Text (Text)
import Data.Default
import Data.Maybe(isJust)
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Gas
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise (serialisePact_repl_spaninfo)
import Pact.Core.Test.TestPrisms

isParseError :: Prism' ParseError a -> PactErrorI -> Bool
isParseError p s = isJust $ preview (_PEParseError . _1 . p) s

isDesugarError :: Prism' DesugarError a -> PactErrorI -> Bool
isDesugarError p s = isJust $ preview (_PEDesugarError . _1 . p) s

isExecutionError :: Prism' EvalError a -> PactErrorI -> Bool
isExecutionError p s = isJust $ preview (_PEExecutionError . _1 . p) s

runStaticTest :: String -> Text -> (PactErrorI -> Bool) -> Assertion
runStaticTest label src predicate = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replCoreBuiltinMap
  let source = SourceCode label src
      rstate = ReplState
            { _replFlags = mempty
            , _replEvalState = def
            , _replPactDb = pdb
            , _replGas = gasRef
            , _replEvalLog = gasLog
            , _replCurrSource = source
            , _replEvalEnv = ee
            , _replUserDocs = mempty
            , _replTLDefPos = mempty
            , _replTx = Nothing
            }
  stateRef <- newIORef rstate
  v <- runReplT stateRef (interpretReplProgram source (const (pure ())))
  case v of
    Left err ->
      assertBool ("Expected Error to match predicate, but got " <> show err <> " instead") (predicate err)
    Right _v -> assertFailure ("Error: Static failure test succeeded for test: " <> label)

parseTests :: [(String, PactErrorI -> Bool, Text)]
parseTests =
  [ ("defpact_empty", isParseError _ParsingError, [text|
      (module m g (defcap g () true)
        (defpact f ())
        )
      |])
  , ("defpact_outside_module", isParseError _ParsingError, [text|
      (defpact f ()
        (step "step-0")
        )
      |])
  , ("defcap_outside_module", isParseError _ParsingError, [text|
      (defcap G () true)
      |])
  ]

desugarTests :: [(String, PactErrorI -> Bool, Text)]
desugarTests =
  [ ("no_bind_body", isDesugarError _EmptyBindingBody, [text|(bind {"a":1} {"a":=a})|])
  , ("defpact_last_step_rollback", isDesugarError _LastStepWithRollback, [text|
      (module m g (defcap g () true)

        (defpact f ()
          (step-with-rollback 1 1)
          )
        )
      |])
  , ("enforce-one_no_list", isDesugarError _InvalidSyntax, [text|
      (module m g (defcap g () true)
        (defun enforce-cap ()
          (enforce-one "foo" 1)
          )
        )
      |])
  , ("managed_invalid", isDesugarError _InvalidManagedArg, [text|
      (module mgd-mod G
        (defcap G () true)
        (defcap C:bool (id:string) @managed notId foo true)
        (defun foo:string (a:string b:string) a)
        )
      |])
  , ("import_invalid_set", isDesugarError _InvalidImports, [text|
      (module m mg (defcap mg () true))

      (module n ng (defcap ng () true)
        (use m [ nonexistent ])
        )
      |])
  , ("module_instead_of_interface", isDesugarError _InvalidModuleReference, [text|
      (interface iface
        (defun foo:string (a:integer b:integer))
        )
      iface
      |])
  , ("interface_instead_of_module", isDesugarError _InvalidModuleReference, [text|
      (module mod G (defcap G () true))

      (module other-mod OG (defcap OG () true)
        (defun foo:string (a:string b:module{mod}) a)
        )
      |])
  , ("interface_instead_of_module_same", isDesugarError _NoSuchModule, [text|
      (module mod G (defcap G () true)
        (defun foo:string (a:string b:module{mod}) a)
        )
      |])
  , ("import_unknown_module", isDesugarError _NoSuchModule, [text|
      (module m g (defcap g () true)
        (use nonexistent)
        )
      |])
  , ("import_unknown_module_self", isDesugarError _NoSuchModule, [text|
      (module m g (defcap g () true)
        (use m)
        )
      |])
  , ("import_unknown_module_namespaced_self", isDesugarError _NoSuchModule, [text|
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (namespace 'carl)
      (module m g (defcap g () true)
        (use carl.m)
        )
      |])
  , ("import_unknown_module_namespaced_outside", isDesugarError _NoSuchModule, [text|
      (begin-tx)
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (namespace 'carl)
      (module m g (defcap g () true))
      (commit-tx)

      (module n ng (defcap ng () true)
        (use carl.n)
        )
      |])
    -- TODO better error
  , ("invalid_schema_iface_wrong_name", isDesugarError _NoSuchModule, [text|
      (interface iface
        (defconst c:object{nonexistent} { 'flag:true })
        )
      |])
    -- TODO better error
  , ("invalid_schema_iface_wrong_ref", isDesugarError _NoSuchModule, [text|
      (interface iface
        (defun i ())
        (defconst c:object{i} { 'flag:true })
        )
      |])
    -- TODO better error
  , ("invalid_schema_iface_wrong_ref_qual", isDesugarError _NoSuchModuleMember, [text|
      (interface iface
        (defun i ())
        )
      (interface iface2
        (defconst c:object{m.i} { 'flag:true })
        )
      |])
    -- TODO better error
  , ("invalid_schema_mod_wrong_name", isDesugarError _NoSuchModule, [text|
      (module m g (defcap g () true)
        (defconst c:object{nonexistent} { 'flag:true })
        )
      |])
  , ("invalid_schema_mod_wrong_ref", isDesugarError _InvalidDefInSchemaPosition, [text|
      (module m g (defcap g () true)
        (defun i () true)
        (defconst c:object{i} { 'flag:true })
        )
      |])
  , ("invalid_schema_mod_wrong_ref_other", isDesugarError _InvalidDefInSchemaPosition, [text|
      (module m g (defcap g () true)
        (defun i () true)
      )
      (interface iface2
        (defconst c:object{m.i} { 'flag:true })
        )
      |])
  , ("invalid_var_kind", isDesugarError _InvalidDefInTermVariable, [text|
      (module m g (defcap g () true)
        (defschema p flag:bool)
        (defun i () p)
      )
      |])
    -- TODO better error; intended to trigger `UnboundTypeVariable` instead in `renameDefTable`
  , ("invalid_def_table_nonexistent", isDesugarError _NoSuchModule, [text|
      (module fdb G
        (defcap G () true)
        (deftable fdb-tbl:{fdb-test})
        )
      |])
  , ("invalid_def_table_wrong_type", isDesugarError _InvalidDefInSchemaPosition, [text|
      (module fdb G
        (defcap G () true)
        (defun i () true)
        (deftable fdb-tbl:{i})
        )
      |])
    -- TODO better errror; intended to trigger `expectedFree` instead
  , ("defmanaged_wrong_ref", isDesugarError _NoSuchModule, [text|
      (module m g (defcap g () true)
        (defcap CAP:bool (a:integer b:integer)
          @managed a b
          true
        )
        )
      |])
  , ("invalid_ifdefcap_ref_mod", isDesugarError _NoSuchModuleMember, [text|
      (module m g (defcap g () true)
        (defun i () true)
        )
      (interface iface
        (defcap CAP:bool (a:integer)
          @managed a m
        )
        )
      |])
  , ("invalid_ifdefcap_ref_qual", isDesugarError _NoSuchModuleMember, [text|
      (module m g (defcap g () true)
        (defun i () true)
        )
      (interface iface
        (defcap CAP:bool (a:integer)
          @managed a m.i
        )
        )
      |])
  , ("resolve_qualified_failure", isDesugarError _NoSuchModuleMember, [text|
      (module m g (defcap g () true)
        (defun f () true)
        )
      (module n ng (defcap ng () true)
        (defun f () m.nonexistent)
        )
      |])
  , ("resolve_qualified_shadowing", isDesugarError _NoSuchModuleMember, [text|
      (module m g (defcap g () true)
        (defun fff () true)
        )
      (module n ng (defcap ng () true)
        (defun f () m.f)
        )
      |])
  , ("dyninvoke_unbound", isDesugarError _UnboundTermVariable, [text|
      (module m g (defcap g () true)
        (defun i () true)
        )
      (module n ng (defcap ng () true)
        (defun g () m::i)
        )
      |])
  , ("dyninvoke_invalid_bound", isDesugarError _InvalidDynamicInvoke, [text|
      (defun f () 1)
      (defun invalid-dynamic-invoke () (f::g))
      |])
  , ("cyclic_defs", isDesugarError _RecursionDetected, [text|
      (module m g (defcap g () true)
        (defun f1 () f2)
        (defun f2 () f1)
        )
      |])
  , ("cyclic_defs_longer", isDesugarError _RecursionDetected, [text|
      (module m g (defcap g () true)
        (defun f1 () f2)
        (defun f2 () f3)
        (defun f3 () f1)
        )
      |])
  , ("cyclic_defcap", isDesugarError _RecursionDetected, [text|
      (module m1 g (defcap g () true)
        (defcap c ()
          (with-capability (c) 1)
          )
        )
      |])
  , ("dup_defs", isDesugarError _DuplicateDefinition, [text|
      (module m g (defcap g () true)
        (defun f () true)
        (defun f () false)
        )
      |])
  , ("dup_defs_different_kind", isDesugarError _DuplicateDefinition, [text|
      (module m g (defcap g () true)
        (defun f () true)
        (defconst f true)
        )
      |])
  , ("governance_wrong", isDesugarError _InvalidGovernanceRef, [text|
      (module m g (defconst g true))
      |])
  , ("governance_nonexistent", isDesugarError _InvalidGovernanceRef, [text|
      (module m g (defconst k true))
      |])
  , ("module_implements_nonexistent", isDesugarError _NoSuchModule, [text|
      (module m g (defcap g () true)
        (implements nonexistent)
        )
      |])
  , ("module_implements_module", isDesugarError _NoSuchInterface, [text|
      (module notiface ng (defcap ng () true))

      (module m g (defcap g () true)
        (implements notiface)
        )
      |])
  , ("module_implements_dfun_missing", isDesugarError _NotImplemented, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        )
      |])
  , ("module_implements_dfun_wrong_kind", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defconst f true)
        )
      |])
  , ("module_implements_dfun_wrong_ret", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun f:bool (a:integer b:integer) true)
        )
      |])
  , ("module_implements_dfun_wrong_args_type", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun f:bool (a:integer b:bool) true)
        )
      |])
  , ("module_implements_dfun_wrong_args_count_less", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun f:bool (a:integer) true)
        )
      |])
  , ("module_implements_dfun_wrong_args_count_more", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun f:bool (a:integer b:integer c:integer) true)
        )
      |])
  , ("module_implements_dfun_wrong_args_unspec", isDesugarError _ImplementationError, [text|
      (interface iface
        (defun f:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun f:integer (a b) a)
        )
      |])
  , ("module_implements_defcap_missing", isDesugarError _NotImplemented, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        )
      |])
  , ("module_implements_defcap_wrong_kind", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun CAP:bool (a:integer) true)
        )
      |])
  , ("module_implements_defcap_wrong_meta", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer)
          @managed a CAP-MGR
        )
        (defun CAP-MGR:integer (a:integer b:integer))
        )

      (module m g (defcap g () true)
        (implements iface)

        (defcap CAP:bool (a:integer) true)
        (defun CAP-MGR:integer (a:integer b:integer) 1)
        )
      |])
  , ("module_implements_defcap_wrong_args_count_more", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defcap CAP:bool (a:integer b:bool) true)
        )
      |])
  , ("module_implements_defcap_wrong_args_count_less", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defcap CAP:bool () true)
        )
      |])
  , ("module_implements_defcap_wrong_args_type", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defcap CAP:bool (a:bool) true)
        )
      |])
  , ("module_implements_defcap_wrong_ret_type", isDesugarError _ImplementationError, [text|
      (interface iface
        (defcap CAP:bool (a:integer))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defcap CAP:integer (a:integer) 1)
        )
      |])
  , ("module_implements_defpact_missing", isDesugarError _NotImplemented, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        )
      |])
  , ("module_implements_defpact_wrong_kind", isDesugarError _ImplementationError, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defun p:string (arg:string) arg)
        )
      |])
  , ("module_implements_defpact_wrong_ret_type", isDesugarError _ImplementationError, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defpact p:bool (arg:string)
          (step "step-0"))
        )
      |])
  , ("module_implements_defpact_wrong_arg_type", isDesugarError _ImplementationError, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defpact p:string (arg:bool)
          (step "step-0"))
        )
      |])
  , ("module_implements_defpact_wrong_arg_count_less", isDesugarError _ImplementationError, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defpact p:string ()
          (step "step-0"))
        )
      |])
  , ("module_implements_defpact_wrong_arg_count_more", isDesugarError _ImplementationError, [text|
      (interface iface
        (defpact p:string (arg:string))
        )

      (module m g (defcap g () true)
        (implements iface)
        (defpact p:string (arg:string b:bool)
          (step "step-0"))
        )
      |])
  , ("module_use_invalid_hash", isDesugarError _InvalidBlessedHash, [text|
      (module m g (defcap g () true))

      (use m "definitelynotanykindofhash")
      |])
  , ("module_use_wrong_hash", isDesugarError _InvalidImportModuleHash, [text|
      (module m g (defcap g () true))

      (use m "A_fIcwIweiXXYXnKU59CNCAUoIXHXwQtB_D8xhEflLY")
      |])
  , ("with_capability_in_defcap", isDesugarError _NotAllowedWithinDefcap, [text|
      (module m1 g (defcap g () true)
        (defcap c1 () true)
        (defcap c2 ()
          (with-capability (c1) 1)
          )
        )
    |])
  ]

executionTests :: [(String, PactErrorI -> Bool, Text)]
executionTests =
  [ ("enforce_ns_install_module", isExecutionError _NamespaceInstallError, [text|
      (module m g (defcap g () true)
        (defun manage (ns guard) true)
        )
      (env-namespace-policy false (manage))

      (module another ag (defcap ag () true))
    |])
  , ("enforce_ns_install_interface", isExecutionError _NamespaceInstallError, [text|
      (module m g (defcap g () true)
        (defun manage (ns guard) true)
        )
      (env-namespace-policy false (manage))

      (interface iface
        (defun foo:string ())
        )
    |])
  , ("enforce_ns_define_namespace", isExecutionError _DefineNamespaceError, [text|
      (module m g (defcap g () true)
        (defun manage (ns guard) false)
        )
      (env-namespace-policy false (manage))

      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (namespace 'carl)
    |])
  , ("interface_upgrade", isExecutionError _CannotUpgradeInterface, [text|
      (interface iface
        (defun foo:string ())
        )
      (interface iface
        (defun foo:string ())
        (defun bar:string ())
        )
    |])
  , ("interface_module", isExecutionError _ExpectedModule, [text|
      (interface iface
        (defun foo:string ())
        )
      (module iface g (defcap g () true))
    |])
  , ("modref_namespace", isExecutionError _ExpectedModule, [text|
      (begin-tx)
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (namespace 'carl)
      (interface iface
        (defun foo:string ())
        )
      (commit-tx)

      carl.iface
    |])
  , ("import_unknown_module_namespaced_self_nons", isExecutionError _ModuleDoesNotExist, [text|
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (namespace 'carl)
      (module m g (defcap g () true)
        (use m)
        )
      |])
  , ("get_module_unknown", isExecutionError _ModuleDoesNotExist, [text|
      (describe-module 'nonexistent)
      |])
  , ("module_gov_keyset_nonexistent", isExecutionError _NoSuchKeySet, [text|
      (module m 'nonexistent (defun f () true))
      |])
  , ("module_gov_keyset_different", isExecutionError _EvalError, [text|
      (env-data {"ks":["jose"]})
      (define-keyset 'somekeyset (read-keyset 'ks))
      (module m 'somekeyset (defun f () 1))
      |])
  , ("module_gov_keyset_empty", isExecutionError _ModuleGovernanceFailure, [text|
      (module m "" (defun f () true))
      |])
  , ("module_gov_keyset_not_in_sigs", isExecutionError _EvalError, [text|
      (env-data { "kall": ["a" "b" "c"], "kadmin": ["admin"] })
      (define-keyset 'kall)
      (define-keyset 'kadmin)

      (env-keys ["admin"])
      (module m 'kall (defun f () true))
      |])
  , ("defconst_not_a_value_module", isExecutionError _EvalError, [text|
      (module m g (defcap g () true)
        (defconst not-a-value (lambda (x) x))
        )
      |])
  , ("defconst_not_a_value_iface", isExecutionError _EvalError, [text|
      (interface iface
        (defconst not-a-value (lambda (x) x))
        )
      |])

  -- CEK errors
  , ("modref_no_ns", isExecutionError _ModRefNotRefined, [text|
      (module m g (defcap g () true))
      m
      |])

  , ("defpact_not_init", isExecutionError _NoDefPactIdAndExecEnvSupplied, [text|
      (module m g (defcap g () true))
      (continue-pact 1)
      |])
  ] <> let simpleDefpact = [text|
      (module m g (defcap g () true)
        (defpact p:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        )|] in
  [ ("defpact_args", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      $simpleDefpact
      (p 'meh)
      |])
  , ("defpact_continuing_completed_samestep", isExecutionError _DefPactAlreadyCompleted, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 2)
      (continue-pact 3)
      |])
  , ("defpact_continuing_completed_samestep", isExecutionError _DefPactAlreadyCompleted, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 2)
      (continue-pact 2)
      |])
  , ("defpact_continuing_completed_prevstep", isExecutionError _DefPactAlreadyCompleted, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 2)
      (continue-pact 1)
      |])
  , ("defpact_continuing_firststep", isExecutionError _DefPactStepMismatch, [text|
      $simpleDefpact
      (p)
      (continue-pact 0)
      |])
  , ("defpact_continuing_incomplete_samestep", isExecutionError _DefPactStepMismatch, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 1)
      |])
  , ("defpact_continuing_incomplete_badstep", isExecutionError _InvalidDefPactStepSupplied, [text|
      $simpleDefpact
      (p)
      (continue-pact 100)
      |])
  , ("defpact_continuing_incomplete_negstep", isExecutionError _InvalidDefPactStepSupplied, [text|
      $simpleDefpact
      (p)
      (continue-pact (- 1))
      |]) -- TODO (-1) here errors out
  , ("defpact_same_nested", isExecutionError _MultipleOrNestedDefPactExecFound, [text|
      $simpleDefpact
      (p)
      (p)
      |])
  , ("defpact_same_nested_inprogress", isExecutionError _MultipleOrNestedDefPactExecFound, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (p)
      |])
  , ("defpact_continuing_norollback_samestep", isExecutionError _DefPactStepHasNoRollback, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 1 true)
      |])
  , ("defpact_continuing_norollback_diffstep", isExecutionError _DefPactStepHasNoRollback, [text|
      $simpleDefpact
      (p)
      (continue-pact 0 true)
      |])
  , ("defpact_continuing_norollback_diffstep", isExecutionError _DefPactRollbackMismatch, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 2 true)
      |])
  , ("defpact_continuing_rollback_final", isExecutionError _DefPactAlreadyCompleted, [text|
      $simpleDefpact
      (p)
      (continue-pact 1)
      (continue-pact 2)
      (continue-pact 2 true)
      |])
  ] <>
  [ ("defpact_nested_stepcount", isExecutionError _NestedDefPactParentStepCountMismatch, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          )
        )
      (p)
      |])
  , ("defpact_nested_already_started_before", isExecutionError _MultipleOrNestedDefPactExecFound, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          (step (continue (n)))
          (step (continue (n)))
          )
        )
      (n)
      (p)
      |])
  , ("defpact_nested_already_started_after", isExecutionError _MultipleOrNestedDefPactExecFound, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          (step (continue (n)))
          (step (continue (n)))
          )
        )
      (p)
      (n)
      |])
  , ("defpact_nested_rollback_mismatch_outer_missing", isExecutionError _NestedDefPactParentRollbackMismatch, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step-with-rollback "hello2" "bye2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          (step (continue (n)))
          (step (continue (n)))
          )
        )
      (p)
      (continue-pact 1)
      (continue-pact 1 true)
      |])
  , ("defpact_nested_rollback_mismatch_inner_missing", isExecutionError _NestedDefPactParentRollbackMismatch, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          (step-with-rollback (continue (n)) "bye-outer")
          (step (continue (n)))
          )
        )
      (p)
      (continue-pact 1)
      (continue-pact 1 true)
      |])
  -- TODO better error type here? v
  , ("defpact_nested_dynamic_step_count_mismatch", isExecutionError _NestedDefPactNeverStarted, [text|
      (module m g (defcap g () true)
        (defpact n:string ()
          (step "hello1")
          (step "hello2")
          (step "hello3")
          )
        (defpact p:string ()
          (step (n))
          (step (+ (continue (n)) (continue (n))))
          (step (continue (n)))
          )
        )
      (p)
      (continue-pact 1)
      |])
  , ("defpact_nested_double_execution", isExecutionError _NestedDefPactDoubleExecution, [text|
      (module m g (defcap g () true)
        (defpact n1:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact n2:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact p:string ()
          (step (n1))
          (step (n2))
        )
        )
      (p)
      (continue-pact 1)
      |])
  , ("defpact_nested_advance_all", isExecutionError _NestedDefpactsNotAdvanced, [text|
      (module m g (defcap g () true)
        (defpact n1:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact n2:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact p:string ()
          (step [(n1) (n2)])
          (step ["hello2" "hello2"])
        )
        )
      (p)
      (continue-pact 1)
      |])
  , ("defpact_nested_advance_some", isExecutionError _NestedDefpactsNotAdvanced, [text|
      (module m g (defcap g () true)
        (defpact n1:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact n2:string ()
          (step "hello1")
          (step "hello2")
        )
        (defpact p:string ()
          (step [(n1) (n2)])
          (step [(n1) "hello2"])
        )
        )
      (p)
      (continue-pact 1)
      |])
  ] <>
  [ ("env_namespace_wrong_kind", isExecutionError _NativeArgumentsError, [text|
      (module m g (defcap g () true))
      (env-namespace-policy false (m.g))
    |])
  , ("changing_hash_makes_it_nonblessed", isExecutionError _HashNotBlessed, [text|
      (define-keyset 'k (sig-keyset))

      (module impure 'k
        (defconst VERSION 1)
        (defschema foo-schema value:integer)
        (deftable foo:{foo-schema})
        (defun ins (k v) (insert foo k v)))

      (module dep 'k
        (defun dep-impure (k v) (ins k { "value": v })))

      (module impure 'k
        (defconst VERSION 2)
        (defschema foo-schema value:integer)
        (deftable foo:{foo-schema})
        (defun ins (k v) (insert foo k v)))

      (dep.dep-impure "b" 1)
    |])
  , ("managed_auto_cap_not_installed", isExecutionError _CapNotInstalled, [text|
      (module m g (defcap g () true)
        (defcap PAY (sender:string receiver:string amount:integer)
          @managed
          (enforce-keyset (read-keyset sender))
          )
        (defun pay (sender receiver amount)
          (with-capability (PAY sender receiver amount) amount)
          )
        )

      (env-data { "alice": ["alice"] })
      (env-keys ["alice"])

      (pay "alice" "bob" 10)
    |])
  , ("managed_cap_not_installed", isExecutionError _CapNotInstalled, [text|
      (module m g (defcap g () true)
        (defcap PAY (sender:string receiver:string amount:integer)
          @managed amount PAY-mgr
          (enforce-keyset (read-keyset sender))
          )
        (defun PAY-mgr (mgd req) 0)
        (defun pay (sender receiver amount)
          (with-capability (PAY sender receiver amount) amount)
          )
        )

      (env-data { "alice": ["alice"] })
      (env-keys ["alice"])

      (pay "alice" "bob" 10)
    |])
  , ("emit_event_module_mismatch", isExecutionError _EventDoesNotMatchModule, [text|
      (module m1 g (defcap g () true)
        (defcap ev () @event true)
        )
      (module m2 g (defcap g () true)
        (defun emit-ev ()
          (emit-event (ev))
          )
        )
      (emit-ev)
    |])
  , ("install_cap_not_managed", isExecutionError _InvalidManagedCap, [text|
      (module m g (defcap g () true)
        (defcap c:bool ()
          true)
        )
      (install-capability (c))
    |])
  , ("partial_defun_user", isExecutionError _CannotApplyPartialClosure, [text|
      (module m g (defcap g () true)
        (defun f (n:integer s:string) s)
        )
      ((f 1) "foo")
    |])
  , ("partial_defun_native", isExecutionError _CannotApplyPartialClosure, [text|
      ((take 1) "foo")
    |])
  , ("closure_too_many_user", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (module m g (defcap g () true)
        (defun f (n:integer s:string) s)
        )
      (f 1 "foo" "bar")
    |])
  , ("closure_too_many_native", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (take 1 "foo" "bar")
    |])
  , ("closure_too_many_user_map", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (module m g (defcap g () true)
        (defun f (n:integer s:string) s)
        )
      (map (f 1 "foo") ["meh"])
    |])
  , ("closure_too_many_native_map", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (map (take 1 "foo") ["meh"])
    |])
  , ("closure_too_many_user_map_unary", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (module m g (defcap g () true)
        (defun f () 1)
        )
      (map f ["meh"])
    |])
  , ("closure_too_many_defcap", isExecutionError _ClosureAppliedToTooManyArgs, [text|
      (module m g (defcap g () true)
        (defcap c:bool ()
          @managed
          true)
        )
      (install-capability (c "meh"))
    |])
  ]

builtinTests :: [(String, PactErrorI -> Bool, Text)]
builtinTests =
  [ ("integer_pow_negative", isExecutionError _ArithmeticException, "(^ 0 -1)")
  , ("floating_pow_negative", isExecutionError _FloatingPointError, "(^ 0.0 -1.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_pow_negative_flr", isExecutionError _NativeArgumentsError, "(^ 0 -1.0)")
  , ("mixed_pow_negative_fll", isExecutionError _NativeArgumentsError, "(^ 0.0 -1)")
  , ("integer_log_negative_base", isExecutionError _ArithmeticException, "(log -1 10)")
  , ("integer_log_negative_arg", isExecutionError _ArithmeticException, "(log 2 -1)")
  , ("integer_log_zero_arg", isExecutionError _ArithmeticException, "(log 2 0)")
  , ("floating_log_negative_base", isExecutionError _ArithmeticException, "(log -1.0 10.0)")
  , ("floating_log_negative_arg", isExecutionError _ArithmeticException, "(log 2.0 -1.0)")
  , ("floating_log_zero_arg", isExecutionError _ArithmeticException, "(log 2.0 0.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_log_negative_base_fll", isExecutionError _NativeArgumentsError, "(log -1.0 10)")
  , ("mixed_log_negative_arg_fll", isExecutionError _NativeArgumentsError, "(log 2.0 -1)")
  , ("mixed_log_zero_arg_fll", isExecutionError _NativeArgumentsError, "(log 2.0 0)")
  , ("mixed_log_negative_base_flr", isExecutionError _NativeArgumentsError, "(log -1 10.0)")
  , ("mixed_log_negative_arg_flr", isExecutionError _NativeArgumentsError, "(log 2 -1.0)")
  , ("mixed_log_zero_arg_flr", isExecutionError _NativeArgumentsError, "(log 2 0.0)")
  , ("integer_div_zero", isExecutionError _ArithmeticException, "(/ 2 0)")
  , ("floating_div_zero", isExecutionError _ArithmeticException, "(/ 2.0 0.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_div_zero_fll", isExecutionError _NativeArgumentsError, "(/ 2.0 0)")
  , ("mixed_div_zero_flr", isExecutionError _NativeArgumentsError, "(/ 2 0.0)")
  , ("integer_square_negative", isExecutionError _ArithmeticException, "(sqrt -1)")
  , ("floating_square_negative", isExecutionError _ArithmeticException, "(sqrt -1.0)")

  , ("enumerate_up_diverging", isExecutionError _EnumerationError, "(enumerate 0 10 -1)")
  , ("enumerate_down_diverging", isExecutionError _EnumerationError, "(enumerate 10 0 1)")
  , ("at_oob_bigger", isExecutionError _ArrayOutOfBoundsException, "(at 100 [1 2 3])")
  , ("at_oob_bound", isExecutionError _ArrayOutOfBoundsException, "(at 3 [1 2 3])")
  , ("at_oob_smaller", isExecutionError _ArrayOutOfBoundsException, "(at -1 [1 2 3])")
  , ("at_oob_empty", isExecutionError _ArrayOutOfBoundsException, "(at 0 [])")
  , ("at_key_missing", isExecutionError _EvalError, "(at 'bar { 'foo: 1 })")
  , ("yield_outside", isExecutionError _YieldOutsiteDefPact, "(yield {})")
  , ("resume_no_defpact", isExecutionError _NoActiveDefPactExec, "(resume { 'field := binder } binder)")
  , ("resume_no_yield", isExecutionError _NoYieldInDefPactStep, [text|
      (module m g (defcap g () true)
        (defpact p ()
          (step "hello")
          (step (resume { 'field := binder } binder)))
        )

      (p)
      (continue-pact 1)
    |])
  , ("yield_provenance_mismatch", isExecutionError _YieldProvenanceDoesNotMatch, [text|
      (module m g (defcap g () true)
        (defpact p ()
          (step (yield { 'field: 1 } "1"))
          (step (resume { 'field := binder } binder)))
        )

      (env-chain-data { 'chain-id: "0" })
      (p)
      (continue-pact 1)
    |])
  , ("toplevel_create_table", isExecutionError _NativeIsTopLevelOnly, [text|
      (module m g (defcap g () true)
        (defschema s a:integer)
        (deftable t:{s})
        (defun f () (create-table t))
        )
      (f)
    |])
  , ("toplevel_describe_table", isExecutionError _NativeIsTopLevelOnly, [text|
      (module m g (defcap g () true)
        (defschema s a:integer)
        (deftable t:{s})
        (defun f () (describe-table t))
        )
      (f)
    |])
  , ("toplevel_define_keyset", isExecutionError _NativeIsTopLevelOnly, [text|
      (env-data { "kall": ["a" "b" "c"] })
      (module m g (defcap g () true)
        (defun f () (define-keyset 'kall))
        )
      (f)
    |])
  , ("toplevel_define_keyset_guarded", isExecutionError _NativeIsTopLevelOnly, [text|
      (env-data { "kall": ["a" "b" "c"] })
      (module m g (defcap g () true)
        (defun f () (define-keyset 'kall (sig-keyset)))
        )
      (f)
    |])
  , ("toplevel_describe_module", isExecutionError _NativeIsTopLevelOnly, [text|
      (module m g (defcap g () true)
        (defun f () (describe-module 'm))
        )
      (f)
    |])
  , ("toplevel_describe_keyset", isExecutionError _NativeIsTopLevelOnly, [text|
      (env-data { "kall": ["a" "b" "c"] })
      (define-keyset 'kall)
      (module m g (defcap g () true)
        (defun f () (describe-keyset 'kall))
        )
      (f)
    |])
  , ("toplevel_namespace", isExecutionError _NativeIsTopLevelOnly, [text|
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys))
      (module m g (defcap g () true)
        (defun f () (namespace 'carl))
        )
      (f)
    |])
  , ("toplevel_define_namespace", isExecutionError _NativeIsTopLevelOnly, [text|
      (env-data { "carl-keys" : ["carl"], "carl.carl-keys": ["carl"] })
      (env-keys ["carl"])

      (module m g (defcap g () true)
        (defun f () (define-namespace 'carl (read-keyset 'carl-keys) (read-keyset 'carl-keys)))
        )
      (f)
    |])
  , ("b64decode", isExecutionError _DecodeError, [text|(base64-decode "foobar!")|])
  , ("b64decode-str-to-int", isExecutionError _DecodeError, [text|(str-to-int 64 "foobar!")|])
  , let long = T.replicate 513 "0" in
    ("str-to-int-too-long", isExecutionError _DecodeError, [text|(str-to-int "$long")|])
  , let long = T.replicate 513 "0" in
    ("str-to-int-base-too-long", isExecutionError _DecodeError, [text|(str-to-int 2 "$long")|])
  , ("str-to-int-empty", isExecutionError _DecodeError, [text|(str-to-int "")|])
  , ("str-to-int-base-empty", isExecutionError _DecodeError, [text|(str-to-int 2 "")|])
  , ("keyset_def_ns_mismatch", isExecutionError _MismatchingKeysetNamespace, [text|
      (env-exec-config ["RequireKeysetNs"])
      (env-data
        { "alice-keys" : ["alice"]
        , "bob-keys"   : ["bob"]
        , "alice.alice-keys": ["alice"]
        , "bob.bob-keys" : ["bob"]
        })
      (env-keys ["alice", "bob"])

      (define-namespace 'alice
        (read-keyset 'alice-keys)
        (read-keyset 'alice-keys))

      (define-namespace 'bob
        (read-keyset 'bob-keys)
        (read-keyset 'bob-keys))
      (namespace 'alice)
      (define-keyset "bob.bob-keys")
    |])
  , ("emit_event_unmanaged", isExecutionError _InvalidEventCap, [text|
      (module m g (defcap g () true))
      (emit-event (g))
    |])
  ]

tests :: TestTree
tests =
  testGroup "CoreStaticTests" (go <$> parseTests <> desugarTests <> executionTests <> builtinTests)
  where
  go (label, p, srcText) = testCase label $ runStaticTest label srcText p
