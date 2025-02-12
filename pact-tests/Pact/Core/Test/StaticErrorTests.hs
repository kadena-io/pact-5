{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.Test.StaticErrorTests(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Control.Lens
import Data.IORef
import Data.Text (Text)
import Data.Functor (void)
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Info

isParseError :: Prism' ParseError a -> PactError FileLocSpanInfo -> Bool
isParseError p s = has (_PEParseError . _1 . p) s

isDesugarError :: Prism' DesugarError a -> PactError FileLocSpanInfo -> Bool
isDesugarError p s = has (_PEDesugarError . _1 . p) s

isExecutionError :: Prism' EvalError a -> PactError FileLocSpanInfo -> Bool
isExecutionError p s = has (_PEExecutionError . _1 . p) s

isUserRecoverableError :: Prism' UserRecoverableError a -> PactError FileLocSpanInfo -> Bool
isUserRecoverableError p s = has (_PEUserRecoverableError . _1 . p) s

runStaticTest :: String -> Text -> ReplInterpreter -> (PactError FileLocSpanInfo -> Bool) -> Assertion
runStaticTest label src interp predicate = do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let source = SourceCode label src
      rstate = mkReplState ee (const (const (pure ()))) (\f reset -> void (loadFile interp f reset))
                & replCurrSource .~ source
                & replNativesEnabled .~ True
  stateRef <- newIORef rstate
  v <- evalReplM stateRef (interpretReplProgram interp source)
  case v of
    Left err ->
      assertBool ("Expected Error to match predicate, but got " <> show err <> " instead") (predicate err)
    Right _v -> assertFailure ("Error: Static failure test succeeded for test: " <> label)

parseTests :: [(String, PactError FileLocSpanInfo -> Bool, Text)]
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

desugarTests :: [(String, PactError FileLocSpanInfo -> Bool, Text)]
desugarTests =
  [ ("no_bind_body", isDesugarError _EmptyBindingBody, [text|(bind {"a":1} {"a":=a})|])
  , ("defpact_last_step_rollback", isDesugarError _LastStepWithRollback, [text|
      (module m g (defcap g () true)

        (defpact f ()
          (step-with-rollback 1 1)
          )
        )
      |])
  , ("user_guard_no_app", isDesugarError _InvalidSyntax, [text|
      (create-user-guard 1)
      |])

  , ("empty_if", isDesugarError _InvalidSyntax, [text|
      (if)
      |])

  , ("if_invalid_args", isDesugarError _InvalidSyntax, [text|
      (if 1)
      |])

  , ("empty_do", isDesugarError _InvalidSyntax, [text|
      (do)
      |])

  , ("empty_with_cap", isDesugarError _InvalidSyntax, [text|
      (with-capability)
      |])

  , ("one_arg_with_cap", isDesugarError _InvalidSyntax, [text|
      (with-capability 1)
      |])

  , ("empty_enforce_one", isDesugarError _InvalidSyntax, [text|
      (enforce-one)
      |])

  , ("one_arg_enforce_one", isDesugarError _InvalidSyntax, [text|
      (enforce-one 1)
      |])

  , ("empty_suspend", isDesugarError _InvalidSyntax, [text|
      (suspend)
      |])

  , ("n-ary_suspend", isDesugarError _InvalidSyntax, [text|
      (suspend 2 3 4 5 6)
      |])

  , ("empty_cond", isDesugarError _InvalidSyntax, [text|
      (cond)
      |])

  , ("invalid_cond", isDesugarError _InvalidSyntax, [text|
      (cond 1 2 3)
      |])

  , ("empty_create-user-guard", isDesugarError _InvalidSyntax, [text|
      (create-user-guard)
      |])

  , ("n-ary_create-user-guard", isDesugarError _InvalidSyntax, [text|
      (create-user-guard 2 3 4 5 6)
      |])

  , ("empty_try", isDesugarError _InvalidSyntax, [text|
      (try)
      |])

  , ("n-ary_try", isDesugarError _InvalidSyntax, [text|
      (try 2 3 4 5 6)
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
      (module modl G (defcap G () true))

      (module other-mod OG (defcap OG () true)
        (defun foo:string (a:string b:module{modl}) a)
        )
      |])
  , ("interface_instead_of_module_same", isDesugarError _NoSuchModule, [text|
      (module modl G (defcap G () true)
        (defun foo:string (a:string b:module{modl}) a)
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
  , ("module_implements_module", isDesugarError _InvalidModuleReference, [text|
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
  , ("shadowing_disallowed_letbinds", isDesugarError _InvalidNativeShadowing, [text|
    (let ((identity 1)) 1)
  |])
  , ("shadowing_disallowed_letbinds_nested", isDesugarError _InvalidNativeShadowing, [text|
    (let ((i (let* ((identity 1)) 2))) 1)
  |])
  , ("shadowing_disallowed_lambdas", isDesugarError _InvalidNativeShadowing, [text|
    (lambda (identity) identity)
  |])
  , ("shadowing_disallowed_module_name", isDesugarError _InvalidNativeShadowing, [text|
    (module identity g
      (defcap g () true)
      (defun foo () 1)
    )
  |])
  , ("shadowing_disallowed_defun_name", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defun identity () 1)
    )
  |])
  , ("shadowing_disallowed_defun_arg", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defun f (identity:string) 1)
    )
  |])
  , ("shadowing_disallowed_defun_inner_term", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defun f (foo:string)
        (let ((f (lambda (identity) 1))) 2)
      )
    )
  |])
  , ("shadowing_disallowed_defcap_name", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defcap identity () 1)
    )
  |])
  , ("shadowing_disallowed_defcap_arg", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defcap f (identity:string) 1)
    )
  |])
  , ("shadowing_disallowed_defcap_inner_term", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defcap f (foo:string)
        (let ((f (lambda (identity) 1))) 2)
      )
    )
  |])
  , ("shadowing_disallowed_defpact_name", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defpact identity () (step 2))
    )
  |])
  , ("shadowing_disallowed_defpact_arg", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defpact f (identity:string) (step 1))
    )
  |])
  , ("shadowing_disallowed_defpact_inner_term", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defpact f (foo:string) (step (lambda (identity) 1)))
    )
  |])
  , ("shadowing_disallowed_defconst", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defconst identity 1)
    )
  |])
  , ("shadowing_disallowed_defconst_term", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defconst foo (let ((identity 1)) 2))
    )
  |])
  , ("shadowing_disallowed_defschema", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defschema identity a:integer)
    )
  |])
  , ("shadowing_disallowed_deftable", isDesugarError _InvalidNativeShadowing, [text|
    (module m g
      (defcap g () true)
      (defschema ident-schema a:integer)
      (deftable identity:{ident-schema})
    )
  |])
  , ("shadowing_disallowed_interface", isDesugarError _InvalidNativeShadowing, [text|
    (interface identity
     (defun foobar:integer (a:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defun_name", isDesugarError _InvalidNativeShadowing, [text|
    (interface foobar
     (defun identity:integer (a:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defun_arg", isDesugarError _InvalidNativeShadowing, [text|
    (interface foointerface
     (defun foobar:integer (identity:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defcap_name", isDesugarError _InvalidNativeShadowing, [text|
    (interface foobar
     (defcap identity:integer (a:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defcap_arg", isDesugarError _InvalidNativeShadowing, [text|
    (interface foointerface
     (defcap foobar:integer (identity:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defpact_name", isDesugarError _InvalidNativeShadowing, [text|
    (interface foobar
     (defpact identity:integer (a:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defpact_arg", isDesugarError _InvalidNativeShadowing, [text|
    (interface foointerface
     (defpact foobar:integer (identity:integer))
    )
  |])
  , ("shadowing_disallowed_interface_defconst", isDesugarError _InvalidNativeShadowing, [text|
    (interface foointerface
     (defconst identity foobar)
    )
  |])
  ]

executionTests :: [(String, PactError FileLocSpanInfo -> Bool, Text)]
executionTests =
  [ ("enforce_ns_install_module", isExecutionError _RootNamespaceInstallError, [text|
      (module m g (defcap g () true)
        (defun manage (ns guard) true)
        )
      (env-namespace-policy false (manage))

      (module another ag (defcap ag () true))
    |])
  , ("enforce-one_no_list", isExecutionError _NativeExecutionError, [text|
      (enforce-one "asdf" 1)
      |])

  , ("enforce_ns_install_interface", isExecutionError _RootNamespaceInstallError, [text|
      (module m g (defcap g () true)
        (defun manage (ns guard) true)
        )
      (env-namespace-policy false (manage))

      (interface iface
        (defun foo:string ())
        )
    |])
  , ("enforce_ns_define_namespace", isExecutionError _NativeExecutionError, [text|
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
  , ("reexposed_module_missing_name", isExecutionError _ModuleMemberDoesNotExist, [text|
      (module m g
        (defcap g () true)

        (defun f () 1)

        (defschema foo-schema elem:guard)
        (deftable foo:{foo-schema})
        )
      (create-table foo)
      (insert foo "k" {"elem":(create-user-guard (f))})

      (module m g
        (defcap g () true)
        (defschema foo-schema elem:guard)
        (deftable foo:{foo-schema})
        )

      (enforce-guard (at "elem" (read foo "k")))
      |])
  , ("module_gov_keyset_nonexistent", isExecutionError _NoSuchKeySet, [text|
      (module m 'nonexistent (defun f () true))
      |])
  , ("module_gov_keyset_different", isUserRecoverableError _KeysetPredicateFailure, [text|
      (env-data {"ks":["jose"]})
      (define-keyset 'somekeyset (read-keyset 'ks))
      (module m 'somekeyset (defun f () 1))
      |])
  , ("module_gov_keyset_empty", isExecutionError _InvalidKeysetNameFormat, [text|
      (module m "" (defun f () true))
      |])
  , ("module_gov_keyset_not_in_sigs", isUserRecoverableError _KeysetPredicateFailure, [text|
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
  , ("entity_not_allowed", isExecutionError _EntityNotAllowedInDefPact, [text|
      (module m g (defcap g () true)
        (defpact tester ()
          (step 1 2)
        )
        )
    |])
  , ("entity_not_allowed_rb", isExecutionError _EntityNotAllowedInDefPact, [text|
      (module m g (defcap g () true)
        (defpact tester ()
          (step-with-rollback 1 2 3)
        )
        )
    |])
  ]

builtinTests :: [(String, PactError FileLocSpanInfo -> Bool, Text)]
builtinTests =
  [ ("integer_pow_negative", isExecutionError _ArithmeticException, "(^ 0 -1)")
  , ("floating_pow_negative", isExecutionError _FloatingPointError, "(^ 0.0 -1.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_pow_negative_flr", isExecutionError _FloatingPointError, "(^ 0 -1.0)")
  , ("mixed_pow_negative_fll", isExecutionError _FloatingPointError, "(^ 0.0 -1)")
  , ("integer_log_negative_base", isExecutionError _ArithmeticException, "(log -1 10)")
  , ("integer_log_negative_arg", isExecutionError _ArithmeticException, "(log 2 -1)")
  , ("integer_log_zero_arg", isExecutionError _ArithmeticException, "(log 2 0)")
  , ("floating_log_negative_base", isExecutionError _ArithmeticException, "(log -1.0 10.0)")
  , ("floating_log_negative_arg", isExecutionError _ArithmeticException, "(log 2.0 -1.0)")
  , ("floating_log_zero_arg", isExecutionError _ArithmeticException, "(log 2.0 0.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_log_negative_base_fll", isExecutionError _ArithmeticException, "(log -1.0 10)")
  , ("mixed_log_negative_arg_fll", isExecutionError _ArithmeticException, "(log 2.0 -1)")
  , ("mixed_log_zero_arg_fll", isExecutionError _ArithmeticException, "(log 2.0 0)")
  , ("mixed_log_negative_base_flr", isExecutionError _ArithmeticException, "(log -1 10.0)")
  , ("mixed_log_negative_arg_flr", isExecutionError _ArithmeticException, "(log 2 -1.0)")
  , ("mixed_log_zero_arg_flr", isExecutionError _ArithmeticException, "(log 2 0.0)")
  , ("integer_div_zero", isExecutionError _ArithmeticException, "(/ 2 0)")
  , ("floating_div_zero", isExecutionError _ArithmeticException, "(/ 2.0 0.0)")
  -- TODO better error messages for the mixed ones
  , ("mixed_div_zero_fll", isExecutionError _ArithmeticException, "(/ 2.0 0)")
  , ("mixed_div_zero_flr", isExecutionError _ArithmeticException, "(/ 2 0.0)")
  , ("integer_square_negative", isExecutionError _ArithmeticException, "(sqrt -1)")
  , ("floating_square_negative", isExecutionError _ArithmeticException, "(sqrt -1.0)")

  , ("enumerate_up_diverging", isExecutionError _EnumerationError, "(enumerate 0 10 -1)")
  , ("enumerate_down_diverging", isExecutionError _EnumerationError, "(enumerate 10 0 1)")
  , ("at_oob_bigger", isExecutionError _ArrayOutOfBoundsException, "(at 100 [1 2 3])")
  , ("at_oob_bound", isExecutionError _ArrayOutOfBoundsException, "(at 3 [1 2 3])")
  , ("at_oob_smaller", isExecutionError _ArrayOutOfBoundsException, "(at -1 [1 2 3])")
  , ("at_oob_empty", isExecutionError _ArrayOutOfBoundsException, "(at 0 [])")
  , ("at_key_missing", isExecutionError _ObjectIsMissingField, "(at 'bar { 'foo: 1 })")
  , ("yield_outside", isExecutionError _YieldOutsideDefPact, "(yield {})")
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

forkingNameResolutionTests :: [(String, PactError FileLocSpanInfo -> Bool, Text)]
forkingNameResolutionTests =
  [ ("pact51_hash-keccak_does_not_resolve", isDesugarError _NoSuchModule, [text|
      (env-exec-config ["DisablePact51"])
      (hash-keccak256 [""])
      |])

  , ("pact51_hash-poseidon_does_not_resolve", isDesugarError _NoSuchModule, [text|
      (env-exec-config ["DisablePact51"])
      (hash-poseidon 1 2 3)
      |])]

tests :: TestTree
tests =
  testGroup "CoreStaticTests"
    [ testGroup "CoreStaticTests:CEK" (go interpretEvalBigStep <$> allTests)
    , testGroup "CoreStaticTests:Direct" (go interpretEvalDirect <$> allTests)
    ]
  where
  allTests = parseTests <> desugarTests <> executionTests <> builtinTests <> forkingNameResolutionTests
  go interp (label, p, srcText) = testCase label $ runStaticTest label srcText interp p
