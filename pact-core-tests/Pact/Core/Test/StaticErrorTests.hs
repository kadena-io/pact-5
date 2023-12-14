{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.Test.StaticErrorTests(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Data.Text (Text)
import Data.Maybe(isJust)
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Evaluate
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.Errors

import Pact.Core.Test.TestPrisms

isDesugarError :: Prism' DesugarError a -> PactErrorI -> Bool
isDesugarError p s = isJust $ preview (_PEDesugarError . _1 . p) s

runStaticTest :: String -> Text -> (PactErrorI -> Bool) -> Assertion
runStaticTest label src predicate = do
  pdb <- mockPactDb
  let evalEnv = defaultEvalEnv pdb rawBuiltinMap
  v <- fst <$> evaluate evalEnv src
  case v of
    Left err ->
      assertBool ("Expected Error to match predicate, but got " <> show err <> " instead") (predicate err)
    Right _v -> assertFailure ("Error: Static failure test succeeded for test: " <> label)

staticTests :: [(String, PactErrorI -> Bool, Text)]
staticTests =
  [ ("no_bind_body", isDesugarError _EmptyBindingBody, [text|(bind {"a":1} {"a":=a})|])
  , ("defpact_last_step_rollback", isDesugarError _LastStepWithRollback, [text|
      (module m g (defcap g () true)

        (defpact f ()
          (step-with-rollback 1 1)
          )
        )
      |])
  , ("interface_defcap_meta_impl", isDesugarError _ImplementationError, [text|
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
  , ("enforce-one_no_list", isDesugarError _InvalidSyntax, [text|
      (module m g (defcap g () true)
        (defun enforce-cap ()
          (enforce-one "foo" 1)
          )
        )
      |])
  {- TODO unable to trigger Desugar.hs:336/344 in `desugarDefun`, parser gets there first
  , ("defun_outside_module", isDesugarError _NotAllowedOutsideModule, [text|
      (interface iface
        (defun foo:string (m:string) m)
        )
      |]) -}
  {- TODO ditto in desugarDefPact
  , ("defpact_empty", isDesugarError _EmptyDefPact, [text|
      (module m g (defcap g () true)
        (defpact f ())
        )
      |]) -}
  {- TODO ditto in desugarDefPact
  , ("defpact_outside_module", isDesugarError _NotAllowedOutsideModule, [text|
      (defpact f ()
        (step "step-0")
        )
      |]) -}
  {- TODO ditto in desugarDefCap
  , ("defcap_outside_module", isDesugarError _NotAllowedOutsideModule, [text|(defcap G () true)|]) -}
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
  ]

tests :: TestTree
tests =
  testGroup "CoreStaticTests" (go <$> staticTests)
  where
  go (label, p, srcText) = testCase label $ runStaticTest label srcText p
