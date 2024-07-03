module Pact.Core.Test.PrincipalTests(tests) where

import Data.Attoparsec.Text
import Data.Text

import Pact.Core.Principal

import Test.Tasty
import Test.Tasty.HUnit
import Pact.Core.Guards
import Pact.Core.Names

tests :: TestTree
tests = testGroup "PrincipalTests"
  [ kSpec
  , wSpec
  , rSpec
  , mSpec
  , uSpec
  , pSpec ]

-- | Default info is sufficient for this spec
--
kSpec :: TestTree
kSpec =
  testGroup "k:" [kRoundtrips, kCorrectIdents]
  where
    kRoundtrips =
      testCase "k: parser roundtrips" $ do
        -- principal -> text
        pk @?= Right k'
        -- text -> principal
        mkPrincipalIdent k' @?= k
    kCorrectIdents =
      testCase "k: has correct identifiers" $
        -- principal -> correct id
        fmap showPrincipalType pk @?= Right (showPrincipalType k')
    k = "k:584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68"
    k' = K (PublicKeyText "584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68")
    pk = parseOnly principalParser k

wSpec :: TestTree
wSpec =
  testGroup "w:" [wRoundtrips, wCorrectIdents]
  where
    wRoundtrips =
      testCase "w: parser roundtrips" $ do
        pw @?= Right w'
        mkPrincipalIdent w' @?= w
    wCorrectIdents =
      testCase "w: has correct identifiers" $
        fmap showPrincipalType pw @?= Right (showPrincipalType w')
    w = "w:5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew:keys-all"
    w' = W "5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew" "keys-all"
    pw = parseOnly principalParser w

rSpec :: TestTree
rSpec =
  testGroup "r:" [rRoundtrips, rCorrectIdents]
  where
    rRoundtrips =
      testCase "r: parser roundtrips" $ do
        pr @?= Right r'
        mkPrincipalIdent r' @?= r
    rCorrectIdents =
      testCase "r: has correct identifiers" $
        fmap showPrincipalType pr @?= Right (showPrincipalType r')
    r = "r:ks"
    r' = R (KeySetName "ks" Nothing)
    pr = parseOnly principalParser r

mSpec :: TestTree
mSpec =
  testGroup "m:" [mRoundtrips, mCorrectIdents]
  where
    mRoundtrips =
      testCase "m: parser roundtrips" $ do
        pm @?= Right m'
        mkPrincipalIdent m' @?= m
    mCorrectIdents =
      testCase "m: has correct identifiers" $
        fmap showPrincipalType pm @?= Right (showPrincipalType m')
    m = "m:test-ns.tester:tester"
    m' = M (ModuleName "tester" (Just (NamespaceName "test-ns"))) "tester"
    pm = parseOnly principalParser m

uSpec :: TestTree
uSpec =
  testGroup "u:" [uRoundtrips, uCorrectIdents]
  where
    uRoundtrips = testCase "u: parser roundtrips" $ do
      pu @?= Right u'
      mkPrincipalIdent u' @?= u
    uCorrectIdents =
       testCase "u: has correct identifiers" $
        fmap showPrincipalType pu @?= Right (showPrincipalType u')
    u :: Text
    u = "u:test-ns.tester.both-guard:aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    u' = U "test-ns.tester.both-guard" "aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    pu = parseOnly principalParser u

pSpec :: TestTree
pSpec = testGroup "p:"
  [ pRoundtrips
  , pCorrectIdents ]
  where
    pRoundtrips = testCase "p: parser roundtrips" $ do
      pp @?= Right p'
      mkPrincipalIdent p' @?= p
    pCorrectIdents = testCase "p: has correct identifiers" $
      fmap showPrincipalType pp @?= Right (showPrincipalType p')
    p = "p:DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g:pact-guard"
    p' = P (DefPactId "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g") "pact-guard"
    pp = parseOnly principalParser p
