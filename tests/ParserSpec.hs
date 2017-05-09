{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Apps.Juno.Parser
import Data.Ratio
import Data.Text as T
import Juno.Hoplite.Term
import Juno.Hoplite.Types (Literal(..))

spec :: Spec
spec = do
  describe "admins" testAdmins
  describe "hopperLite" testHopperLite
  describe "transmatic" testTransmatic

testAdmins :: Spec
testAdmins = do
  it "parses ObserveAccounts" $
     readHopper "ObserveAccounts"
             `shouldBe` Right ObserveAccounts
  it "parses ObserveAccount" $
     readHopper "ObserveAccount foo"
             `shouldBe` Right (ObserveAccount "foo")
  it "parses ObserveAccounts" $
     readHopper "AdjustAccount foo 1%1"
             `shouldBe` Right (AdjustAccount "foo" (1%1))
  it "parses CreateAccount" $
     readHopper "CreateAccount foo"
             `shouldBe` Right (CreateAccount "foo")

testHopperLite :: Spec
testHopperLite = do
  it "does the thing" $ readHopper "transfer(foo->bar,101%100)" `shouldBe`
         Right (Program T.empty (Let "t" (PrimApp "transfer"
                                   [ELit (LText "foo"),ELit (LText "bar"),
                                    ELit (LRational (101 % 100)),ELit (LText "cryptSig")])
                                 (V "t")))


testTransmatic :: Spec
testTransmatic = do
  it "does the stuff" $ readHopper "(#transfer \"foo\" \"bar\" (% 110 100) \"baz\")" `shouldBe`
     Right (Program  T.empty (PrimApp "transfer"
                              [ELit (LText "foo"),
                               ELit (LText "bar"),
                               ELit (LRational (11 % 10)),
                               ELit (LText "baz")]))
