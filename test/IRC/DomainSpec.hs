{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.DomainSpec (spec) where

import IRC.Domain
import IRC.Protocol
import Relude
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = describe "targetToParams/parseTarget roundtrip" $ do
  it "roundtrips all generated targets" $ do
    forAllValid $ \t -> case targetToParams t of
      Params (p : _) -> parseTarget p `shouldBe` t
      _ -> expectationFailure "targetToParams returned empty Params"
