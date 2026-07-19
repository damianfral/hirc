{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IRC.ProtocolSpec (spec) where

import IRC.Protocol
import Relude
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "encodeMessage/decodeMessage roundtrip" $ do
    it "roundtrips all generated messages" $ do
      forAllValid $ \msg -> do
        decodeMessage (encodeMessage msg) `shouldBe` Just msg

  describe "decodePrefix" $ do
    it "parses nick!user@host" $ do
      let expected =
            PrefixUser
              $ User (Nickname "nick") (Just (Username "user")) (Just "host")
      decodePrefix "nick!user@host" `shouldBe` Just expected

    it "parses nick!user (no host)" $ do
      let expected =
            PrefixUser $ User (Nickname "nick") (Just (Username "user")) Nothing
      decodePrefix "nick!user" `shouldBe` Just expected

    it "parses nick@host (no user)" $ do
      let expected = PrefixUser $ User (Nickname "nick") Nothing (Just "host")
      decodePrefix "nick@host" `shouldBe` Just expected

    it "parses a plain server name" $ do
      let expected = PrefixServer $ Server "server.name"
      decodePrefix "server.name" `shouldBe` Just expected

    it "parses empty string as a server with empty name" $ do
      let expected = PrefixServer (Server "")
      decodePrefix "" `shouldBe` Just expected

  describe "encodePrefix/decodePrefix roundtrip" $ do
    it "roundtrips all unambiguous prefixes" $ do
      forAllValid $ \p -> decodePrefix (encodePrefix p) `shouldBe` Just p

  describe "parseParams" $ do
    it "parses empty string produces empty params" $ do
      parseParams "" `shouldBe` Params []
    it "parses single param" $ do
      parseParams "hello" `shouldBe` Params ["hello"]
    it "parses multiple middle params" $ do
      parseParams "a b c" `shouldBe` Params ["a", "b", "c"]
    it "parses trailing param" $ do
      parseParams ":hello world" `shouldBe` Params ["hello world"]
    it "parses middle and trailing params" $ do
      parseParams "a :hi world" `shouldBe` Params ["a", "hi world"]
    it "parses empty trailing param" $ do
      parseParams ":" `shouldBe` Params [""]
    it "parses middle and empty trailing" $ do
      parseParams "a :" `shouldBe` Params ["a", ""]
