module Main
  ( main
  ) where

import           Data.Either
import           NLP.Morphology.PT.Verb
import           Text.Read                   (readEither)
import           Test.Hspec

main :: IO ()
main = do
  -- Debugging examples (kept commented)
  -- putStrLn $ fromRight "" $ fmap (txt . morphs) $ mkVS4 "falar" IFUT P3
  -- putStrLn $ fromRight "" $ fmap (txt . deep) $ mkVS4 "falar" IFUT P3
  -- putStrLn $ fromRight "" $ fmap (txt . shallow) $ mkVS4 "falar" IFUT P3
  -- putStrLn $ fromRight "" $ fmap (txt . deep) $ mkVS4 "falar" COND P5
  -- putStrLn $ fromRight "" $ fmap (txt . shallow) $ mkVS4 "falar" COND P5
  -- putStrLn $ fromRight "" $ fmap orth $ mkVS4 "falar" IFUT P3
  hspec $ do
    describe "mkCitation Tests" $ do
      it "Valid citation form ending with 'R'"
        $ mkCitation "falar" `shouldBe` Right "FALAR"
      it "Invalid citation form without 'R'"
        $ mkCitation "fala" `shouldSatisfy` isLeft
      it "Invalid citation form with fewer than 2 characters"
        $ mkCitation "f" `shouldSatisfy` isLeft
    describe "mkRoot Tests" $ do
      it "Creates a valid root from a citation"
        $ mkRoot "falar" `shouldBe` Right (Root "FAL")
    describe "mkThematicVowel Tests" $ do
      it "Creates a thematic vowel for a valid citation"
        $ mkThematicVowel "falar" `shouldBe` Right A'
    describe "mkVS3 Tests" $ do
      it "Creates VS3 with INF" $ mkVS3 "falar" INF `shouldSatisfy` isRight
      it "Fails to create VS3 with invalid MoodTense"
        $ mkVS3 "falar" IPRS `shouldSatisfy` isLeft
    describe "mkVS4 Tests" $ do
      it "Creates VS4 with IPRS and P1"
        $ mkVS4 "falar" IPRS P1 `shouldSatisfy` isRight
      it "Fails to create VS4 with invalid MoodTense"
        $ mkVS4 "falar" INF P1 `shouldSatisfy` isLeft
    describe "mkVS5 Tests" $ do
      it "Creates VS5 with PPP, MSC, and SG"
        $ mkVS5 "falar" PPP MSC SG `shouldSatisfy` isRight
      it "Fails to create VS5 with invalid MoodTense"
        $ mkVS5 "falar" GER MSC SG `shouldSatisfy` isLeft
    describe "mkTense Tests" $ do
      it "Creates a tense table for IPRS" $ do
        let t1 = mkTense "falar" IPRS
        fmap tenseTable t1 `shouldSatisfy` isRight
        fmap tenseForms t1 `shouldSatisfy` isRight
  describe "mkParadigm Tests" $ do
    it "Creates a paradigm for a valid citation" $ do
      let p1 = mkParadigm "falar"
      fmap citation p1 `shouldSatisfy` isRight
      fmap tenseTables p1 `shouldSatisfy` isRight
    it "Fails to create a paradigm for an invalid citation" $ do
      let p2 = mkParadigm "f"
      fmap citation p2 `shouldSatisfy` isLeft
      fmap tenseTables p2 `shouldSatisfy` isLeft
  describe "Input Parsing Tests" $ do
    it "Invalid mood string returns Left" $
      (readEither "INVALID" :: Either String MoodTense) `shouldSatisfy` isLeft

    describe "deep and shallow Tests" $ do
      it "Returns deep form for IFUT P3" $ do
        let res = fromRight "" $ fmap (txt . deep) $ mkVS4 "falar" IFUT P3
        res `shouldBe` "FAL-A-R+H-A-∅-∅"
      it "Returns shallow form for IFUT P3" $ do
        let res = fromRight "" $ fmap (txt . shallow) $ mkVS4 "falar" IFUT P3
        res `shouldBe` "FAL-A-R+∅-A-∅-∅"
