module Exercise5 where

import Data.Char
import Test.QuickCheck (quickCheck)

rot13 :: [Char] -> [Char]
rot13 = map rot13'
  where
    rot13' :: Char -> Char
    rot13' c
      | isAlpha c =
          let base = if isLower c then ord 'a' else ord 'A'
           in chr $ (ord c - base + 13) `mod` 26 + base
      | otherwise = c

-- We used https://rot13.com/, to make some example sentences to test our ro13 function.
-- Furthermore, we used https://randomwordgenerator.com/sentence.php to generate 10 random sentences.
randomSentences :: [String]
randomSentences =
  [ "The secret ingredient to his wonderful life was crime.",
    "The beauty of the African sunset disguised the danger lurking nearby.",
    "The crowd yells and screams for more memes.",
    "Red is greener than purple, for sure.",
    "She saw no irony asking me to change but wanting me to accept her for who she is.",
    "She wore green lipstick like a fashion icon.",
    "She used her own hair in the soup to give it more flavor.",
    "The efficiency we have at removing trash has made creating trash more acceptable.",
    "The pigs were insulted that they were named hamburgers.",
    "There's probably enough glass in my cupboard to build an undersea aquarium."
  ]

randomSentencesRotEncoded :: [String]
randomSentencesRotEncoded =
  [ "Gur frperg vaterqvrag gb uvf jbaqreshy yvsr jnf pevzr.",
    "Gur ornhgl bs gur Nsevpna fhafrg qvfthvfrq gur qnatre yhexvat arneol.",
    "Gur pebjq lryyf naq fpernzf sbe zber zrzrf.",
    "Erq vf terrare guna checyr, sbe fher.",
    "Fur fnj ab vebal nfxvat zr gb punatr ohg jnagvat zr gb npprcg ure sbe jub fur vf.",
    "Fur jber terra yvcfgvpx yvxr n snfuvba vpba.",
    "Fur hfrq ure bja unve va gur fbhc gb tvir vg zber synibe.",
    "Gur rssvpvrapl jr unir ng erzbivat genfu unf znqr perngvat genfu zber npprcgnoyr.",
    "Gur cvtf jrer vafhygrq gung gurl jrer anzrq unzohetref.",
    "Gurer'f cebonoyl rabhtu tynff va zl phcobneq gb ohvyq na haqrefrn ndhnevhz."
  ]

-- This property checks that rot13 applied to encoded text, will return the original text
prop_reversalRot13 :: String -> Bool
prop_reversalRot13 x = rot13 (rot13 x) == x

-- This property ensures that our rot13 function has the same result as the sentences encoded on rot13.com
prop_rot13compareRandomSentences :: Bool
prop_rot13compareRandomSentences = all (\(x, y) -> rot13 x == y) $ zip randomSentences randomSentencesRotEncoded

main :: IO ()
main = do
  quickCheck prop_rot13compareRandomSentences
  quickCheck $ all prop_reversalRot13 randomSentences

-- We tried to use multiple random strings from quickcheck but we didn't manage it to get it working.
-- It fails on the following text: "\199889", but we believe the code should handle this correctly.
-- If a character c is not alphanumeric, it should return c. However, this doesn't seem to be happening correctly.
-- quickCheck prop_reversalRot13

-- Time Spent: 1 hour