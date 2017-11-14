{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( dayOneA
    , dayOneB
    , dayTwoA
    , dayTwoB
    , dayThreeA
    , dayThreeB
    , dayFourAB
    , dayFiveAB
    ) where

import           Crypto.Hash.MD5
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           Data.List              (delete, elemIndex, group, isInfixOf,
                                         sort, elem)
import           Data.List.Split        (splitOn)
import qualified Data.Set               as Set



-- Inputs

readInput :: String -> IO String
readInput day =
  readFile $ "/Users/kieran/dev/aoc15/input/day" ++ day


-- Results

printResult :: Show a => String -> a -> IO ()
printResult day result =
  print $ "Got answer: " ++ show result ++ " for day " ++ day


-- Day One

dayOneA :: IO ()
dayOneA = do
  input <- readInput "One"
  let answer = foldl parseParen 0 input
  printResult "One (A)" answer
  return ()


dayOneB :: IO ()
dayOneB = do
  input <- readInput "One"
  let answer = elemIndex (-1) $ scanl parseParen 0 input
  case answer of
    Just index -> printResult "One (B)" index
    Nothing    -> print "Failed to find index 0"
  return ()


parseParen :: Integer -> Char -> Integer
parseParen n '(' = n + 1
parseParen n ')' = n - 1
parseParen _ _   = undefined



-- Day Two

-- Surface area: 2*l*w + 2*w*h + 2*h*l
-- 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 + 6 (shortest side) = 58
-- 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 + 1 (shortest side) = 43

-- 2x3x4 requires 2+2+3+3 = 10 + 2*3*4 = 24 = 34
-- 1x1x10 requires 1+1+1+1 = 4 + 1*1*10 = 10 = 14

data Present = Present Int Int Int


dayTwoA :: IO ()
dayTwoA = do
  input <- readInput "Two"
  let answer = (calculateRequiredPaper . parsePresents) input
  printResult "Two (A)" answer
  return ()


calculateRequiredPaper :: [Present] -> Int
calculateRequiredPaper =
  foldr (\p a -> a + calculateArea p) 0


calculateArea :: Present -> Int
calculateArea (Present w h l) =
  shortestSide + sum (map (*2) sides)
  where
    sides = [l*w, w*h, h*l]
    shortestSide = minimum sides


dayTwoB :: IO ()
dayTwoB = do
  input <- readInput "Two"
  let answer = (calculateRequiredRibbon . parsePresents) input
  printResult "Two (B)" answer
  return ()


calculateRequiredRibbon :: [Present] -> Int
calculateRequiredRibbon =
  foldr (\p a -> a + calculateRibbon p) 0


calculateRibbon :: Present -> Int
calculateRibbon (Present w h l) =
  sideRibbon + bowRibbon
  where
    sides = [w, h, l]
    shortestSides = delete (maximum sides) sides
    sideRibbon = sum (map sum [shortestSides, shortestSides])
    bowRibbon = w * h * l


parsePresents :: String -> [Present]
parsePresents input =
  map parsePresent $ lines input


parsePresent :: String -> Present
parsePresent line =
  case splitOn "x" line of
    [w, h, l] ->
      Present (read w) (read h) (read l)
    _ ->
      undefined


-- Day Three

dayThreeA :: IO ()
dayThreeA = do
  input <- readInput "Three"
  printResult "Three (A)" $ Set.size $ uniqLocations input
  return ()


uniqLocations :: String -> Set.Set (Int, Int)
uniqLocations input =
  Set.fromList $ scanr deliver (0,0) input


deliver ::  Char -> (Int, Int) ->(Int, Int)
deliver '>' pos = (fst pos + 1, snd pos)
deliver '<' pos = (fst pos - 1, snd pos)
deliver 'v' pos = (fst pos, snd pos - 1)
deliver '^' pos = (fst pos, snd pos + 1)
deliver _ _     = undefined


splitInstructions :: String -> (String, String) -> (String, String)
splitInstructions (x:y:xs) (a, b) = splitInstructions xs (x : a, y : b)
splitInstructions [x]      (a, b) = (x : a, b)
splitInstructions []       (a, b) = (a, b)


dayThreeB :: IO ()
dayThreeB = do
  input <- readInput "Three"
  let (santa, robo) = splitInstructions input ("","")
      locations = Set.union (uniqLocations santa) (uniqLocations robo)
  printResult "Three (B)" $ Set.size locations
  return ()


-- Day Four

dayFourAB :: IO ()
dayFourAB = do
  input <- readInput "Four"
  answerA <- lowestNumberToProduce "00000" 0 input
  answerB <- lowestNumberToProduce "000000" 0 input
  printResult "Four (A)" answerA
  printResult "Four (B)" answerB
  return ()


lowestNumberToProduce :: BS.ByteString -> Int -> String -> IO Int
lowestNumberToProduce prefix num key =
  if prefix `BS.isPrefixOf` performHash key num then
    return num
  else
    lowestNumberToProduce prefix (num + 1) key


performHash :: String -> Int -> BS.ByteString
performHash key number =
  Base16.encode $ Crypto.Hash.MD5.hash $ Char8.pack (key ++ show number)


-- Day Five

dayFiveAB :: IO ()
dayFiveAB = do
  input <- readInput "Five"
  printResult "Five (A)" $ numberOfNiceStringsA input
  printResult "Five (B)" $ numberOfNiceStringsB input
  return ()


numberOfNiceStringsA :: String -> Int
numberOfNiceStringsA str =
  length (filter isNiceA $ lines str)


isNiceA :: String -> Bool
isNiceA str =
  containsThreeVowels str && containsAPair str && excludesSpecialPairs str


containsThreeVowels :: String -> Bool
containsThreeVowels str =
  (length $ filter (\c -> c `elem` ("aeiou" :: [Char])) str) >= 3


containsAPair :: String -> Bool
containsAPair str =
  any (\pair -> pair `isInfixOf` str) pairs
  where
    pairs = group $ sort (['a'..'z'] ++ ['a'..'z'])


excludesSpecialPairs :: String -> Bool
excludesSpecialPairs str =
  not $ any (\pair -> pair `isInfixOf` str) specialPairs
  where
    specialPairs = ["ab", "cd", "pq", "xy"]


numberOfNiceStringsB :: String -> Int
numberOfNiceStringsB str =
  length (filter isNiceB $ lines str)


isNiceB :: String -> Bool
isNiceB str =
  containsRepeatedNonOverlappingPair str && containsRepeatedLetterStraddlingAnother str


data Pair =
  Pair { first :: Char
       , second :: Char
       , tailIndex :: Int
       } deriving Show

instance Eq Pair where
  (Pair a1 a2 i) == (Pair b1 b2 j) = a1 == b1 && a2 == b2 && (i - j >= 2 || j - i >= 2)


containsRepeatedNonOverlappingPair :: String -> Bool
containsRepeatedNonOverlappingPair str =
  comparePairs $ constructPairs str


constructPairs :: [Char] -> [Pair]
constructPairs (x:y:xs) = Pair x y (length xs) : constructPairs (y : xs)
constructPairs [_] = []
constructPairs [] = []


comparePairs :: [Pair] -> Bool
comparePairs (x:xs) = any (\p -> p == x) xs || comparePairs xs
comparePairs _ = False


containsRepeatedLetterStraddlingAnother :: String -> Bool
containsRepeatedLetterStraddlingAnother (x:y:z:xs) = x == z || containsRepeatedLetterStraddlingAnother (y : z : xs)
containsRepeatedLetterStraddlingAnother _ = False


