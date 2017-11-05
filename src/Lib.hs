module Lib
    ( dayOneA
    , dayOneB
    , dayTwoA
    , dayTwoB
    , dayThreeA
    , dayThreeB
    ) where

import           Data.List       (delete, elemIndex)
import           Data.List.Split (splitOn)
import qualified Data.Set        as Set


-- Inputs

readInput :: String -> IO String
readInput day =
  readFile $ "/Users/kieran/dev/haskell/aoc15/input/day" ++ day


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
