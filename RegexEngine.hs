
module Main where
import Data.Char
import Data.List

type Regex = String -> Int -> (Bool, Int)

greedyMatch ss i m = match ss i m 0
  where match :: String -> Int -> Regex -> Int -> (Int, Int)
        match str indx isMatch nrMatch = 
          if indx < length str && (fst $ isMatch str indx) then
            match str (indx + 1) isMatch (nrMatch + 1)
          else (nrMatch, indx)

matchN ss i m n = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes = 
          if nrTimes < nrMatch && indx < length str && (fst $ isMatch str indx)
          then match str (indx + 1) isMatch nrMatch (nrTimes +1)                                          
          else (nrTimes == nrMatch, indx)

matchAtLeastN ss i m n = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes =
          if indx < length str && (fst $ isMatch str indx) then
            match str (indx + 1) isMatch nrMatch (nrTimes +1)
          else (nrTimes >= nrMatch, indx)

matchBetweenN1N2 ss i m n1 n2 = match ss i m n1 n2 0
  where match str indx isMatch min max counter = 
          if counter < max && indx < length str && (fst $ isMatch str indx) 
          then match str (indx + 1) isMatch min max (counter + 1) 
          else (counter >= min && counter <= max, indx)


-- regular perl meanings
dot :: Regex
dot ss i = ('\n' /= ss !! i, i + 1)

dollar :: Regex
dollar ss i = ((i + 1) == length ss || ss !! i == '\n', i + 1)

power :: Regex
power ss i = if i == 0 then (True, i) 
             else (ss !! (i - 1) == '\n', i + 1)

isWhitespace :: Regex
isWhitespace  ss i = (isSpace (ss !! i), i + 1)

isWordChar :: Regex
isWordChar ss i = (isLetter (ss !! i), i + 1)

isAlNum :: Regex
isAlNum ss i = (isAlphaNum (ss !! i), i + 1)

character :: Char -> Regex
character ch ss i = (ch == ss !! i, i + 1)

plus  :: Regex -> Regex
plus m ss i = let (nrMatch, indx) = greedyMatch ss i m 
              in (nrMatch > 0, indx)

asterix :: Regex -> Regex
asterix m ss i = let (nrMatch, indx) = greedyMatch ss i m 
                 in (nrMatch >= 0, indx)

pipe :: Regex -> Regex -> Regex
pipe m1 m2 ss i = ((fst $ m1 ss i) || (fst $ m2 ss i), i + 1)

range :: String -> Regex
range cs ss i = (any (fst . isMatch ss i) cs, i + 1)
  where isMatch ss i ch = character ch ss i

notRange :: String -> Regex
notRange cs ss i = let (bool, indx) = range cs ss i 
                   in (not bool, indx)

questionMark :: Regex -> Regex
questionMark m ss i = case m ss i of 
  (True, indx) -> (True, indx)
  (False, _) -> (True, i)

-- chain functions together and providing the end index 
-- of the previous as the start of the current.
combine :: [Regex] -> Regex
combine funs = acc funs []
  where acc :: [Regex] -> [Bool] -> String -> Int -> (Bool, Int)
        acc [] rs ss i = (and rs, i)
        acc (f:fs) rs ss i = let (bool, indx) = f ss i in
          if indx < length ss then
            acc fs (bool:rs) ss indx
          else (False, indx)

-- apply the regex on tails str and record the matched ranges
matchRegex :: Regex -> String -> [(Int, Int)]
matchRegex regex str = match regex (str ++ ['\n']) [] 0
  where match :: Regex -> String -> [(Int, Int)] -> Int -> [(Int, Int)]
        match re ss ms i = if i < length ss then 
                             let (bool, indx) = re ss i in
                             if bool then
                               match re ss (ms ++ [(i, indx)]) (i + 1)
                             else match re ss ms (i + 1)
                           else ms

makeRegex :: String -> Regex
makeRegex regexStr = undefined

main :: IO ()
main = return ()