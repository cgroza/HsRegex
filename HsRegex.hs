module Main where
import Data.Char
import Data.List

-- All symbols are the perl equivalents with a dot appended to the function name with the exception of:
-- . =  ...
-- sc = \s
-- @#. = alphanumerical
-- @. = normal char
-- % = []
-- ^% = [^]

type Regex = String -> Int -> (Bool, Int)

subRange :: (Int, Int) -> [a] -> [a]
subRange (x, y) = take (y - x) . drop x


greedyMatch :: Regex -> String -> Int -> (Int, Int)
greedyMatch m ss i = match ss i m 0
  where match :: String -> Int -> Regex -> Int -> (Int, Int)
        match str indx isMatch nrMatch = 
          if indx < length str && fst (isMatch str indx) then
            match str (indx + 1) isMatch (nrMatch + 1)
          else (nrMatch, indx)

matchN :: Regex -> Int -> Regex
matchN m n ss i = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes = 
          if nrTimes < nrMatch && indx < length str && fst (isMatch str indx)
          then match str (indx + 1) isMatch nrMatch (nrTimes +1)                                          
          else (nrTimes == nrMatch, indx)

matchAtLeastN :: Regex -> Int -> Regex
matchAtLeastN m n ss i = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes =
          if indx < length str && fst (isMatch str indx) then
            match str (indx + 1) isMatch nrMatch (nrTimes +1)
          else (nrTimes >= nrMatch, indx)

matchBetweenN1N2 :: Regex -> Int -> Int -> Regex
matchBetweenN1N2 m n1 n2 ss i = match ss i m n1 n2 0
  where match str indx isMatch min max counter = 
          if counter < max && indx < length str && fst (isMatch str indx) 
          then match str (indx + 1) isMatch min max (counter + 1) 
          else (counter >= min && counter <= max, indx)


-- regular perl meanings
(...) :: Regex
(...) ss i = ('\n' /= ss !! i, i + 1)

($.) :: Regex
($.) ss i = ((i + 1) == length ss || ss !! i == '\n', i + 1)

(^.) :: Regex
(^.) ss i = if i == 0 then (True, i) 
             else (ss !! (i - 1) == '\n', i + 1)

sc :: Regex
sc ss i = (isSpace (ss !! i), i + 1)

notSc :: Regex
notSc ss i = let m = sc ss i in (not $ fst $ m, snd m)

wc :: Regex
wc ss i = (isLetter (ss !! i), i + 1)

notWc :: Regex
notWc ss i = let m = wc ss i in (not $ fst $ m, snd m)

(#.) :: Regex
(#.) ss i = (ss !! i `elem` ['0' .. '9'], i + 1)

(^#.) :: Regex
(^#.) ss i = let m = (#.) ss i in (not $ fst $ m, snd m)

(@#.) :: Regex
(@#.) ss i = (isAlphaNum (ss !! i), i + 1)

(@.) :: Char -> Regex
(@.) ch ss i = (ch == ss !! i, i + 1)

(+.)  :: Regex -> Regex
(+.) m ss i = let (nrMatch, indx) = greedyMatch m ss i 
              in (nrMatch > 0, indx)

(*.) :: Regex -> Regex
(*.) m ss i = let (nrMatch, indx) = greedyMatch m ss i 
                 in (nrMatch >= 0, indx)

(|.) :: Regex -> Regex -> Regex
(|.) m1 m2 ss i = (fst (m1 ss i) || fst (m2 ss i), i + 1)

(%.) :: String -> Regex
(%.) cs ss i = (any (fst . isMatch ss i) cs, i + 1)
  where isMatch ss i ch = (@.) ch ss i

(^%.) :: String -> Regex
(^%.) cs ss i = let (bool, indx) = (%.) cs ss i 
                   in (not bool, indx)

(?.) :: Regex -> Regex
(?.) m ss i = case m ss i of 
  (True, indx) -> (True, indx)
  (False, _) -> (True, i)

b :: String -> Int -> (Bool, Int)
b ss i = f ss i where
  f :: String -> Int -> (Bool, Int)
  f str indx 
    | indx >= length str - 1 = (False, indx)
    | isSpace (str !! indx) && isAlphaNum (str !! (indx + 1 )) = (True, indx)
    | otherwise = f str (indx + 1)

(%#.) :: Regex -> Int -> Regex
(%#.) = matchAtLeastN

(#%.) :: Regex -> Int -> Regex
(#%.) = matchAtLeastN

(#%#.) :: Regex -> (Int, Int) -> Regex
(#%#.) m (min, max) = matchBetweenN1N2 m min max
     
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

matchRegex :: Regex -> String -> [(Int, Int)] -> Int -> [(Int, Int)]
matchRegex re ss ms i = if i < length ss then 
                     let (bool, indx) = re ss i in
                     if bool then
                       matchRegex re ss (ms ++ [(i, indx)]) (i + 1)
                     else matchRegex re ss ms (i + 1)
                   else ms

-- apply the regex on tails str and record the matched ranges
(=~) :: String ->  Regex -> [String]
(=~) str regex = map  (flip subRange str) $ matchRegex regex (str ++ ['\n']) [] 0




main :: IO ()
main = return ()