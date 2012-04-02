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


greedyMatch ss i m = match ss i m 0
  where match :: String -> Int -> Regex -> Int -> (Int, Int)
        match str indx isMatch nrMatch = 
          if indx < length str && fst (isMatch str indx) then
            match str (indx + 1) isMatch (nrMatch + 1)
          else (nrMatch, indx)

matchN ss i m n = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes = 
          if nrTimes < nrMatch && indx < length str && fst (isMatch str indx)
          then match str (indx + 1) isMatch nrMatch (nrTimes +1)                                          
          else (nrTimes == nrMatch, indx)

matchAtLeastN ss i m n = match ss i m n 0
  where match :: String -> Int -> Regex -> Int -> Int -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes =
          if indx < length str && fst (isMatch str indx) then
            match str (indx + 1) isMatch nrMatch (nrTimes +1)
          else (nrTimes >= nrMatch, indx)

matchBetweenN1N2 ss i m n1 n2 = match ss i m n1 n2 0
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
notSc ss i = not $ sc ss i

wc :: Regex
wc ss i = (isLetter (ss !! i), i + 1)

notWc :: Regex
notWc ss i = not $ wc ss i

(#.) :: Regex
(#.) ss i = show ss !! i `elem` [0 .. 9]

(^#.) :: Regex
(^#.) ss i = not $ (#.) ss i

(@#.) :: Regex
(@#.) ss i = (isAlphaNum (ss !! i), i + 1)

(@.) :: Char -> Regex
(@.) ch ss i = (ch == ss !! i, i + 1)

(+.)  :: Regex -> Regex
(+.) m ss i = let (nrMatch, indx) = greedyMatch ss i m 
              in (nrMatch > 0, indx)

(*.) :: Regex -> Regex
(*.) m ss i = let (nrMatch, indx) = greedyMatch ss i m 
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
(=~) :: Regex -> String -> [(Int, Int)]
(=~) regex str = match regex (str ++ ['\n']) [] 0
  where match :: Regex -> String -> [(Int, Int)] -> Int -> [(Int, Int)]
        match re ss ms i = if i < length ss then 
                             let (bool, indx) = re ss i in
                             if bool then
                               match re ss (ms ++ [(i, indx)]) (i + 1)
                             else match re ss ms (i + 1)
                           else ms



main :: IO ()
main = return ()