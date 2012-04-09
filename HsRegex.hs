module HsRegex (subRange, 
                dot, 
                endl, 
                stl, 
                spc, 
                notSpc,
                wc, 
                notWc, 
                digit, 
                notDigit, 
                alnum, 
                ch, 
                plus, 
                star, 
                pipe, 
                range, 
                notRange, 
                qMark, 
                wb,
                reGroup,
                var,
                mN, 
                mLN, 
                mN1N2,
                combine, 
                (=~), 
                replRe) where

import Data.Char
import Data.List
import Data.String.Utils

-- for 3 element tuples 
fst' (x, _, _) = x
snd' (_, y, _) = y
thrd' (_, _, z) = z

-- for extracting substrings
subRange :: (Int, Int) -> [a] -> [a]
subRange (x, y) = take (y - x) . drop x

-- for extracting strings from index tuple
extractMatches :: String -> [(Int, Int)] -> [String]
extractMatches str = map (flip subRange str)



-- for notational convienience
type Regex = [String] -> String -> Int -> (Bool, Int, [String])


greedyMatch :: Regex -> [String] -> String -> Int -> (Int, Int)
greedyMatch m  groups ss i = match ss i m 0 groups
  where match :: String -> Int -> Regex -> Int -> [String] -> (Int, Int)
        match str indx isMatch nrMatch gs =
          if indx < length str && fst' (isMatch gs str indx ) then
            match str (indx + 1) isMatch (nrMatch + 1) gs
          else (nrMatch, indx)

matchN :: Regex -> Int -> [String]-> String -> Int -> (Bool, Int)
matchN m n groups ss i = match ss i m n 0 groups
  where match :: String -> Int -> Regex -> Int -> Int -> [String] -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes gs =
          if nrTimes < nrMatch && indx < length str && fst' (isMatch gs str indx)
          then match str (indx + 1) isMatch nrMatch (nrTimes +1) gs
          else (nrTimes == nrMatch, indx)

matchAtLeastN :: Regex -> Int -> [String] -> String -> Int -> (Bool, Int)
matchAtLeastN m n groups ss i = match ss i m n 0 groups
  where match :: String -> Int -> Regex -> Int -> Int -> [String] -> (Bool, Int)
        match str indx isMatch nrMatch nrTimes gs =
          if indx < length str && fst' (isMatch gs str indx) then
            match str (indx + 1) isMatch nrMatch (nrTimes +1) gs
          else (nrTimes >= nrMatch, indx)

matchBetweenN1N2 :: Regex -> Int -> Int -> [String] -> String -> Int -> (Bool, Int)
matchBetweenN1N2 m n1 n2 groups ss i = match ss i m n1 n2 0 groups
  where match str indx isMatch min max counter gs =
          if counter < max && indx < length str && fst' (isMatch gs str indx)
          then match str (indx + 1) isMatch min max (counter + 1) gs
          else (counter >= min && counter <= max, indx)

-- .
dot :: Regex
dot gs ss i = ('\n' /= ss !! i, i + 1, gs)

-- $
endl :: Regex
endl gs ss i = ((i + 1) == length ss || ss !! i == '\n', i + 1, gs)

-- ^
stl :: Regex
stl gs ss i = if i == 0 then (True, i, gs)
             else (ss !! (i - 1) == '\n', i + 1, gs)

-- \s
spc :: Regex
spc gs ss i = (isSpace (ss !! i), i + 1, gs)

-- \S
notSpc :: Regex
notSpc gs ss i = let m = spc gs ss i in (not $ fst' $ m, snd' m, gs)

-- \w
wc :: Regex
wc gs ss i = (isLetter (ss !! i), i + 1, gs)

-- \W
notWc :: Regex
notWc gs ss i = let m = wc gs ss i in (not $ fst' m, snd' m, gs)

-- \d
digit :: Regex
digit gs ss i = (ss !! i `elem` ['0' .. '9'], i + 1, gs)

-- \D
notDigit :: Regex
notDigit gs ss i = let m = digit gs ss i in (not $ fst' m, snd' m, gs)

-- \w
alnum :: Regex
alnum gs ss i = (isAlphaNum (ss !! i), i + 1, gs)

-- [a-Z-0-9]
ch :: Char -> Regex
ch ch gs ss i = (ch == ss !! i, i + 1, gs)

-- +
plus  :: Regex -> Regex
plus m gs ss i = let (nrMatch, indx) = greedyMatch m gs ss i
              in (nrMatch > 0, indx, gs)

-- *
star :: Regex -> Regex
star m gs ss i = let (nrMatch, indx) = greedyMatch m gs ss i
                 in (nrMatch >= 0, indx, gs)

-- |
pipe :: Regex -> Regex -> Regex
pipe m1 m2 gs ss i = (fst' (m1 gs ss i) || fst' (m2 gs ss i), i + 1, gs)

-- []
range :: String -> Regex
range cs gs ss i = (any (fst' . isMatch ss i gs) cs, i + 1, gs)
  where isMatch ss i groups c  = ch c groups ss i 

-- [^]
notRange :: String -> Regex
notRange cs gs ss i = let (bool, indx, groups) = range cs gs ss i
                      in (not bool, indx, groups)

-- ?
qMark :: Regex -> Regex
qMark m gs ss i = case m gs ss i of
  (True, indx, groups) -> (True, indx, groups)
  (False, _, groups) -> (True, i, groups)

-- \b
wb :: Regex
wb gs ss i = f gs ss i where
  f :: Regex
  f groups str indx
    | indx >= length str - 1 = (False, indx, groups)
    | isSpace (str !! indx) && isAlphaNum (str !! (indx + 1 )) =
      (True, indx, groups)
    | otherwise = f gs str (indx + 1)


reGroup :: (String -> Int -> (Bool, Int, [String])) -> Regex
reGroup re gs ss i = let (bool, indx, group) = re ss i
                     in if bool then
                          (True, indx, group ++ [subRange (i, indx) ss])
                        else (False, i, gs)

var :: Int -> Regex
var varN gs ss i = let str = if not $ null gs then gs !! (varN - 1) else []
                       strEndIndex = i + length str in
                   if not $ null str && str == subRange (i, strEndIndex) ss 
                   then (True, strEndIndex, gs)
                   else (False, i, gs)

-- {n}
mN :: Regex -> Int -> [String] -> String -> Int -> (Bool, Int, [String])
mN m n gs ss i = let (bool, pos) = matchN m n gs ss i in (bool, pos, gs)

-- {,n}
mLN :: Regex -> Int -> [String] -> String -> Int -> (Bool, Int, [String])
mLN m n gs ss i = let (bool, pos) = matchAtLeastN m n gs ss i in (bool, pos, gs)

-- {n, n}
mN1N2 :: Regex -> (Int, Int) -> [String] -> String -> Int -> (Bool,Int,[String])
mN1N2 m (min, max) gs ss i = let (bool, pos) = matchBetweenN1N2 m min max gs ss i 
                             in (bool, pos, gs)
     
-- chain functions together and providing the end index
-- of the previous as the start of the current.
combine :: [Regex] -> String -> Int -> (Bool, Int, [String])
combine funs = acc funs [] []
  where acc :: [Regex] -> [Bool] -> [String] -> String -> Int -> (Bool, Int, [String])
        acc [] rs gs ss i = (and rs, i, gs)
        acc (f:fs) rs gs ss i = let (bool, indx, groups) = f gs ss i in
          if indx < length ss then
            acc fs (bool:rs) groups ss indx
          else (False, indx, groups)

replaceRegex :: (String -> Int -> (Bool, Int, [String])) -> String -> String -> String
replaceRegex re ss subStr = foldr (flip replace subStr) ss (ss =~ re)

matchRegex :: (String -> Int -> (Bool, Int, [String])) -> String -> 
              [(Int, Int)] -> Int -> [(Int, Int)]
matchRegex re ss ms i = if i < length ss then
                          let (bool, indx, _) = re ss i in
                          if bool then
                            matchRegex re ss (ms ++ [(i, indx)]) (i + 1)
                          else matchRegex re ss ms (i + 1)
                        else ms

-- apply the regex on tails str and record the matched ranges
(=~) :: String ->  (String -> Int -> (Bool, Int, [String])) -> [String]
(=~) str regex =  extractMatches str $ matchRegex regex (str ++ ['\n']) [] 0

-- replace the matches found with a regex with the suplied string
replRe :: (String -> Int -> (Bool, Int , [String])) -> String -> String 
          -> String
replRe = replaceRegex
