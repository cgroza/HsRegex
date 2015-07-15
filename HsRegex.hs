module HsRegex (subRange,
                char,
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
                RegexS)
       where

import Data.Char
import Data.List
import Control.Monad.State as C
import Control.Monad.Writer
import Control.Monad.Loops
import Data.String.Utils

-- for extracting substrings
subRange :: (Int, Int) -> [a] -> [a]
subRange (x, y) = take (y - x) . drop x

-- for extracting strings from index tuple
extractMatches :: String -> [(Int, Int)] -> [String]
extractMatches str = map (flip subRange str)

data RegexS = RegexS {groups :: [String], text :: String, position :: Int}

addGroup :: String -> State RegexS ()
addGroup g = do
  (RegexS gs t p) <- get
  put $ RegexS (gs ++ [g]) t p

getGroup :: Int -> State RegexS (Maybe String)
getGroup varN = do
  (RegexS gs _ _) <- get
  return $ if varN < length gs then Just $ gs !! (varN - 1) else Nothing

setPosition :: Int -> State RegexS ()
setPosition p'  = do
  (RegexS g t _) <- get
  put $ RegexS g t p'

advancePosition :: Int -> State RegexS ()
advancePosition i = getPosition >>= (setPosition . (i +))

currentChar :: RegexS -> Char
currentChar st = text st !! position st

getPosition :: State RegexS Int
getPosition = liftM position get

getTextLength :: State RegexS Int
getTextLength = liftM (length . text) get

match :: State RegexS Bool -> State RegexS Bool
match m = do
  oldPos <- getPosition
  matched <- m
  st <- get
  if position st < length (text st) && matched
    then return True
    else setPosition oldPos >> return False

-- match using regexp m as many times as possible
greedyMatch :: State RegexS Bool -> State RegexS Int
greedyMatch m  = liftM length $ whileM (match m) (return ())

-- match using regexp m N times
-- if N is not satisfied, fst is false
matchN :: State RegexS Bool -> Int -> State RegexS Bool
matchN m i =  andM $ replicate i (match m)

-- match using regexp m at least N times
-- if N is not satisfied, fst is false
matchAtLeastN :: State RegexS Bool -> Int -> State RegexS Bool
matchAtLeastN m i = liftM (i <=) (greedyMatch m)

-- match using regexp m between N1 and N2
-- if not between N1 N2, fst is false
matchBetweenN1N2 :: State RegexS Bool -> Int -> Int -> State RegexS Bool
matchBetweenN1N2 m n1 n2 = fmap (>= n1) $ liftM (length . takeWhile id) $ sequence
                           (replicate n2 (match m))

-- characters
char :: Char -> State RegexS Bool
char c = do
    st <- get
    advancePosition 1
    return $ c == text st !! position st

-- .
dot :: State RegexS Bool
dot = do
  st <- get
  advancePosition 1
  return $ '\n' /= text st !! position st

-- $
endl :: State RegexS Bool
endl = do
  st <- get
  advancePosition 1
  return $ (position st >= length  (text st)) || (text st !! position st == '\n')

-- ^
stl :: State RegexS Bool
stl = do
  st <- get
  if position st == 0 then return True
    else return $ text st !! (position st - 1) == '\n'

-- \s
spc :: State RegexS Bool
spc = do
  st <- get
  advancePosition 1
  return $ isSpace (text st !! position st)

-- \S
notSpc :: State RegexS Bool
notSpc = liftM not spc

-- \w
wc :: State RegexS Bool
wc = do
  st <- get
  advancePosition 1
  return $ isLetter (text st !! position st)

-- \W
notWc :: State RegexS Bool
notWc = liftM not wc

-- \d
digit :: State RegexS Bool
digit = do
  st <- get
  advancePosition 1
  return $ text st !! position st `elem` ['0' .. '9']

-- \D
notDigit :: State RegexS Bool
notDigit = liftM not digit

-- \w
alnum :: State RegexS Bool
alnum = do
  st <- get
  advancePosition 1
  return $ isAlphaNum (text st !! position st)

-- +
plus  :: State RegexS Bool -> State RegexS Bool
plus m = liftM (> 0) (greedyMatch m)
-- *
star :: State RegexS Bool -> State RegexS Bool
star m = greedyMatch m >> return True

-- |
pipe :: State RegexS Bool -> State RegexS Bool -> State RegexS Bool
pipe m1 m2 = liftM2 (||) m1 m2

-- []
range :: String -> State RegexS Bool
range cs = do
  st <- get
  advancePosition 1
  return $ text st !! position st `elem` cs

-- [^]
notRange :: String -> State RegexS Bool
notRange cs = liftM not (range cs)

-- ?
qMark :: State RegexS Bool -> State RegexS Bool
qMark m = do
  oldPos <- getPosition
  mR <- m
  if not mR then setPosition oldPos >> return True
    else return True

-- \b
wb :: State RegexS Bool
wb = plus spc >> alnum

-- (regexp)
reGroup :: [State RegexS Bool] -> State RegexS Bool
reGroup rs = do initialPos <- getPosition
                matched <- combine rs
                finalPos <- getPosition
                st <- get
                addGroup $ subRange (initialPos, finalPos) (text st)
                return matched
-- $n
var :: Int -> State RegexS Bool
var varN = do
  pos <- getPosition
  st <- get
  (Just str) <- getGroup varN
  let strEndIndex = pos + length str
  if not $ null str && str == subRange (pos, strEndIndex) (text st)
    then setPosition strEndIndex >> return True
    else return False

-- {n}
mN :: State RegexS Bool -> Int -> State RegexS Bool
mN = matchN

-- {,n}
mLN :: State RegexS Bool -> Int -> State RegexS Bool
mLN = matchAtLeastN

-- {n, n}
mN1N2 :: State RegexS Bool -> (Int, Int) -> State RegexS Bool
mN1N2 m (min, max) = matchBetweenN1N2 m min max

-- chain functions together and providing the end index
-- of the previous as the start of the next.
combine :: [State RegexS Bool] -> State RegexS Bool
combine regex = liftM and $ mapM acc regex
  where acc :: State RegexS Bool -> State RegexS Bool
        acc m = do
          len <- getTextLength
          pos <- getPosition
          if len <= pos then return False
            else m

-- replace all matches of (combine rs) with subStr
replaceRegex :: [State RegexS Bool] -> String -> String -> String
replaceRegex  rs ss subStr = foldr (flip replace subStr) ss (ss =~ rs)

-- return all matches of (combine rs)

matchRegex ::  [State RegexS Bool] -> String -> [(Int, Int)]
matchRegex rs ss = C.join $ map snd $ map runWriter $ map (subMatch ss (combine rs)) [0 .. length ss]
subMatch :: String -> State RegexS Bool -> Int -> Writer [(Int, Int)] ()
subMatch ss re i =
        let (matched, st) = runState re (RegexS [] ss i)
            pos = position st in
        if (pos < length (text st)) && matched then tell [(i, pos)]
        else return ()

-- -- apply the regex on tails str and return bigest match
(=~) :: String ->  [State RegexS Bool] -> [String]
(=~) str rs = extractMatches str $ matchRegex rs (str ++ ['\n'])

