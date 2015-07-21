module HsRegex (subRange, char, dot, endl, stl, spc, notSpc, wc, notWc, digit,
                notDigit, alnum, plus, star, pipe, range, notRange, qMark, wb,
                reGroup, var, mN, mLN, mN1N2, combine, (=~), RegexS)
       where

import           Control.Monad.Loops
import           Control.Monad.State  as C
import           Control.Monad.Writer
import           Data.Char
import           Data.String.Utils

data RegexS = RegexS {groups :: [String], text :: String, position :: Int}

-- for extracting substrings
subRange :: (Int, Int) -> [a] -> [a]
subRange (x, y) = take (y - x) . drop x

-- for extracting strings from index tuple
extractMatches :: String -> [(Int, Int)] -> [String]
extractMatches str = map (`subRange` str)

addGroup :: String -> State RegexS ()
addGroup g = do (RegexS gs t p) <- get
                put $ RegexS (gs ++ [g]) t p

getGroup :: Int -> State RegexS (Maybe String)
getGroup varN = do (RegexS gs _ _) <- get
                   return $ if varN < length gs then Just $ gs !! (varN - 1)
                            else Nothing

setPosition :: Int -> State RegexS ()
setPosition p'  = do (RegexS g t _) <- get
                     put $ RegexS g t p'

movePosition :: Int -> State RegexS ()
movePosition i = getPosition >>= (setPosition . (i +))

currentChar :: State RegexS Char
currentChar = liftM (\st -> text st !! position st) get

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
matchBetweenN1N2 m n1 n2 = liftM ((>= n1) . length . takeWhile id) $
                           replicateM n2 (match m)

-- characters
char :: Char -> State RegexS Bool
char c = liftM (== c) currentChar <* movePosition 1

-- .
dot :: State RegexS Bool
dot =  liftM ('\n' /=) currentChar <* movePosition 1

-- $
endl :: State RegexS Bool
endl =  orM [liftM2 (>=) getPosition getTextLength, liftM (== '\n') currentChar] <*
        movePosition 1

-- ^
stl :: State RegexS Bool
stl = do
  st <- get
  if position st == 0 then return True
    else return $ text st !! (position st - 1) == '\n'

-- \s
spc :: State RegexS Bool
spc = liftM isSpace currentChar <* movePosition 1 

-- \S
notSpc :: State RegexS Bool
notSpc = liftM not spc

-- \w
wc :: State RegexS Bool
wc = liftM isLetter currentChar <* movePosition 1

-- \W
notWc :: State RegexS Bool
notWc = liftM not wc

-- \d
digit :: State RegexS Bool
digit = liftM (`elem` ['0' .. '9']) currentChar <* movePosition 1

-- \D
notDigit :: State RegexS Bool
notDigit = liftM not digit

-- \w
alnum :: State RegexS Bool
alnum = liftM isAlphaNum currentChar <* movePosition 1

-- +
plus  :: State RegexS Bool -> State RegexS Bool
plus m = liftM (> 0) (greedyMatch m)
-- *
star :: State RegexS Bool -> State RegexS Bool
star m = greedyMatch m >> return True

-- |
pipe :: State RegexS Bool -> State RegexS Bool -> State RegexS Bool
pipe = liftM2 (||)

-- []
range :: String -> State RegexS Bool
range cs = liftM  (`elem` cs) currentChar <* movePosition 1

-- [^]
notRange :: String -> State RegexS Bool
notRange cs = liftM not (range cs)

-- ?
qMark :: State RegexS Bool -> State RegexS Bool
qMark m = do oldPos <- getPosition
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
  group <- getGroup varN
  case group of
    (Just str) -> do let strEndIndex = pos + length str
                     if not $ null str &&
                        str == subRange (pos, strEndIndex) (text st)
                       then setPosition strEndIndex >> return True
                       else return False
    Nothing -> return False

-- {n}
mN :: State RegexS Bool -> Int -> State RegexS Bool
mN = matchN

-- {,n}
mLN :: State RegexS Bool -> Int -> State RegexS Bool
mLN = matchAtLeastN

-- {n, n}
mN1N2 :: State RegexS Bool -> (Int, Int) -> State RegexS Bool
mN1N2 m (minM, maxM) = matchBetweenN1N2 m minM maxM

-- chain functions together and providing the end index
-- of the previous as the start of the next.
combine :: [State RegexS Bool] -> State RegexS Bool
combine regex = liftM and $ mapM acc regex
  where acc :: State RegexS Bool -> State RegexS Bool
        acc m = do len <- getTextLength
                   pos <- getPosition
                   if len < pos then return False
                     else m

-- replace all matches of (combine rs) with subStr
replaceRegex :: [State RegexS Bool] -> String -> String -> String
replaceRegex  rs ss subStr = foldr (flip replace subStr) ss (ss =~ rs)

-- return all matches of (combine rs)

matchRegex ::  [State RegexS Bool] -> String -> [(Int, Int)]
matchRegex rs ss = C.join $ map (snd . runWriter . subMatch ss (combine rs)) [0 .. length ss]
  where subMatch :: String -> State RegexS Bool -> Int -> Writer [(Int, Int)] ()
        subMatch ss re i =
          let (matched, st) = runState re (RegexS [] ss i)
              pos = position st in
          when ((pos <= length (text st)) && matched) $ tell [(i, pos)]

-- -- apply the regex on tails str and return bigest match
(=~) :: String ->  [State RegexS Bool] -> [String]
(=~) str rs = extractMatches str $ matchRegex rs (str ++ "\n")
