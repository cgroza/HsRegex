module HsRegex (subRange, char, dot, endl, stl, spc, notSpc, wc, notWc, digit,
                notDigit, alnum, plus, star, pipe, range, notRange, qMark, wb,
                reGroup, var, mN, mLN, mN1N2, combine, (=~), RegexS)
       where

import           Control.Monad.Loops
import           Control.Monad.State  as S
import           Control.Monad.Writer
import           Data.Char
import           Data.String.Utils

-- [Int] contains the mathches of a single Regex
-- [[Int]] records the history of matches of the full combined regex.
data RegexS = RegexS { groups :: [String], text :: String, positions :: [[Int]]}
type Regex a = State RegexS a

position :: RegexS -> Int
position = last . last . positions

-- for extracting substrings
subRange :: (Int, Int) -> [a] -> [a]
subRange (x, y) = take (y - x) . drop x

-- for extracting strings from index tuple
extractMatches :: String -> [(Int, Int)] -> [String]
extractMatches str = fmap (`subRange` str)

addGroup :: String -> Regex ()
addGroup g = do (RegexS gs t p) <- get
                put $ RegexS (gs ++ [g]) t p

getGroup :: Int -> Regex (Maybe String)
getGroup varN = do gs <- liftM groups get
                   return $ if varN < length gs then Just $ gs !! (varN - 1)
                            else Nothing

setPos :: Int -> Regex ()
setPos p'  = do (RegexS g t ps) <- get
                put $ RegexS g t (ps ++ [[p']])

-- Adds  new collection of matches.
movePos :: Int -> Regex ()
movePos i = getPos >>= (setPos . (i +))

-- Appends to current collection of matches.
addPos :: Int -> Regex ()
addPos i = do (RegexS g t ps) <- get
              put $ RegexS g t (init ps ++ [last ps ++ [i]])

popPos :: Regex Int
popPos = do st@(RegexS g t ps) <- get
            put $ RegexS g t (init ps ++ [init $ last ps])
            return $ position st

-- Removes the current collection of matches and returns it.
popPosCollection :: Regex [Int]
popPosCollection =  do (RegexS g t ps) <- get
                       put $ RegexS g t (init ps)
                       return $ last ps

getPrevPosCollection :: Regex [Int]
getPrevPosCollection =  liftM (last . init . positions) get

currentChar :: Regex Char
currentChar = liftM (\st -> text st !! position st) get

-- Returns furthermost position in current collection of matches.
getPos :: Regex Int
getPos = liftM position get

getTextLength :: Regex Int
getTextLength = liftM (length . text) get

match :: Regex Bool -> Regex Bool
match m = do
  matched <- m
  st <- get
  if position st < length (text st) && matched
    then popPosCollection >>= addPos . head >> return True
    else popPosCollection >> return False

-- match using regexp m as many times as possible
greedyMatch :: Regex Bool -> Regex Int
greedyMatch m  = liftM length $ whileM (match m) (return ())

-- match using regexp m N times
-- if N is not satisfied, fst is false
matchN :: Regex Bool -> Int -> Regex Bool
matchN m i =  andM $ replicate i (match m)

-- match using regexp m at least N times
-- if N is not satisfied, fst is false
matchAtLeastN :: Regex Bool -> Int -> Regex Bool
matchAtLeastN m i = liftM (i <=) (greedyMatch m)

-- match using regexp m between N1 and N2
-- if not between N1 N2, fst is false
matchBetweenN1N2 :: Regex Bool -> Int -> Int -> Regex Bool
matchBetweenN1N2 m n1 n2 = liftM ((>= n1) . length . takeWhile id) $
                           replicateM n2 (match m)

-- characters
char :: Char -> Regex Bool
char c = liftM (== c) currentChar <* movePos 1

-- .
dot :: Regex Bool
dot =  liftM ('\n' /=) currentChar <* movePos 1

-- $
endl :: Regex Bool
endl =  orM [liftM2 (>=) getPos getTextLength, liftM (== '\n') currentChar] <*
        movePos 1

-- ^
stl :: Regex Bool
stl = do
  st <- get
  if position st == 0 then return True
    else return $ text st !! (position st - 1) == '\n'

-- \s
spc :: Regex Bool
spc = liftM isSpace currentChar <* movePos 1

-- \S
notSpc :: Regex Bool
notSpc = liftM not spc

-- \w
wc :: Regex Bool
wc = liftM isLetter currentChar <* movePos 1

-- \W
notWc :: Regex Bool
notWc = liftM not wc

-- \d
digit :: Regex Bool
digit = liftM (`elem` ['0' .. '9']) currentChar <* movePos 1

-- \D
notDigit :: Regex Bool
notDigit = liftM not digit

-- \w
alnum :: Regex Bool
alnum = liftM isAlphaNum currentChar <* movePos 1

-- +
plus  :: Regex Bool -> Regex Bool
plus m = liftM (> 0) (greedyMatch m)
-- *
star :: Regex Bool -> Regex Bool
star m = greedyMatch m >> return True

-- |
pipe :: Regex Bool -> Regex Bool -> Regex Bool
pipe = liftM2 (||)

-- []
range :: String -> Regex Bool
range cs = liftM  (`elem` cs) currentChar <* movePos 1

-- [^]
notRange :: String -> Regex Bool
notRange cs = liftM not (range cs)

-- ?
qMark :: Regex Bool -> Regex Bool
qMark m = do oldPos <- getPos
             mR <- m
             if not mR then setPos oldPos >> return True
               else return True

-- \b
wb :: Regex Bool
wb = plus spc >> alnum

-- (regexp)
reGroup :: [Regex Bool] -> Regex Bool
reGroup rs = do initialPos <- getPos
                matched <- combine rs
                finalPos <- getPos
                st <- get
                addGroup $ subRange (initialPos, finalPos) (text st)
                return matched
-- $n
var :: Int -> Regex Bool
var varN = do
  pos <- getPos
  st <- get
  group <- getGroup varN
  case group of
    (Just str) -> do let strEndIndex = pos + length str
                     if not $ null str &&
                        str == subRange (pos, strEndIndex) (text st)
                       then setPos strEndIndex >> return True
                       else return False
    Nothing -> return False

-- {n}
mN :: Regex Bool -> Int -> Regex Bool
mN = matchN

-- {,n}
mLN :: Regex Bool -> Int -> Regex Bool
mLN = matchAtLeastN

-- {n, n}
mN1N2 :: Regex Bool -> (Int, Int) -> Regex Bool
mN1N2 m (minM, maxM) = matchBetweenN1N2 m minM maxM

-- chain functions together and providing the end index
-- of the previous as the start of the next.
combine :: [Regex Bool] -> Regex Bool
combine regex = liftM and $ mapM acc regex
  where acc :: Regex Bool -> Regex Bool
        acc m = do len <- getTextLength
                   pos <- getPos
                   if len <= pos then return False
                     else do matched <- m
                             if matched then return True
                               else do coll <- getPrevPosCollection
                                       if length coll == 1 then return False
                                         else popPosCollection >> popPos >> acc m

-- replace all matches of (combine rs) with subStr
replaceRegex :: [Regex Bool] -> String -> String -> String
replaceRegex  rs ss subStr = foldr (`replace` subStr) ss (ss =~ rs)

-- return all matches of (combine rs)

matchRegex ::  [Regex Bool] -> String -> [(Int, Int)]
matchRegex rs ss = S.join $ map (snd . runWriter . subMatch ss (combine rs)) [0 .. length ss]
  where subMatch :: String -> Regex Bool -> Int -> Writer [(Int, Int)] ()
        subMatch str re i =
          let (matched, st) = runState re (RegexS [] str [[i]])
              pos = position st in
          when ((pos <= length (text st)) && matched) $ tell [(i, pos)]

-- -- apply the regex on tails str and return bigest match
(=~) :: String ->  [Regex Bool] -> [String]
(=~) str rs = extractMatches str $ matchRegex rs (str ++ "\n")
