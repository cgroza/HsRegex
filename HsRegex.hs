module HsRegex (subRange, char, dot, endl, stl, spc, notSpc, wc, notWc, digit,
                notDigit, alnum, plus, star, pipe, range, notRange, qMark, wb,
                reGroup, var, mN, mLN, mN1N2, combine, (=~), RegexS)
       where

import           Control.Monad as C
import           Control.Monad.Loops
import           Control.Monad.State  as S
import           Control.Monad.Writer
import           Data.Char
import           Data.List
import qualified Data.Text as T
import           Data.String.Utils

-- [Int] contains the mathches of a single Regex
-- [[Int]] records the history of matches of the full combined regex.
data RegexS = RegexS { failed :: Bool, groups :: [T.Text], text :: T.Text, positions :: [[Int]]} deriving (Show)
type Regex = State RegexS

position :: RegexS -> Int
position = head . head . positions

-- for extracting substrings
subRange :: (Int, Int) -> T.Text -> T.Text
subRange (x, y) = T.take (y - x) . T.drop x

-- for extracting strings from index tuple
extractMatches :: T.Text -> [(Int, Int)] -> [T.Text]
extractMatches str = fmap (`subRange` str)

addGroup :: T.Text -> Regex ()
addGroup g = modify $ \(RegexS f gs t p) -> RegexS f (gs ++ [g]) t p

getGroup :: Int -> Regex (Maybe T.Text)
getGroup varN = do gs <- gets groups
                   return $ if varN < length gs then Just $ gs !! (varN - 1)
                            else Nothing

setPos :: Int -> Regex ()
setPos p  = modify $ \(RegexS f g t psCol) -> RegexS f g t ([p] : psCol)

-- Adds  new collection of matches.
movePos :: Int -> Regex ()
movePos i = getPos >>= (setPos . (i +))

-- Appends to current collection of matches.
addPos :: Int -> Regex ()
addPos p = modify $ \(RegexS f g t (ps:psCol)) -> RegexS f g t ((p:ps):psCol)

-- popPos :: Regex Int
-- popPos = state $ \(RegexS f g t ((p:ps):psCol)) -> (p, RegexS f g t (ps:psCol))

-- Removes the current collection of matches and returns it.
popPosCollection :: Regex [Int]
popPosCollection = state $ \(RegexS f g t (ps:psCol)) -> (ps, RegexS f g t psCol)

-- getPrevPosCollection :: Regex [Int]
-- getPrevPosCollection =  gets $ head . tail . positions

currentChar :: Regex Char
currentChar = gets $ \s -> T.index  (text s) (position s)

-- Returns furthermost position in current collection of matches.
getPos :: Regex Int
getPos = gets position

getTextLength :: Regex Int
getTextLength = gets $ T.length . text

getMatches :: RegexS -> [Int]
getMatches = head . positions

failRegex :: Regex Bool
failRegex = modify (\(RegexS _ g t p) -> RegexS True g t p) >> return False


successRegex :: Regex Bool
successRegex = modify (\(RegexS _ g t p) -> RegexS False g t p) >> return True

match :: Regex Bool -> Regex Bool
match m = do
  matched <- m
  st <- get
  if position st < T.length (text st) && matched
    then popPosCollection >>= addPos . head >> successRegex
    else popPosCollection >> failRegex

-- match using regexp m as many times as possible
greedyMatch :: Regex Bool -> Regex Int
greedyMatch m  = length <$> whileM (match m) (return ())

-- match using regexp m N times
-- if N is not satisfied, fst is false
matchN :: Regex Bool -> Int -> Regex Bool
matchN m i =  andM $ replicate i (match m)

-- match using regexp m at least N times
-- if N is not satisfied, fst is false
matchAtLeastN :: Regex Bool -> Int -> Regex Bool
matchAtLeastN m i = (i <=) <$> greedyMatch m

-- match using regexp m between N1 and N2
-- if not between N1 N2, fst is false
matchBetweenN1N2 :: Regex Bool -> Int -> Int -> Regex Bool
matchBetweenN1N2 m n1 n2 = (>= n1) . length . takeWhile id <$>
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
stl = gets $ \st -> position st == 0 || T.index (text st) (position st - 1) == '\n'

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
plus m = do nMatches <- greedyMatch m
            if nMatches > 1 then successRegex else failRegex
-- *
star :: Regex Bool -> Regex Bool
star m = greedyMatch m >> successRegex

-- |
pipe :: Regex Bool -> Regex Bool -> Regex Bool
pipe m1 m2 = do initialState <- get
                match1 <- m1
                if match1 then
                  successRegex
                  else do put initialState
                          match2 <- m2
                          if match2 then
                            successRegex
                            else failRegex

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
             if not mR then setPos oldPos >> successRegex
               else successRegex

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
    (Just str) -> do let strEndIndex = pos + T.length str
                     if not $ T.null str &&
                        str == subRange (pos, strEndIndex) (text st)
                       then setPos strEndIndex >> successRegex
                       else failRegex
    Nothing -> failRegex

-- {n}
mN :: Regex Bool -> Int -> Regex Bool
mN = matchN

-- {,n}
mLN :: Regex Bool -> Int -> Regex Bool
mLN = matchAtLeastN

-- {n, n}
mN1N2 :: Regex Bool -> (Int, Int) -> Regex Bool
mN1N2 m (minM, maxM) = matchBetweenN1N2 m minM maxM

genStates :: RegexS -> [RegexS]
genStates (RegexS f g t (p:ps)) = fmap (RegexS f g t  . (:ps) . (`drop` p)) [0.. length p - 1]

sortMatches :: [Regex RegexS] -> Regex [RegexS]
sortMatches rs = sortOn position . filter (not . failed) <$> C.sequence rs

withRegexState :: [Regex Bool] -> RegexS -> Regex RegexS
withRegexState [] st = return st
withRegexState (r:rs) st = do put st
                              pos <- getPos
                              len <- getTextLength
                              if pos < len then
                                do m <- r
                                   s <- get
                                   matches <- liftM getMatches get
                                   if m then
                                     do successRegex
                                        if length matches > 1 then
                                          do newSts <- sortMatches $ map (withRegexState rs) (genStates s)
                                             if not $ null newSts then
                                               let newSt = last newSts in put newSt >> return newSt
                                               else failRegex >> get
                                          else withRegexState rs s
                                     else failRegex >> return s
                                else failRegex >> return st

-- chain functions together and providing the end index
-- of the previous as the start of the next.
-- combine :: [Regex Bool] -> Regex Bool
-- combine regex = liftM and $ mapM acc regex
--   where acc :: Regex Bool -> Regex Bool
--         acc m = do len <- getTextLength
--                    pos <- getPos
--                    if len <= pos then failRegex
--                      else do matched <- m
--                              if matched then successRegex
--                                else do coll <- getPrevPosCollection
--                                        if length coll == 1 then failRegex
--                                          else popPosCollection >> popPos >> acc m

combine :: [Regex Bool] -> Regex Bool
combine rs = do initialState <- get
                finalState <- withRegexState rs initialState
                return $ failed finalState
-- replace all matches of (combine rs) with subStr
replaceRegex :: [Regex Bool] -> String -> String -> String
replaceRegex  rs ss subStr = foldr (`replace` subStr) ss (ss =~ rs)

-- return all matches of (combine rs)

matchRegex ::  [Regex Bool] -> T.Text -> [(Int, Int)]
matchRegex rs ss = S.join $ map (snd . runWriter . subMatch ss (combine rs)) [0 .. T.length ss]
  where subMatch :: T.Text -> Regex Bool -> Int -> Writer [(Int, Int)] ()
        subMatch str re i =
          let st = execState re (RegexS False [] str [[i]])
              pos = position st in
          when ((pos <= T.length (text st)) && (not $ failed st)) $ tell [(i, pos)]

-- -- apply the regex on tails str and return bigest match
(=~) :: String ->  [Regex Bool] -> [String]
(=~) str rs = fmap T.unpack $ extractMatches textStr $ matchRegex rs
              (T.append textStr $ T.pack "\n") where textStr = T.pack str
