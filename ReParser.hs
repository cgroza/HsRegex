module ReParser
       where
import HsRegex
  
-- skip function in order to consume parts of regexp
skip :: Bool -> [a] -> [a]
skip True _ = []
skip False xs = xs

-- parse regexp functions skip
parse [] rs s = rs  
parse (c:expr) rs s = case c of
  '.' ->  parse expr (rs ++ skip s [dot]) False
  '$' ->  parse expr (rs ++ skip s [endl]) False
  '^' ->  parse expr (rs ++ skip s [stl]) False
  '*' ->  parse expr (rs ++ skip s [star (last rs)]) True
  '+' ->  parse expr (rs ++ skip s [plus (last rs)]) True
  '?' ->  parse expr (rs ++ skip s [qMark (last rs)]) True
  '|' ->  let next = pipe (last rs) (head $ parse expr [] False)
          in parse expr ((init rs) ++ [next]) True
  _  ->   parse expr (rs ++ skip s [char c]) False

parseRange :: String -> Regex
parseRange = undefined
parseBracket :: String -> Regex
parseBracket = undefined

