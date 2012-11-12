module Common
where

salt = "RANDOM_SALT"
dateLen :: Int
dateLen = 19

replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll [] _ _ = []
replaceAll s find repl =
    if take (length find) s == find
        then repl ++ (replaceAll (drop (length find) s) find repl)
        else [head s] ++ (replaceAll (tail s) find repl)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ drop (length find) s
        else [head s] ++ (replace (tail s) find repl)

