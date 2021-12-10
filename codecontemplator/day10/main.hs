
data Result = Success | Incomplete | Corrupted Char deriving Show

score (Corrupted c) = 
    case c of
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137
        _ -> error "invalid char"        
score _ = 0

parse :: [Char] -> [Char] -> Result
parse [] [] = Success
parse _ []  = Incomplete
parse stack (x:xs) = 
    if x `elem` ['(', '{', '<', '['] then
        parse (x:stack) xs
    else
        case stack of
            (y:ys) | y == '{' && x == '}' -> continue ys
            (y:ys) | y == '(' && x == ')' -> continue ys
            (y:ys) | y == '<' && x == '>' -> continue ys
            (y:ys) | y == '[' && x == ']' -> continue ys
            _ -> Corrupted x
        where continue ys = parse ys xs
        
main = do
    input <- lines <$> readFile "input.txt"
    print $ sum $ map (score . parse []) input