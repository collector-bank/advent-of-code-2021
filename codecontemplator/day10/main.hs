import Data.List (sort)
import Text.Printf (printf)

data Result = Success | Incomplete [Char] | Corrupted Char deriving Show

solvePart1 :: [Result] -> Int
solvePart1 = sum . map scoreCorrupted
    where
        scoreCorrupted :: Result -> Int 
        scoreCorrupted (Corrupted c) = 
            case c of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137
                _ -> error "invalid char"        
        scoreCorrupted _ = 0

solvePart2 :: [Result] -> Int
solvePart2 = head . middle . sort . filter (/=0) . map scoreIncomplete
    where
        middle :: [a] -> [a]
        middle l@(_:_:_:_) = middle $ tail $ init l
        middle l           = l

        scoreIncomplete :: Result -> Int
        scoreIncomplete (Incomplete stack) = foldr ((\e s -> 5 * s + e) . scoreChar) 0 (reverse stack)
            where
                scoreChar :: Char -> Int
                scoreChar c =
                    case c of
                        '(' -> 1
                        '[' -> 2
                        '{' -> 3
                        '<' -> 4
                        _ -> error "invalid char"
        scoreIncomplete _ = 0

parse :: [Char] -> [Char] -> Result
parse [] [] = Success
parse stack []  = Incomplete stack
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
    let seq = map (parse []) input
    printf "part 1 = %d\n" $ solvePart1 seq --  469755
    printf "part 2 = %d\n" $ solvePart2 seq -- 2762335572