data Number = Pair Number Number | Regular Int

instance Show Number where
    show (Regular x) = show x
    show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

parse :: String -> Number
parse = snd . go
    where
        go :: String -> (String,Number)
        go (x:xs) =
            if x == '[' then
                let (xs',l) = go xs
                    (xs'',r) = go (tail xs')
                in (tail xs'',Pair l r)
            else
                (xs, Regular (read [x] :: Int))
        go _ = error "failed"

add :: Number -> Number -> Number
add a b = reduce $ Pair a b

reduce :: Number -> Number
reduce n =
    case explode n of
        (n', True ) -> reduce n'
        (_ , False) ->
            case split n of
                (n', True ) -> reduce n'
                (_ , False) -> n

explode :: Number -> (Number, Bool)
explode n = case go 0 n of (n', _ , _, found) -> (n', found)
    where
        go :: Int -> Number -> (Number, Maybe Int, Maybe Int, Bool)
        go _ n@(Regular _) = (n, Nothing , Nothing , False)
        go depth n@(Pair (Regular a) (Regular b)) =
            if depth >= 4 then
                (Regular 0, Just a, Just b, True)
            else
                (n, Nothing, Nothing, False)
        go depth (Pair a b) =
            case go (depth+1) a of
                (a', inl, Just x , True ) -> (Pair a' (incLeft b x), inl, Nothing , True)
                (a', inl, Nothing, True ) -> (Pair a' b            , inl, Nothing , True)
                (_ , _  ,       _, False) ->
                    case go (depth+1) b of
                        (b', Just x , inr, True ) -> (Pair (incRight a x) b', Nothing, inr    , True)
                        (b', Nothing, inr, True ) -> (Pair a b'             , Nothing, inr    , True)
                        (_ , _      , _  , False) -> (Pair a b              , Nothing, Nothing, False)

        incLeft :: Number -> Int -> Number
        incLeft (Regular x) i = Regular (x + i)
        incLeft (Pair a b) i = Pair (incLeft a i) b

        incRight :: Number -> Int -> Number
        incRight (Regular x) i = Regular (x + i)
        incRight (Pair a b) i = Pair a (incRight b i)


split :: Number -> (Number, Bool)
split n@(Regular x) =
    if x >= 10 then
        (Pair (Regular (div x 2)) (Regular (div x 2 + rem x 2)), True)
    else
        (n, False)
split (Pair a b) =
    case split a of
        (a', True ) -> (Pair a' b, True)
        (_ , False) ->
            case split b of
                (b', True) -> (Pair a b', True)
                _          -> (Pair a b, False)

sumN :: [Number] -> Number
sumN = foldl1 add

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair a b)  = 3 * magnitude a + 2 * magnitude b

maxMag :: [Number] -> Int
maxMag ns = 
    let pairs = concat [ [[x,y],[y,x]] | x <- ns, y <- ns ]
    in maximum . map (magnitude . sumN) $ pairs

main = do
    numbers <- map parse . lines <$> readFile "input.txt"
    print "# part 1, magnitude of sum"
    print $ magnitude . sumN $ numbers
    print "# part 2, max magnitude"
    print $ maxMag numbers

