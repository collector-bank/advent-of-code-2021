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


-- test = parse "[[[[7,1],[0,0]],[6,[8,2]]],[8,[3,8]]]"

add :: Number -> Number -> Number
add a b = reduce $ Pair a b 

reduce :: Number -> Number
reduce n =
    case explode n of
        (n', True) -> reduce n'
        (_, False) ->
            case split n of
                (n', True) -> reduce n'
                (_, False) -> n

data InstructionLeft = NoneL | AddLeft Int
data InstructionRight = NoneR | AddRight Int 

explode :: Number -> (Number, Bool)
explode n = case go 0 n of (n', _ , _, found) -> (n', found)
    where 
        go :: Int -> Number -> (Number, InstructionLeft, InstructionRight, Bool)
        go _ n@(Regular _) = (n, NoneL, NoneR, False)
        go depth n@(Pair (Regular a) (Regular b)) = 
            if depth >= 4 then
                (Regular 0, AddLeft a, AddRight b, True)
            else
                (n, NoneL, NoneR, False)
        go depth (Pair a b) =
            case go (depth+1) a of
                (a', inl, AddRight x, True  ) -> (Pair a' (incLeft b x), inl, NoneR, True)
                (a', inl, NoneR     , True  ) -> (Pair a' b            , inl, NoneR, True)
                (_ , _  ,          _, False ) -> 
                    case go (depth+1) b of
                        (b', AddLeft x, inr, True)  -> (Pair (incRight a x) b', NoneL, inr, True   )
                        (b', NoneL    , inr, True)  -> (Pair a b'             , NoneL, inr, True   )
                        (_ , _        , _  , False) -> (Pair a b              , NoneL, NoneR, False)

        incLeft :: Number -> Int -> Number
        incLeft (Regular x) i = {-trace ("incLeft("++show x ++","++show i++")")-} (Regular (x + i))
        incLeft (Pair a b) i = Pair (incLeft a i) b

        incRight :: Number -> Int -> Number
        incRight (Regular x) i = {-trace ("incRight("++show x ++","++show i++")")-} (Regular (x + i))
        incRight (Pair a b) i = Pair a (incRight b i)


-- test2 = fst $ explode $ parse "[[[[[9,8],1],2],3],4]"
-- a2 = show test2 == "[[[[0,9],2],3],4]"

-- test3 = fst $ explode $ parse "[7,[6,[5,[4,[3,2]]]]]"
-- a3 = show test3 == "[7,[6,[5,[7,0]]]]"

-- test4 = fst $ explode $ parse "[[6,[5,[4,[3,2]]]],1]"
-- test5 = fst $ explode $ parse "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
-- test6 = fst $ explode $ parse "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

split :: Number -> (Number, Bool)
split n = go n
    where
        go n@(Regular x) = 
            if x >= 10 then 
                (Pair (Regular (div x 2)) (Regular (div x 2 + rem x 2)), True)
            else
                (n, False)
        go (Pair a b) = 
            case go a of
                (a', True) -> (Pair a' b, True)
                (_ , False) -> 
                    case go b of 
                        (b', True) -> (Pair a b', True)
                        _ -> (Pair a b, False)

-- test7 = 
--     let l = parse "[[[[4,3],4],4],[7,[[8,4],9]]]"
--         r = parse "[1,1]"
--     in
--         add l r  -- [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

sumN :: [Number] -> Number
sumN = foldl1 add          

-- test8 = sumN $ map parse ["[1,1]", "[2,2]", "[3,3]", "[4,4]"]  -- [[[[1,1],[2,2]],[3,3]],[4,4]]
-- test9 = sumN $ map parse ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"]
-- test10 = sumN $ map parse ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"]

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair a b) =
    let ma = magnitude a
        mb = magnitude b
    in
        3 * ma + 2 * mb

maxMag :: [Number] -> Int
maxMag ns = maximum  [ max (magnitude (sumN [x,y])) (magnitude (sumN [y,x])) | x <- ns, y <- ns ]
main = do
    numbers <- map parse . lines <$> readFile "input.txt"
    print "# part 1"
    print $ "sum : " ++ show (sumN numbers)
    print $ "magnitude : " ++ show (magnitude $ sumN numbers)
    print "# part 2"
    print $ "max magnitude : " ++ show (maxMag numbers)

