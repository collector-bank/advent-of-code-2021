{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import qualified Control.Monad as Monad

type BinVec = [Bool]

-- showBv :: [Bool] -> String
-- showBv xs = foldr (\ x -> (:) (if x then '1' else '0')) "" xs

hex2bin :: Char -> BinVec
hex2bin d = map (=='1') $
    case d of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _   -> error "undefined digit"

bin2dec :: BinVec -> Int
bin2dec = fst . foldr  (\bit (v,pos) -> (v + if bit then 2^pos else 0, pos+1)) (0,0)

type Version = Int
type TypeId = Int
data Op = Sum | Product | Min | Max | Gt | Lt | Eq deriving Show
data PData = Literal Int | Operator Op [Packet] deriving Show
data Packet = Packet Version TypeId PData deriving Show

newtype Parser a = Parser (BinVec -> Maybe (BinVec, a)) deriving Functor

instance Applicative Parser where
    pure a = Parser (\bv -> Just (bv, a))
    (<*>) = Monad.ap

instance Monad Parser where
    (Parser p) >>= f =
            Parser $
                \bc -> case p bc of
                            Just (bv', a) ->
                                let (Parser p' ) = f a
                                in p' bv'
                            Nothing -> Nothing

runParser :: BinVec -> Parser a -> Maybe a
runParser bv (Parser p) =
    case p bv of
        Just (_, a) -> Just a
        Nothing -> Nothing

pFail :: Parser a
pFail = Parser $ const Nothing

pBit :: Parser Bool
pBit = Parser $ \case [] -> Nothing; (x:xs) -> Just (xs, x)

pEos :: Parser Bool
pEos = Parser $ \case [] -> Just ([], True); s -> Just (s, False)

pRepeat :: Parser a -> Parser [a]
pRepeat p = do
    done <- pEos
    if done then do
        return []
    else do
        x <- p
        xs <- pRepeat p
        return $ x:xs

pCount :: Int -> Parser a -> Parser [a]
pCount 0 _ = return []
pCount n p = do
    x <- p
    xs <- pCount (n-1) p
    return $ x:xs

pBitVector :: Int -> Parser BinVec
pBitVector 0 = return []
pBitVector n = do
    b <- pBit
    bs <- pBitVector (n-1)
    return $ b : bs

pVersion :: Parser Int
pVersion = bin2dec <$> pBitVector 3

pTypeId :: Parser Int
pTypeId = bin2dec <$> pBitVector 3


pLiteral :: Parser PData
pLiteral = Literal . bin2dec <$> go
    where
        go :: Parser BinVec
        go = do
            hasMore <- pBit
            value <- pBitVector 4
            if hasMore then
                (value++) <$> go
            else
                return value

typeId2Op :: Int -> Op
typeId2Op typeId = 
    case typeId of
          0 -> Sum 
          1 -> Product
          2 -> Min
          3 -> Max
          5 -> Gt
          6 -> Lt
          7 -> Eq
          _ -> error "invalid op"

pOperator :: Op -> Parser PData
pOperator op = do
    lengthTypeId <- pBit
    if lengthTypeId then do
        numSubPackets <- bin2dec <$> pBitVector 11
        packets <- pCount numSubPackets pPacket
        return $ Operator op packets
    else do
        totalLen <- bin2dec <$> pBitVector 15
        bits <- pBitVector totalLen
        let maybePackets = runParser bits (pRepeat pPacket)
        case maybePackets of
            Just packets -> return $ Operator op packets
            _ -> pFail

pPacket :: Parser Packet
pPacket = do
    version <- pVersion
    typeId <- pTypeId
    case typeId of
        4 -> Packet version typeId <$> pLiteral
        _ -> Packet version typeId <$> pOperator (typeId2Op typeId)

parse :: String -> Maybe Packet
parse hex =
    let bin = concatMap hex2bin hex
    in runParser bin pPacket

--test1 = runParser [True,False,True,False] pVersion
--test2 = parse "D2FE28"  -- Packet 6 4 (Literal 2021)
--test3 = parse "8A004A801A8002F478"

versions :: Packet -> [Int]
versions (Packet v _ (Literal _)) = [v]
versions (Packet v _ (Operator _ xs)) = v : concatMap versions xs

solve1 :: String -> Maybe Int
solve1 s = sum . versions <$> parse s

input = "005410C99A9802DA00B43887138F72F4F652CC0159FE05E802B3A572DBBE5AA5F56F6B6A4600FCCAACEA9CE0E1002013A55389B064C0269813952F983595234002DA394615002A47E06C0125CF7B74FE00E6FC470D4C0129260B005E73FCDFC3A5B77BF2FB4E0009C27ECEF293824CC76902B3004F8017A999EC22770412BE2A1004E3DCDFA146D00020670B9C0129A8D79BB7E88926BA401BAD004892BBDEF20D253BE70C53CA5399AB648EBBAAF0BD402B95349201938264C7699C5A0592AF8001E3C09972A949AD4AE2CB3230AC37FC919801F2A7A402978002150E60BC6700043A23C618E20008644782F10C80262F005679A679BE733C3F3005BC01496F60865B39AF8A2478A04017DCBEAB32FA0055E6286D31430300AE7C7E79AE55324CA679F9002239992BC689A8D6FE084012AE73BDFE39EBF186738B33BD9FA91B14CB7785EC01CE4DCE1AE2DCFD7D23098A98411973E30052C012978F7DD089689ACD4A7A80CCEFEB9EC56880485951DB00400010D8A30CA1500021B0D625450700227A30A774B2600ACD56F981E580272AA3319ACC04C015C00AFA4616C63D4DFF289319A9DC401008650927B2232F70784AE0124D65A25FD3A34CC61A6449246986E300425AF873A00CD4401C8A90D60E8803D08A0DC673005E692B000DA85B268E4021D4E41C6802E49AB57D1ED1166AD5F47B4433005F401496867C2B3E7112C0050C20043A17C208B240087425871180C01985D07A22980273247801988803B08A2DC191006A2141289640133E80212C3D2C3F377B09900A53E00900021109623425100723DC6884D3B7CFE1D2C6036D180D053002880BC530025C00F700308096110021C00C001E44C00F001955805A62013D0400B400ED500307400949C00F92972B6BC3F47A96D21C5730047003770004323E44F8B80008441C8F51366F38F240"
part1 = solve1 input

eval :: Packet -> Int
eval (Packet _ _ (Literal v)) = v
eval (Packet _ _ (Operator op subp)) =
    let subv = map eval subp
    in
        case op of 
            Sum -> sum subv
            Product -> product subv
            Min -> minimum subv
            Max -> maximum subv
            Gt -> let [l,r] = subv in if l > r then 1 else 0
            Lt -> let [l,r] = subv in if l < r then 1 else 0
            Eq -> let [l,r] = subv in if l == r then 1 else 0

solve2 :: String -> Maybe Int
solve2 s = eval <$> parse s

-- test4 = solve2 "C200B40A82"
-- test5 = solve2 "04005AC33890"
-- test6 = solve2 "CE00C43D881120"
-- test7 = solve2 "D8005AC2A8F0"
-- test8 = solve2 "F600BC2D8F"
-- test9 = solve2 "9C005AC2F8F0"
-- test10 = solve2 "9C0141080250320F1802104A08"

part2 = solve2 input