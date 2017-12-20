import Data.List

data RawItem = RI { riDepth :: Int, riContent :: String } deriving Show
data HierItem = HI String [HierItem] deriving Show
data Item = KV String String | L String [Item]

parseRI :: String -> RawItem
parseRI line = RI (length tabs) rest
  where
    (tabs, rest) = span (== '\t') line

processBlock :: [RawItem] -> Maybe (HierItem, [RawItem])
processBlock [] = Nothing
processBlock (RI depth content : is) = Just (block, rest)
  where
    block = HI content $ parseHI subs
    (subs, rest) = span ((> depth) . riDepth) is

parseHI :: [RawItem] -> [HierItem]
parseHI = unfoldr processBlock

fmtHI :: HierItem -> [String]
fmtHI (HI content children)
    = content : map (' ':) (concatMap fmtHI children)

parseItem :: HierItem -> Item
parseItem (HI content [])
    | (key, ':':val) <- span (/= ':') content
    = KV (clean key) val

parseItem (HI content subs)
    | (title, ':':'\t':sub) <- span (/= ':') content
    = parseItem $ HI title (HI sub []:subs)

parseItem (HI content subs)
    | (title, ":") <- span (/= ':') content
    = parseItem $ HI title subs

parseItem (HI content subs) = L (clean content) (map parseItem subs)

clean :: String -> String
clean ('*':' ':str) = str
clean (' ':str) = clean str
clean str = str

fmtI :: Item -> [String]
fmtI (KV key val) = [key ++ " = " ++ val]
fmtI (L content subs) = content : map (' ':) (concatMap fmtI subs)

main :: IO ()
main = do
    stuff <- map parseItem . parseHI . map parseRI . lines
        <$> readFile "/home/ziman/iw-scan.txt"
    mapM_ (putStrLn . unlines . fmtI) stuff
