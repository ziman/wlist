module Parser
    ( parse
    )
    where

import Data.List

data RawItem = RI { riDepth :: Int, riContent :: String } deriving Show
data HierItem = HI String [HierItem] deriving Show
data Item = KV String String | L String [Item] deriving Show
data AP = AP
    { apEssid :: String
    , apBssid :: String
    , apFreq :: Int
    , apSignal :: Float
    , apStationCount :: Int
    , apUtilisation :: (Int, Int)  -- fraction
    , apDetails :: [Item]
    }

parse :: String -> [AP]
parse = map (parseAP . parseItem) . parseHI . map parseRI . lines

parseAP :: Item -> AP
parseAP it@(L title subs)
    | Just essid <- get ["SSID"] subs
    , Just freq <- get ["freq"] subs
    , Just signal <- get ["signal"] subs
    , Just stationCount <- get ["BSS Load","station count"] subs
    , Just apUtil <- get ["BSS Load", "channel utilisation"] subs
    = AP {
        apEssid = essid,
        apBssid = bssid title,
        apFreq = read freq,
        apSignal = read . fst $ span (/=' ') signal,
        apStationCount = read stationCount,
        apUtilisation =
          let (cnt, '/':tot) = span (/= '/') apUtil
            in (read cnt, read tot),
        apDetails = it
    }
  where
    bssid :: String
    bssid ('B':'S':'S':' ':rest) = fst $ span (/= '(') rest
    bssid s = error $ "can't parse BSSID from: " ++ show s

    get :: [String] -> [Item] -> Maybe String
    get [k] (KV k' v : is) | k == k' = Just v
    get [k] (_:is) = get [k] is

    get (k:ks) (L k' subs : _) | k == k' = get ks subs
    get ks (_:is) = get ks is

    get _ _ = Nothing

parseAP it = error $ "could not parse AP from " ++ show it

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
    = KV (clean key) (clean val)

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
