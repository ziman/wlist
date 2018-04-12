{-# LANGUAGE RecordWildCards #-}
module Parser
    ( parse
    , AP(..)
    )
    where

import Data.List
import Data.Maybe

-- Stage 1: raw items
data RawItem = RI { riDepth :: Int, riContent :: String } deriving Show

-- Stage 2: hierarchised items
data HierItem = HI String [HierItem] deriving Show

-- Stage 3: parsed items
data Item
    = KV String String  -- "key: value" item
    | L String [Item]   -- captioned sublist of items
    deriving Show

-- Stage 4: AP from parsed items
data AP = AP
    { apEssid :: String
    , apBssid :: String
    , apFreq :: Int
    , apSignal :: Float
    , apStationCount :: Maybe Int
    , apUtilisation :: Maybe (Int, Int)  -- fraction
    , apDetails :: [Item]
    }

instance Show AP where
    show AP{..} = unlines . map unwords $
        [ [apEssid, apBssid]
        , ["  ", show apSignal, show apStationCount]
        ]

parse :: String -> [AP]
parse = map (parseAP . parseItem) . parseHI . map parseRI . lines

unwrap :: String -> Maybe a -> a
unwrap = fromMaybe . error

parseAP :: Item -> AP
parseAP it@(L title subs)
    | essid <- fromMaybe "(no essid)" $ get ["SSID"] subs
    , freq <- unwrap "freq" $ get ["freq"] subs
    , signal <- unwrap "signal" $ get ["signal"] subs
    , stationCount <- get ["BSS Load","station count"] subs
    , apUtil <- get ["BSS Load", "channel utilisation"] subs
    = AP {
        apEssid = essid,
        apBssid = bssid title,
        apFreq = read freq,
        apSignal = read . fst $ span (/=' ') signal,
        apStationCount = read <$> stationCount,
        apUtilisation = (\apUtil ->
          let (cnt, '/':tot) = span (/= '/') apUtil
            in (read cnt, read tot)) <$> apUtil,
        apDetails = subs
    }
  where
    bssid :: String -> String
    bssid ('B':'S':'S':' ':rest) = fst $ span (/= '(') rest
    bssid s = error $ "can't parse BSSID from: " ++ show s

    get :: [String] -> [Item] -> Maybe String
    get [k] (KV k' v : is) | k == k' = Just v
    get [k] (_:is) = get [k] is

    get (k:ks) (L k' subs : _) | k == k' = get ks subs
    get ks (_:is) = get ks is

    get ks _ = Nothing

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
