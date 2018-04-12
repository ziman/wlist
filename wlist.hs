#!/usr/bin/env runhaskell

{-# LANGUAGE RecordWildCards #-}
import Parser

import Data.Ord
import Data.List
import qualified Data.Map as M

type Essids = M.Map String [AP]

groupEssids :: [AP] -> Essids
groupEssids = foldr (\ap -> M.insertWith (++) (apEssid ap) [ap]) M.empty

fmtItem :: (String, [AP]) -> [String]
fmtItem (essid, aps) = essid : map ("  " ++) subs
  where
    subs = [apBssid ++ " " ++ show apSignal | AP{..} <- sortBy (comparing apSignal) aps]

main :: IO ()
main = do
    essids <- groupEssids . parse <$> getContents
    putStr . unlines . concatMap fmtItem $ M.toList essids
