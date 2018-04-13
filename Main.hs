#!/usr/bin/env runhaskell
import Parser

import Data.Ord
import Data.List
import Data.Semigroup
import qualified Data.Map as M

import qualified Options.Applicative as Opts
import Options.Applicative ((<**>))
import System.Process

data Options = Options
    { optIface :: String
    }

optParser :: Opts.Parser Options
optParser = Options
    <$> Opts.argument Opts.str
        ( Opts.metavar "IFACE"
        <> Opts.help "the wireless interface")

optParserInfo :: Opts.ParserInfo Options
optParserInfo = Opts.info (optParser <**> Opts.helper)
    ( Opts.fullDesc
    <> Opts.progDesc "Select WiFi network on IFACE"
    <> Opts.header "wlist - WiFi network selector")

type Essids = M.Map String [AP]

groupEssids :: [AP] -> Essids
groupEssids = foldr (\ap -> M.insertWith (++) (apEssid ap) [ap]) M.empty

fmtItem :: (String, [AP]) -> [String]
fmtItem (essid, aps) = essid : map ("  " ++) subs
  where
    subs = [apBssid ++ " " ++ show apSignal | AP{..} <- sortBy (comparing apSignal) aps]

main :: IO ()
main = do
    opts@Options{..} <- Opts.execParser optParserInfo
    callProcess "ip" ["link", "set", optIface, "up"]
    essids <- groupEssids . parse <$> readProcess "iw" [optIface, "scan"] ""
    putStr . unlines . concatMap fmtItem $ M.toList essids
