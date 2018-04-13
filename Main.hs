#!/usr/bin/env runhaskell
import Parser

import Data.Ord
import Data.List
import Data.Semigroup
import qualified Data.Map as M

import qualified Options.Applicative as Opts
import Options.Applicative ((<**>))
import System.IO
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

type Essid ap = (String, [ap])

groupEssids :: [AP] -> M.Map String [AP]
groupEssids = foldr (\ap -> M.insertWith (++) (apEssid ap) [ap]) M.empty

arrange :: [Essid AP] -> [Essid (Int, AP)]
arrange = go 1
  where
    go n [] = []
    go n ((essid, aps) : rest) =
        (essid, zip [n..] . reverse $ sortBy (comparing apSignal) aps)
        : go (n + length aps) rest

fmtItem :: Essid (Int, AP) -> [String]
fmtItem (essid, aps) = essid : map ("  " ++) subs
  where
    subs =
        [ show i ++ ": " ++ show apSignal ++ "    " ++ apBssid
        | (i, AP{..}) <- aps
        ]

main :: IO ()
main = do
    opts@Options{..} <- Opts.execParser optParserInfo
    callProcess "ip" ["link", "set", optIface, "up"]
    essids <- groupEssids . parse <$> readProcess "iw" [optIface, "scan"] ""
    putStr . unlines . concatMap fmtItem . arrange $ M.toList essids

    putStr "pick your wifi > "
    hFlush stdout
    resp <- getLine
    return ()
