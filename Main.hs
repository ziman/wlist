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

type SSID = String
type Essid ap = (SSID, [ap])

groupEssids :: [AP] -> M.Map SSID [AP]
groupEssids = foldr (\ap -> M.insertWith (++) (apEssid ap) [ap]) M.empty

arrange :: [Essid AP] -> [Essid (Int, AP)]
arrange = go 1
  where
    go n [] = []
    go n ((essid, aps) : rest) =
        (essid, zip [n..] . reverse $ sortBy (comparing apSignal) aps)
        : go (n + length aps) rest

flatten :: [Essid (Int, AP)] -> [(Int, SSID, AP)]
flatten = concatMap (\(ssid, aps) -> [(i,ssid,ap) | (i,ap) <- aps])

fmtItem :: Essid (Int, AP) -> [String]
fmtItem (essid, aps) = essid : map ("  " ++) subs
  where
    subs =
        [ show i ++ ": " ++ show apSignal ++ "    " ++ apBssid
        | (i, AP{..}) <- aps
        ]

connectTo :: Options -> (SSID, AP) -> IO ()
connectTo Options{..} (ssid, AP{..}) = do
    putStrLn $ "connecting to " ++ ssid ++ " - " ++ apBssid
    callProcess "dhcpcd" ["-k", optIface]
    callProcess "iw" [optIface, "connect", ssid, apBssid]
    callProcess "dhcpcd" [optIface]

main :: IO ()
main = do
    opts@Options{..} <- Opts.execParser optParserInfo
    callProcess "ip" ["link", "set", optIface, "up"]
    essids <- arrange . M.toList . groupEssids . parse 
        <$> readProcess "iw" [optIface, "scan"] ""
    putStr . unlines . concatMap fmtItem $ essids

    putStr "pick your wifi (or ^C to cancel)> "
    hFlush stdout

    let chooseAP = do
            resp <- read <$> getLine
            case filter (\(i,_,_) -> i == resp) (flatten essids) of
                [] -> do
                    putStrLn $ "no such AP: " ++ show resp
                    chooseAP
                [(i,essid,ap)] -> return (essid, ap)
                aps  -> error $ "should never happen: multiple APs with same id: " ++ show aps

    connectTo opts =<< chooseAP
