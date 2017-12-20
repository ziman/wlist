import Parser

main :: IO ()
main = do
    aps <- parse <$> readFile "/home/ziman/iw-scan.txt"
    print aps
