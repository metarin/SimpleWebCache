module Main (main) where

import Lib (startApp)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Console.GetOpt
    ( getOpt
    , usageInfo
    , ArgDescr(ReqArg, NoArg)
    , ArgOrder(Permute)
    , OptDescr(..) )
import Network.URI
    ( parseAbsoluteURI
    , URI )

data Options = Options
    { optDownloadMissed :: Bool
    , optPortNum        :: Int
    , optReverseProxy   :: Maybe URI
    , optCacheDir       :: FilePath
    } deriving Show

defaultOptions :: Options
defaultOptions          = Options
    { optDownloadMissed = False
    , optPortNum        = 54321
    , optReverseProxy   = Nothing
    , optCacheDir       = ""
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['D'] ["download-uncached"]
        (NoArg (\ opts -> opts { optDownloadMissed = True }))
        "Downloading uncached file when accessing uncached files"
    , Option ['p'] ["port"]
        (ReqArg (\ p opts -> opts { optPortNum = fromMaybe (optPortNum opts) $ readMaybe p }) "PORT")
        "Port num (default 54321)"
    , Option ['r'] ["reverse-proxy"]
        (ReqArg (\ r opts -> opts { optReverseProxy = parseAbsoluteURI r }) "URI")
        "Acts as a reverse proxy when accessing uncached files"
    , Option ['d'] ["cache-dir"]
        (ReqArg (\ d opts -> opts { optCacheDir = d }) "DIR")
        "Location of cache directory"
    ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: SimpleWebCache [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    (opt, _) <- parseArgs args
    let
        port = optPortNum opt
        dir = optCacheDir opt
        mUri = optReverseProxy opt

    startApp port dir mUri
