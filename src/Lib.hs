{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Application.Static
    ( defaultFileServerSettings
    , StaticSettings(ss404Handler)
    )
import Network.Wai.Logger (withStdoutLogger)
import Network.URI (URIAuth(uriRegName))
import Network.HTTP.Client
    ( newManager
    , defaultManagerSettings
    , Manager
    )
import qualified Network.HTTP.Client as HC
    ( Request
    , Request(host)
    , Request(path)
    , Request(method))
import qualified Network.HTTP.Simple as HCC
    ( httpLBS
    , getResponseBody
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.ReverseProxy
    ( waiProxyToSettings
    , defaultWaiProxySettings
    , ProxyDest(ProxyDest)
    , WaiProxySettings (wpsLogRequest)
    , WaiProxyResponse(WPRModifiedRequest, WPRModifiedRequestSecure)
    )
import Data.Maybe
    ( fromJust
    , isNothing
    )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL (writeFile)
import Data.String.Conversions (convertString)
import Text.Printf (printf)

type API = Raw

data ReverseProxySettings = ReverseProxySettings
    {   rpUri       :: URI
    ,   rpManager   :: Manager
    ,   rpCacheDir  :: Maybe FilePath
    }

startApp :: Port -> FilePath -> Bool -> Maybe URI -> IO ()
startApp port dir isDownloadMissed mUri = withStdoutLogger $ \applogger -> case mUri of
    Nothing -> runSettings (settings applogger) $ app dir Nothing
    _ -> do
        manager <- case scheme of
            "http:" -> newManager defaultManagerSettings
            "https:" -> newTlsManager
            _ -> ioError (userError "invalid uri scheme")
        let mrp = Just ReverseProxySettings { rpUri = fromJust mUri, rpManager = manager, rpCacheDir = cacheDir }
        runSettings (settings applogger) $ app dir mrp
        where
            uri = fromJust mUri
            scheme = uriScheme uri
            cacheDir = if isDownloadMissed
                then Just dir
                else Nothing
    where
        settings logger = setPort port $ setLogger logger defaultSettings

app :: FilePath -> Maybe ReverseProxySettings -> Application
app dir mrp = serve api $ server dir mrp

api :: Proxy API
api = Proxy

server :: FilePath -> Maybe ReverseProxySettings -> Server API
server dir mrp = serveDirectoryWith settings
    where
        rp = fromJust mrp
        on404Func = if isNothing mrp
            then Nothing
            else Just (reverseProxy rp)
        settings = (defaultFileServerSettings dir) { ss404Handler = on404Func }

reverseProxy :: ReverseProxySettings -> Application
reverseProxy rp =
    waiProxyToSettings
        ( \request ->
        return $
            modifiedRequest
            ( request
                { requestHeaders = ("Host", hostname) : filter (\x -> fst x /= "Host") (requestHeaders request)
                }
            )
            (ProxyDest hostname reqPort)
        )
        defaultWaiProxySettings { wpsLogRequest = logger }
        manager
    where
        uri = rpUri rp
        scheme = uriScheme uri
        isHttps = scheme == "https:"
        modifiedRequest = if isHttps then WPRModifiedRequestSecure else WPRModifiedRequest
        reqPort = if isHttps then 443 else 80
        manager = rpManager rp
        uriAuth = fromJust $ uriAuthority $ rpUri rp
        hostname = convertString $ uriRegName uriAuth
        cacheDir = rpCacheDir rp
        logger = maybe rpLog rpLogWithDl cacheDir

rpLog :: HC.Request -> IO ()
rpLog req =
    putStrLn $ printf "\"%s %s\" reversed to %s" reqMethod reqPath reqHost
    where
        toString = convertString :: BS.ByteString -> String
        reqHost =  toString $ HC.host req
        reqPath = toString $ HC.path req
        reqMethod = toString $ HC.method req

rpLogWithDl :: FilePath -> HC.Request -> IO ()
rpLogWithDl path req = do
    rpLog req
    contents <- HCC.httpLBS req
    BSL.writeFile (path ++ reqPath) $ HCC.getResponseBody contents
    putStrLn $ "Downloaded " ++ reqPath ++ " to " ++ path
    where
        reqPath = convertString $ HC.path req
