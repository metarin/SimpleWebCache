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
    , StaticSettings(ss404Handler) )
import Network.Wai.Logger (withStdoutLogger)
import Network.URI (URIAuth(uriRegName))
import Network.HTTP.Client
    ( newManager
    , defaultManagerSettings
    , Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.ReverseProxy
    ( defaultOnExc
    , waiProxyTo
    , ProxyDest(ProxyDest)
    , WaiProxyResponse(WPRModifiedRequest, WPRModifiedRequestSecure) )
import Data.Maybe
    ( fromJust
    , isNothing )
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
    ( toLazyByteString
    , string7)

type API = Raw

data ReverseProxySettings = ReverseProxySettings
    {   rpUri     :: URI
    ,   rpManager :: Manager
    }

startApp :: Port -> FilePath -> Maybe URI -> IO ()
startApp port dir mUri = withStdoutLogger $ \applogger -> case mUri of
    Nothing -> runSettings (settings applogger) $ app dir Nothing
    _ -> do
        manager <- case scheme of
            "http:" -> newManager defaultManagerSettings
            "https:" -> newTlsManager
            _ -> ioError (userError "invalid uri scheme")
        let mrp = Just ReverseProxySettings { rpUri = fromJust mUri, rpManager = manager }
        runSettings (settings applogger) $ app dir mrp
        where
            uri = fromJust mUri
            scheme = uriScheme uri
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
            then Just (reverseProxy rp)
            else Nothing
        settings = (defaultFileServerSettings dir) { ss404Handler = on404Func }

reverseProxy :: ReverseProxySettings -> Application
reverseProxy rp =
    waiProxyTo
        ( \request ->
        return $
            modifiedRequest
            ( request
                { requestHeaders = ("Host", hostname) : filter (\x -> fst x /= "Host") (requestHeaders request)
                }
            )
            (ProxyDest hostname reqPort)
        )
        defaultOnExc
        manager
    where
        uri = rpUri rp
        scheme = uriScheme uri
        isHttps = scheme == "https:"
        modifiedRequest = if isHttps then WPRModifiedRequestSecure else WPRModifiedRequest 
        reqPort = if isHttps then 443 else 80
        manager = rpManager rp
        uriAuth = fromJust $ uriAuthority $ rpUri rp
        hostname = toStrict . toLazyByteString . string7 $ uriRegName uriAuth
