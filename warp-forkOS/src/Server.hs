{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (fromByteString)
import Network.HTTP.Types (status200)
import Control.Exception.Base (SomeException)
import Control.Monad (when)

main :: IO()
main = runSettings settings app -- default port is 3000

app :: Application
app _ respond = respond $ responseFile
    status200
    [("Content-Type", "text/plain")]
    "static-file.txt"
    Nothing

settings :: Settings
settings = setOnException onException defaultSettings

onException :: Maybe Request -> SomeException -> IO ()
onException _ e =
     when (defaultShouldDisplayException e)
         $ putStrLn $ show e
