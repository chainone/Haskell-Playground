{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Proxy
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager, defaultManagerSettings)

import Data.ByteString.Lazy




type API = "file" :> ReqBody '[OctetStream] ByteString :> PostNoContent '[OctetStream] NoContent


api :: Proxy API
api = Proxy

upload ::  ByteString -> Manager -> BaseUrl -> ExceptT ServantError IO NoContent
upload = client api

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runExceptT $ do
   let baseURL = BaseUrl Http "localhost" 8083 ""
   manager <- liftIO $ newManager defaultManagerSettings
   content <- liftIO $ Data.ByteString.Lazy.readFile "diff.txt"
   _ <- upload content manager baseURL
   return ()
