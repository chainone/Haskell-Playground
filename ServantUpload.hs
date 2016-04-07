{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString.Lazy -- (ByteString, fromStrict, toStrict, writeFile)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

-- -- Backends for file upload: in memory or in /tmp ?
-- instance MimeRender OctetStream ByteString where
--     mimeRender _ = id
--
-- -- | `fromStrict`
-- instance MimeRender OctetStream BS.ByteString where
--     mimeRender _ = fromStrict

type API = "file" :> ReqBody '[OctetStream] ByteString :> PostNoContent '[OctetStream] NoContent

filesHandler :: ByteString -> ExceptT ServantErr IO NoContent
filesHandler content = do
      liftIO $ Data.ByteString.Lazy.writeFile "test.xx" content
      return NoContent

api :: Proxy API
api = Proxy

server :: Server API
server = filesHandler

app :: Application
app = serve api server

main :: IO ()
main = run 8083 app
