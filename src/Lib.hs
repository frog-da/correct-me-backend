{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- module Lib
--     ( startApp
--     , app
--     ) where
module Lib where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp ( run )
import Servant 
import Servant.Client
import Data.Pool
import Data.ByteString (ByteString)

type DBConnectionString = BS.ByteString
type Phrase = String

type API = ReqBody '[PlainText] Phrase :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Phrase]

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn "create table if not exists phrases (phrase text not null)"
  return ()

server :: Pool Connection -> Server API
server conns = postPhrase :<|> getPhrase

  where postPhrase :: Phrase -> Handler NoContent
        postPhrase phr = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO phrases VALUES (?)"
                    (Only phr)
          return NoContent

        getPhrase :: Handler [Phrase]
        getPhrase = fmap (map fromOnly) . liftIO $
         withResource conns $ \conn ->
            query_ conn "SELECT phrase FROM phrases"

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

initConnectionPool :: ByteString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

postPhr :: Phrase -> ClientM NoContent
getPhrs :: ClientM [Phrase]
postPhr :<|> getPhrs = client api
