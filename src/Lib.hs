{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- module Lib
--     ( startApp
--     , app
--     ) where
module Lib where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple hiding ((:.))
import Network.Wai.Handler.Warp ( run )
import Servant 
import Servant.Client
import Data.Pool
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E


type DBConnectionString = BS.ByteString
type Phrase = String
type Username = T.Text
type Password = T.Text
data User = User
  { user :: Username
  , pass :: Password
  } deriving (Eq, Show)

type API = 
      BasicAuth "People's Phrases" User :>
    ( ReqBody '[PlainText] Phrase :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Phrase]
    )

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn "create table if not exists phrases (phrase text not null)"
  execute_ conn "create table if not exists users (username text not null, password text not null)"
  return ()

server :: Pool Connection -> Server API
server conns user = postPhrase :<|> getPhrase 
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

checkBasicAuth :: Pool Connection -> BasicAuthCheck User
checkBasicAuth conns = BasicAuthCheck $ \basicAuthData ->
  let username = E.decodeUtf8 (basicAuthUsername basicAuthData)
      password = E.decodeUtf8 (basicAuthPassword basicAuthData)
  in
    liftIO $ withResource conns $ \conn ->
    query conn
      "SELECT * FROM users WHERE username = ?"
      (username, password) >>= \case
        [] -> return NoSuchUser
        [(username, pass)] -> if pass == password then return (Authorized (User username password)) else return BadPassword

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serveWithContext api ctx (server conns))
  where ctx = (checkBasicAuth conns :. EmptyContext)

initConnectionPool :: ByteString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

