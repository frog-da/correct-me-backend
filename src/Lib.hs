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
type Phrase = T.Text
type Username = T.Text
type Password = T.Text
data User = User
  { user :: Username
  , pass :: Password
  } deriving (Eq, Show)

type API =
      BasicAuth "People's Phrases" User :>
    ( ReqBody '[JSON] Phrase :> Post '[JSON] Bool
      :<|> "phrases" :> Get '[JSON]  [(Username, Phrase)]
      :<|> "phrases" :> "my" :> Get '[JSON]  [(Username, Phrase)]
      :<|> "phrases" :> "unapproved" :> Get '[JSON]  [(Username, Phrase)]
      -- :<|> "phrases" :> "my" :> QueryParam "approved" Bool :> Put '[JSON] Bool
    )

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn "CREATE TABLE if not exists users (username TEXT PRIMARY KEY, password TEXT NOT NULL)"
  execute_ conn "CREATE TABLE if not exists phrases (phrase TEXT NOT NULL, username TEXT NOT NULL, approved BOOLEAN DEFAULT FALSE, FOREIGN KEY (username) REFERENCES users (username))"
  return ()

server :: Pool Connection -> Server API
server conns (User username _) = postPhrase :<|> getPhrases :<|> getMyPhrases :<|> getUnapprovedPhrases 
-- :<|> approvePhrase
  where postPhrase :: Phrase -> Handler Bool
        postPhrase phr = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO phrases VALUES (?, ?)"
                    (phr, username)
          pure True

        getPhrases :: Handler [(Username, Phrase)]
        getPhrases = liftIO $
         withResource conns $ \conn ->
            query_ conn "SELECT username, phrase FROM phrases"

        getMyPhrases :: Handler  [(Username, Phrase)]
        getMyPhrases = liftIO $ withResource conns $ \conn ->
          (query conn
                "SELECT username, phrase FROM phrases WHERE username = ?"
                (Only username) :: IO [(Username, Phrase)])  >>= \case
                  phrs -> pure phrs

        getUnapprovedPhrases :: Handler  [(Username, Phrase)]
        getUnapprovedPhrases = liftIO $ withResource conns $ \conn ->
          (query conn
                "SELECT username, phrase FROM phrases WHERE approved = FALSE"
                () :: IO [(Username, Phrase)])  >>= \case
                  phrs -> pure phrs

        -- approvePhrase :: Maybe Bool -> Handler Bool
        -- approvePhrase status = do
        --   case status of
        --     Just status' -> (do
        --       liftIO . withResource conns $ \conn ->
        --         execute conn
        --                 "UPDATE phrases SET approved = ? WHERE username = ?"
        --                 (status', username)
        --       pure True)
        --     _ -> pure False

checkBasicAuth :: Pool Connection -> BasicAuthCheck User
checkBasicAuth conns = BasicAuthCheck $ \basicAuthData ->
  let username = E.decodeUtf8 (basicAuthUsername basicAuthData)
      password = E.decodeUtf8 (basicAuthPassword basicAuthData)
  in
    liftIO $ withResource conns $ \conn ->
    query conn
      "SELECT * FROM users WHERE username = ?"
      (Only username) >>= \case
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

