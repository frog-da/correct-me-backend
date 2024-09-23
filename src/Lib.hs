{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- module Lib
--     ( startApp
--     , app
--     ) where
module Lib where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple hiding ((:.))
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client

type DBConnectionString = BS.ByteString

type Username = T.Text

type Password = T.Text

data Phrase = Phrase
  { phraseId :: Int,
    phraseUsername :: Username,
    phraseApproved :: Bool,
    phraseAlternatives :: [AlternativePhrase]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Phrase

data AlternativePhrase = AlternativePhrase
  { alternativePhraseId :: Int,
    alternativePhraseParentId :: Int,
    alternativePhraseText :: T.Text,
    alternativePhraseApproved :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON AlternativePhrase

data User = User
  { userUsername :: Username,
    userPassword :: Password
  }
  deriving (Eq, Show)

type API =
  BasicAuth "People's Phrases" User
    :> ( "phrases" :> ReqBody '[JSON] T.Text :> Post '[JSON] Phrase
           :<|> "phrases" :> Get '[JSON] [Phrase]
           :<|> "phrases" :> "my" :> Get '[JSON] [Phrase]
           :<|> "phrases" :> "unapproved" :> Get '[JSON] [Phrase]
           :<|> "phrases" :> "my" :> "alternatives" :> Capture "phraseId" Int :> Put '[JSON] Phrase
           :<|> "phrases" :> Capture "phraseId" Int :> "alternatives" :> ReqBody '[JSON] T.Text :> Post '[JSON] AlternativePhrase
       )

api :: Proxy API
api = Proxy

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn "CREATE TABLE if not exists users (username TEXT PRIMARY KEY, password TEXT NOT NULL)"
  execute_ conn "CREATE TABLE if not exists phrases (id SERIAL PRIMARY KEY, username TEXT NOT NULL, approved BOOLEAN DEFAULT FALSE, FOREIGN KEY (username) REFERENCES users (username))"
  execute_ conn "CREATE TABLE if not exists alternative_phrases (id SERIAL PRIMARY KEY, phrase_id INTEGER NOT NULL, alternative_phrase TEXT NOT NULL, approved BOOLEAN DEFAULT FALSE, FOREIGN KEY (phrase_id) REFERENCES phrases (id))"
  return ()

server :: Pool Connection -> Server API
server conns (User username _) = postPhrase :<|> getPhrases :<|> getMyPhrases :<|> getUnapprovedPhrases :<|> approvePhrase :<|> postAlternativePhrase
  where
    postPhrase :: T.Text -> Handler Phrase
    postPhrase phr = do
      [(phraseID, username, approved)] <- liftIO . withResource conns $ \conn ->
        query conn "INSERT INTO phrases (username) VALUES (?) returning id, username, approved" (Only username) :: IO [(Int, Username, Bool)]
      [(id, phraseID, alternativePhrase, apApproved)] <- liftIO . withResource conns $ \conn ->
        query conn "INSERT INTO alternative_phrases (phrase_id, alternative_phrase) VALUES (?, ?) returning id, phrase_id, alternative_phrase, approved" (phraseID, phr)
      pure (Phrase phraseID username approved [AlternativePhrase id phraseID alternativePhrase apApproved])

    getPhrases :: Handler [Phrase]
    getPhrases = liftIO $
      withResource conns $ \conn -> do
        phrases <- query_ conn "SELECT id, username, approved FROM phrases"
        alternativePhrases <- query_ conn "SELECT id, phrase_id, alternative_phrase, approved FROM alternative_phrases"
        pure $
          map
            ( \(phraseId, username, approved) ->
                Phrase phraseId username approved
                  $ map
                    ( \(alternativePhraseId, phraseId', alternativePhrase, approved') ->
                        AlternativePhrase alternativePhraseId phraseId' alternativePhrase approved'
                    )
                  $ filter (\(alternativePhraseId, phraseId', _, _) -> phraseId == phraseId') alternativePhrases
            )
            phrases

    getMyPhrases :: Handler [Phrase]
    getMyPhrases = liftIO $ withResource conns $ \conn -> do
      phrases <- query conn "SELECT id, username, approved FROM phrases WHERE username = ?" (Only username)
      alternativePhrases <- query conn "SELECT id, phrase_id, alternative_phrase, approved FROM alternative_phrases WHERE phrase_id IN ?" (Only (In (map (\(phraseId, _, _) -> phraseId) phrases)))
      pure $
        map
          ( \(phraseId, username, approved) ->
              Phrase phraseId username approved
                $ map
                  ( \(alternativePhraseId, phraseId', alternativePhrase, approved') ->
                      AlternativePhrase alternativePhraseId phraseId' alternativePhrase approved'
                  )
                $ filter (\(alternativePhraseId, phraseId', _, _) -> phraseId == phraseId') alternativePhrases
          )
          phrases

    getUnapprovedPhrases :: Handler [Phrase]
    getUnapprovedPhrases = liftIO $ withResource conns $ \conn -> do
      phrases <- query_ conn "SELECT id, username, approved FROM phrases WHERE approved = FALSE"
      alternativePhrases <- query_ conn "SELECT id, phrase_id, alternative_phrase, approved FROM alternative_phrases"
      pure $
        map
          ( \(phraseId, username, approved) ->
              Phrase phraseId username approved
                $ map
                  ( \(alternativePhraseId, phraseId', alternativePhrase, approved') ->
                      AlternativePhrase alternativePhraseId phraseId' alternativePhrase approved'
                  )
                $ filter (\(alternativePhraseId, phraseId', _, _) -> phraseId == phraseId') alternativePhrases
          )
          phrases

    approvePhrase :: Int -> Handler Phrase
    approvePhrase alternativePhraseId = do
      maybePhraseId <- liftIO . withResource conns $ \conn -> do
        query conn "UPDATE alternative_phrases ap SET approved = true FROM phrases p WHERE ap.phrase_id = p.id AND p.username = ? AND ap.id = ? returning p.id" (username, alternativePhraseId) :: IO [Only Int]

      phraseId <- case maybePhraseId of
        [Only phraseId'] -> pure phraseId'
        _ -> throwError err404

      liftIO . withResource conns $ \conn -> execute conn "UPDATE phrases SET approved = true WHERE id = ?" (Only phraseId)

      liftIO . withResource conns $ \conn -> do
        [(phraseId', username, approved)] <- query conn "SELECT id, username, approved FROM phrases WHERE id = ?" (Only phraseId)
        alternativePhrases <- query conn "SELECT id, phrase_id, alternative_phrase, approved FROM alternative_phrases WHERE phrase_id = ?" (Only phraseId)
        pure
          ( Phrase
              phraseId'
              username
              approved
              ( map
                  ( \(alternativePhraseId, phraseId'', alternativePhrase, approved') ->
                      AlternativePhrase alternativePhraseId phraseId'' alternativePhrase approved'
                  )
                  alternativePhrases
              )
          )

    postAlternativePhrase :: Int -> T.Text -> Handler AlternativePhrase
    postAlternativePhrase phraseId alternativePhrase = do
      [(id, phraseId, alternativePhrase, approved)] <- liftIO . withResource conns $ \conn ->
        query conn "INSERT INTO alternative_phrases (phrase_id, alternative_phrase) VALUES (?, ?) returning id, phrase_id, alternative_phrase, approved" (phraseId, alternativePhrase)
      pure (AlternativePhrase id phraseId alternativePhrase approved)

checkBasicAuth :: Pool Connection -> BasicAuthCheck User
checkBasicAuth conns = BasicAuthCheck $ \basicAuthData ->
  let username = E.decodeUtf8 (basicAuthUsername basicAuthData)
      password = E.decodeUtf8 (basicAuthPassword basicAuthData)
   in liftIO $ withResource conns $ \conn ->
        query
          conn
          "SELECT * FROM users WHERE username = ?"
          (Only username)
          >>= \case
            [] -> return NoSuchUser
            [(username, pass)] -> if pass == password then return (Authorized (User username password)) else return BadPassword

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serveWithContext api ctx (server conns))
  where
    ctx = checkBasicAuth conns :. EmptyContext

initConnectionPool :: ByteString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe
