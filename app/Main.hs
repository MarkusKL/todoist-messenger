{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as W
import Network.HTTP.Types.Status (status200)
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Network.HTTP.Simple (httpLBS, setRequestBodyURLEncoded, parseRequest, Response)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.String (fromString)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


data Env = Env { host :: Warp.HostPreference, port :: Int, token :: Token }

main :: IO ()
main = do
  putStrLn "Starting server..."
  menv <- getEnv
  case menv of
    Just env -> run env
    Nothing -> putStrLn "Please set the HOST, PORT and TOKEN environment variables properly"


getEnv :: IO (Maybe Env)
getEnv = do
  mtoken <- lookupEnv "TOKEN"
  host <- fromString . fromMaybe "127.0.0.1" <$> lookupEnv "HOST"
  port' <- lookupEnv "PORT"
  let port = fromMaybe 8000 $ port' >>= readMaybe
  return $ Env host port <$> (fromString <$> mtoken)


run :: Env -> IO ()
run = Warp.runSettings <$> settings <*> app


settings :: Env -> Warp.Settings
settings env = foldr (.) id
  [ Warp.setPort (port env)
  , Warp.setHost (host env)
  , Warp.setServerName ""
  , Warp.setOnOpen $ \_ -> do
      putStrLn $ "Listening on " ++ show (port env)
      return True
  ] Warp.defaultSettings


app :: Env -> W.Application
app env req res = do
  body <- W.strictRequestBody req
  let mess = (A.decode body) :: Maybe Message
  print mess
  case mess of
    Just (Message sender text) -> do
      quickAdd (token env) text
      return ()
    Nothing ->
      return ()
  res $ W.responseLBS status200 [] mempty


quickAdd :: Token -> ByteString -> IO (Response L.ByteString)
quickAdd token text = do
  req <- parseRequest "https://todoist.com/api/v7/quick/add"
  let req2 = setRequestBodyURLEncoded [("token", token), ("text", text)] req
  httpLBS req2


data Message = Message
  { sender :: ByteString
  , text :: ByteString
  } deriving (Show)


type Token = ByteString


instance A.FromJSON ByteString where
  parseJSON v = fromString <$> A.parseJSON v

instance A.FromJSON Message where
  parseJSON = A.withObject "Payload" $ \o -> do
    s <- o .: "sender" >>= A.withObject "Sender" (.: "id")
    t <- o .: "message" >>= A.withObject "Message" (.: "text")
    return $ Message s t

