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


main :: IO ()
main = do
  putStrLn "Starting server..."
  token <- lookupEnv "TOKEN"
  case token of
    Just t -> Warp.runEnv 8000 (app $ fromString t)
    Nothing -> putStrLn "Please set the TOKEN environment variable"


app :: Token -> W.Application
app token req res = do
  body <- W.strictRequestBody req
  let mess = (A.decode body) :: Maybe Message
  print mess
  case mess of
    Just (Message sender text) -> do
      quickAdd token text
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

