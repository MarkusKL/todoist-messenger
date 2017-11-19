{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as W
import Network.HTTP.Types.Status (status200)
import Data.Aeson ((.:))
import qualified Data.Aeson as A


main :: IO ()
main = do
  putStrLn "Starting server..."
  Warp.runEnv 8000 app


app :: W.Application
app req res = do
  body <- W.strictRequestBody req
  let mess = (A.decode body) :: Maybe Message
  print mess
  res $ W.responseLBS status200 [] mempty


data Message = Message
  { sender :: String
  , text :: String
  } deriving (Show)


instance A.FromJSON Message where
  parseJSON = A.withObject "Payload" $ \o -> do
    s <- o .: "sender" >>= A.withObject "Sender" (.: "id")
    t <- o .: "message" >>= A.withObject "Message" (.: "text")
    return $ Message s t

