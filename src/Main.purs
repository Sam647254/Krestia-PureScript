module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Generic (encode)
import Krestia.API (findWord)
import Krestia.Decomposition (DecomposedWord, decompose)
import Krestia.Dictionary (DictionaryIndex, testLoadDictionary)
import Krestia.Utils (EncodableEither, Error)
import Node.Express.App (App, get, listenHttp, use)
import Node.Express.Handler (Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send, sendJson, setStatus)
import Node.HTTP (Server)

todo :: Handler
todo = do
   setStatus 501
   send unit

wordHandler :: DictionaryIndex -> Handler
wordHandler index = do
   query <- getRouteParam "word"
   case query of
      Nothing -> do
         setStatus 400
         send {error: "No query given"}
      Just q -> do
         let word = findWord index q
         case word of
            Nothing -> do
               setStatus 404
               send {error: "Word " <> " not found"}
            Just w ->
               send (encode w)

appSetup :: DictionaryIndex -> App
appSetup index = do
   use (static "/public")
   get "/test" (sendJson (encode (wrap (decompose "vilka") :: EncodableEither Error DecomposedWord)))
   get "/api/word/:word" (wordHandler index)
   get "/api/search/:query" todo
   get "/api/alphabetical" todo
   get "/api/types" todo

main :: Effect Server
main = do
   dictionary <- testLoadDictionary
   listenHttp (appSetup dictionary) 7000 (\_ -> log "Server started on port 7000")