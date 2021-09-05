module Main where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Generic (encode)
import Krestia.Decomposition (DecomposedWord, decompose)
import Krestia.Utils (EncodableEither, Error)
import Node.Express.App (App, get, listenHttp, use)
import Node.Express.Middleware.Static (static)
import Node.Express.Response (sendJson)
import Node.HTTP (Server)

appSetup :: App
appSetup = do
   use (static "/public")
   get "/" (sendJson (encode (wrap (decompose "vilka") :: EncodableEither Error DecomposedWord)))

main :: Effect Server
main = do
   listenHttp appSetup 7000 (\_ -> log "Server started on port 7000")