module Main where

import Web.Scotty as W

import Model as M

import Network.HTTP.Types.Status (notFound404)
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders

import           DatabaseConn
import           Database.Persist hiding (get)
import qualified Database.Persist as DB

import Data.Maybe (fromMaybe, isJust)
import Data.List (elemIndex, mapAccumR, transpose)
import Data.Text.Lazy (fromStrict, toStrict)

import System.Environment (getEnvironment)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
  conn <- getEnvDef "MONGOLAB_URI" devMongo >>= return . parseDatabaseUrl
  port <- fmap read $ getEnvDef "PORT" "8000"

  let conf = mongoConfFrom conn
  pool <- createPoolConfig conf
  scotty port (app conf pool)

  where
    devMongo = "mongodb://127.0.0.1:27017/events"
    getEnvDef e d = getEnvironment >>= return . fromMaybe d . lookup e

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin",  "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type")    
  ]

app conf pool = do
  middleware allowCors
  opt "/" $ "GET"
  get "/" $ do
    W.text "FUCK"

  where
    opt route opts  = addroute OPTIONS route $ do
      setHeader "Access-Control-Allow-Methods" opts