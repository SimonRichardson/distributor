{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Web.Scotty as W

import Model as M

import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders

import           DatabaseConn
import           Database.Persist hiding (get)
import qualified Database.Persist as DB

import Data.Aeson
import Data.Maybe (fromMaybe)

import Debug.Trace

import System.Environment (getEnvironment)

import Control.Monad.Reader

main :: IO ()
main = do
  conn <- getEnvDef "MONGOLAB_URI" devMongo >>= return . parseDatabaseUrl
  port <- fmap read $ getEnvDef "PORT" "8000"

  let conf = mongoConfFrom x

  pool <- createPoolConfig conf
  scotty port (app conf pool)

  where
    devMongo = "mongodb://127.0.0.1:27017/dice"
    getEnvDef e d = getEnvironment >>= return . fromMaybe d . lookup e

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin",  "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type")    
  ]

contentType :: Middleware
contentType = addHeaders [
    ("Content-Type", "text/json")
  ]

app conf pool = do
  middleware allowCors
  middleware contentType
  opt "/" $ "GET"
  get "/" $ do
    events <- getEvents
    let x = (trace "here") events
    W.json $ toOutput <$> (if null x then [] else x)

  where
    runDB action          = liftIO $ runPool conf action pool
    getEvents             = runDB $ DB.selectList [] []
    -- move to model.hs
    toOutput (Entity _ s) = object [ "name"        .= eventName s
                                   , "description" .= eventDescription s
                                   ]
    
    opt route opts  = addroute OPTIONS route $ do
      setHeader "Access-Control-Allow-Methods" opts
