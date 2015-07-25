{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DatabaseConn where

import           Data.Text (Text, pack, unpack, breakOn)
import qualified Data.Text as T

import Database.Persist.MongoDB

import Control.Monad.Trans (MonadIO)

import Network (PortID (PortNumber))
import Network.URI


type DatabaseConnInfo = [(Text, Text)]

parseDatabaseUrl :: String -> DatabaseConnInfo
parseDatabaseUrl durl =
    let muri = parseAbsoluteURI durl
        (auth, dbpath) = case muri of
          Nothing ->  error "couldn't parse absolute uri"
          Just uri -> if uriScheme uri /= "mongodb:"
            then schemeError uri
            else case uriAuthority uri of
              Nothing -> invalid
              Just a  -> (a, uriPath uri)
        (user,password) = userAndPassword auth
     in
      [ (pack "user",     user)
      , (pack "password", T.tail password)
      , (pack "host",     pack $ uriRegName auth)
      , (pack "port",     pack $ tail $ uriPort auth)
      , (pack "dbname",   pack $ tail $ dbpath)
      ]
  where
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . init . uriUserInfo
    schemeError uri = error $ "was expecting a mongodb scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    invalid = error "could not parse heroku MONGOLAB_URI"

mongoConfFrom :: DatabaseConnInfo -> MongoConf
mongoConfFrom params = MongoConf {
    mgDatabase = getParam "dbname",
    mgHost = getParam "host",
    mgPort = PortNumber $ fromIntegral . readInt . unpack $ getParam "port",
    mgAuth = Just $ MongoAuth (getParam "user") (getParam "password"),
    mgAccessMode = master,
    mgPoolStripes = 10,
    mgStripeConnections = 1,
    mgConnectionIdleTime = 1,
    mgReplicaSetConfig = Nothing
  } 
  where 
    getParam n = case lookup n params of
      Just v  -> v
      Nothing -> error $
        "Could not find parameter "
        ++ (show . unpack) n
        ++ " in database config URL."
    readInt = read :: String -> Int

withMongoDBConf :: (Applicative m, MonadIO m) => MongoConf -> (ConnectionPool -> m b) -> m b
withMongoDBConf c = withMongoDBPool
                      (mgDatabase c) (unpack $ mgHost c) (mgPort c)
                      (mgAuth c) (mgPoolStripes c) (mgStripeConnections c)
                      (mgConnectionIdleTime c)
