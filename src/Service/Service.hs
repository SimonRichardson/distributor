{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Service.Service where

data Config = Config { environment :: Environment
                     , pool        :: DB.ConnectionPool
                     }

data Environment = Development
                 | Production
                 | Test
                 deriving (Show, Eq, Read)

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
                  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

type Action = ActionT Error ConfigM ()

type Handler = ScottyT Error ConfigM ()

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin",  "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type")    
  ]

contentType :: Middleware
contentType = addHeaders [
    ("Content-Type", "text/json")
  ]

opt route opts  = addroute OPTIONS route $ do
  setHeader "Access-Control-Allow-Methods" opts

getConfig :: IO Config
getConfig = do
    environment <- getEnvironment
    pool        <- getPool environment
    return Config{..}

getPort :: IO (Maybe Int)
getPort = fmap read <$> lookupEnv "PORT"

getConnectionString :: Environment -> IO DatabaseConnInfo
getConnectionString e = maybe defConnStr mkConnStr <$> lookupEnv "DATABASE_URL"
  where
    defConnStr = getDefaultConnectionString e
    mkConnStr = createConnectionString . parseDatabaseUrl

getDefaultConnectionString :: Environment -> String
getDefaultConnectionString _ = "mongodb://127.0.0.1:27017/events"

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
    s <- getConnectionString 
    let conf = mongoConfFrom s
    createPoolConfig conf

getSettings :: Environment -> IO Settings
getSettings e = do
    let cache = if e == Development then setFdCacheDuration 0 else id
    portSetter <- maybe id setPort <$> getPort
    return $ portSetter $ cache defaultSettings

getOptions :: Environment -> IO Options
getOptions e = do
    s <- getSettings e
    return def { settings = s
               , verbose = case e of Development -> 1
                                     _           -> 0
               }

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM Test        = id

defaultH :: Environment -> Error -> Action
defaultH e x = do
    status internalServerError500
    let o = case e of
                Production -> Null
                _          -> object ["error" .= showError x]
    json o

application :: ScottyT Error ConfigM () -> ScottyT Error ConfigM ()
application app = do
  e <- lift (asks environment)
  middleware (loggingM e)
  defaultHandler (defaultH e)
  app

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
    p <- lift (asks pool)
    liftIO (runPool q p)

runApplication :: Config -> ScottyT Error ConfigM () -> IO ()
runApplication c app = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r r (application app)

runService :: ScottyT Error ConfigM () -> IO ()
runService app = do
  c <- getConfig
  runApplication c app
