{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.EventService where

import           Api.Types
import           Control.Lens
import           Data.Aeson
import           Snap.Core
import           Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data EventService = EventService { }

makeLenses ''EventService

eventRoutes :: [(B.ByteString, Handler b EventService ())]
eventRoutes = 
    [ ("/", method GET getEvents)
    ]

getEvents :: Handler b EventService ()
getEvents = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  let events = []
  writeLBS . encode $ (events :: [Event])

eventServiceInit :: SnapletInit b EventService
eventServiceInit = makeSnaplet "events" "Event Service" Nothing $ do
  addRoutes eventRoutes
  return $ EventService