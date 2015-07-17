{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import           Api.Services.EventService
import           Control.Lens
import           Snap.Core
import           Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data Api = Api { _eventService :: Snaplet EventService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("/", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = do
    modifyResponse . setResponseCode $ 204

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
    service <- nestSnaplet "events" eventService eventServiceInit
    addRoutes apiRoutes
    return $ Api service
