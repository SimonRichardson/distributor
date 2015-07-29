{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.Events where

import qualified Database.Persist as DB

import Data.Aeson (ToJSON, toJSON, (.=), object)

import Database.Persist.MongoDB hiding (master)
import Database.Persist.TH

import Language.Haskell.TH.Syntax

import Service.Service

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Event sql=events
    ident       ObjectId sql=_id
    name        String
    description String
    deriving    Show 
|]

instance ToJSON Event where
  toJSON (Event ident name description) = 
      object [ "_id"         .= show ident
             , "name"        .= name
             , "description" .= description
             ]

