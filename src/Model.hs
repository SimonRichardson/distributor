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
{-# LANGUAGE DeriveGeneric              #-}

module Model where

import Data.Aeson (ToJSON)

import Database.Persist.MongoDB hiding (master)
import Database.Persist.TH

import GHC.Generics

import Language.Haskell.TH.Syntax

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) { mpsGeneric = False }
 in share [mkPersist mongoSettings] [persistLowerCase|
Event
    name String
    description String
    deriving Show Generic
|]

instance ToJSON Event
