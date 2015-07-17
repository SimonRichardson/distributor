{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T

data Event = Event
    { name          :: T.Text
    , description   :: T.Text
    }

instance ToJSON Event where
    toJSON (Event name description) = object 
        [ "name"        .= name
        , "description" .= description
        ]
