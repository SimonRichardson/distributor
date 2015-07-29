{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Models.Events

import Service.Service

min :: IO ()
main = runService $ do
  opt "/" $ "GET"
  get "/" $ do
    events <- runDB $ DB.SelectList [] []
    json $ (extract <$> events) :: [Event]

  where
    extract (Entity _ s) = s