{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib.Config
import Lib.Server

main :: IO ()
main = do
  putStrLn "Loading configuration..."
  settings <- loadSettings
  putStrLn $ "Configuration loaded successfully"
  putStrLn $ "Port: " ++ show (settingsPort settings)
  putStrLn $ "Environment: " ++ show (settingsEnvironment settings)
  runServer settings
