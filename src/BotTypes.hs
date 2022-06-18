{-# LANGUAGE DeriveGeneric #-}

module BotTypes ( 
    Config (..) ) where


import GHC.Generics ( 
    Generic )

import qualified Data.Aeson as JSON (
    FromJSON )


-- Type for yaml configs
data Config = Config {
    token :: String,
    sleep :: Int,
    host :: String,
    port :: Int
} deriving ( Show, Generic, Eq )
instance JSON.FromJSON Config
