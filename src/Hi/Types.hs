{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Option(..)
    , Files
    ) where

type Files = [(FilePath, String)]

data Option = Option
             { initializeGitRepository :: Bool
             , moduleName :: String
             , packageName :: String
             , author :: String
             , email :: String
             , year :: String
             , repository :: String
             } deriving (Eq,Ord,Show)
