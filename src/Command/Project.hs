{-# LANGUAGE OverloadedStrings #-}

module Command.Project where

import Data.Text (Text)
import Command.Commons
import Control.Applicative
import Options.Applicative
import Turtle.Prelude
import Filesystem.Path.CurrentOS

parseNewCommand :: Parser Command
parseNewCommand = 
    (liftA
        CommandNew  -- :: String -> FilePath -> Commandd
        fileNameParser -- :: Parser String
    )

folders :: [Text] 
folders = ["objects", "classes", "pages", "deployed"]

files :: [Text] 
files = ["package.yaml"]

createProject :: Text -> IO ()
createProject dirName = generateFolder folders >> generateFile files >> return ()
    where attachDir dir file  = fromText dir </> fromText file
          generateFolder f = traverse (mktree . attachDir dirName) f
          generateFile f   = traverse (touch . attachDir dirName) f
          