{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Command where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.IO.Class
import System.Environment
import System.Directory
import Control.Monad

data Options = Options 
    {optionsCommand :: Command 
    } deriving Show

data Command 
    = CheckEnv JavaAntChecker
    | CustomObject CustomObjectCommand
    deriving Show

data JavaAntChecker = JavaAntChecker Bool deriving Show

data CustomObjectCommand 
    = FileInput FilePath
    | DirInput FilePath
    deriving Show

optionalBool :: Parser Command
optionalBool = CheckEnv <$>
    (JavaAntChecker
        <$>  switch
            ( long "quiet"
            <> short 'q'
            <> help "Whether to be quiet" 
            )
    )

cObjectParser :: Parser Command
cObjectParser = CustomObject 
    <$> cObjectFileDirInput

cObjectFileDirInput :: Parser CustomObjectCommand
cObjectFileDirInput = cObjectFileInput <|> cObjectDirInput

cObjectFileInput :: Parser CustomObjectCommand
cObjectFileInput = FileInput
    <$> strOption (long "file" <> metavar "FILE")

cObjectDirInput :: Parser CustomObjectCommand
cObjectDirInput = DirInput
    <$> strOption (long "dir" <> metavar "DIR")

opts :: Parser Command
opts = subparser
    ( command "checkenv" (info (optionalBool <**> helper) ( progDesc "Check Java and Ant environment variable" ))
    <> command "new-object" (info (cObjectParser <**> helper) ( progDesc "Create new custom object file" ))
    )

-- printFileName :: FileInput -> IO ()
-- prinfFileName (F 
mainCommand :: IO ()
mainCommand = entrypoint =<< execParser op
    where op = info (opts <**> helper)
            ( fullDesc
            <> progDesc "[--help] COMMAND"
            <> header "sforce-migration - Haskell Salesforce Migration CLI" )

main :: IO ()
main = mainCommand

validateJavaAntEnv :: (String, Maybe String) -> String
validateJavaAntEnv (env, Nothing) = env ++ " environment variable not set."
validateJavaAntEnv (env, (Just path)) = env ++ " set to path: " ++ path

entrypoint :: Command -> IO ()
entrypoint (CustomObject command) = entrypointCommand command
entrypoint (CheckEnv _) = do
    list <- executeJavaAntCheck
    traverse (putStrLn . validateJavaAntEnv) $ list
    return ()

entrypointCommand :: CustomObjectCommand -> IO ()
entrypointCommand (FileInput finput) = doesFileExist finput >>= putStrLn . validateDirFileExist "FILE" >> return ()
entrypointCommand (DirInput dinput)  = doesDirectoryExist dinput >>= putStrLn . validateDirFileExist "DIR" >> return ()

validateDirFileExist :: String -> Bool -> String
validateDirFileExist type_ True = type_ ++ " exxist!"
validateDirFileExist type_ False = type_ ++ " does not exxist!"

executeJavaAntCheck :: IO [(String, Maybe String)]
executeJavaAntCheck = sequence [lookupRequiredEnv "ANT_HOME", lookupRequiredEnv "JAVA_HOME"]

lookupRequiredEnv :: String -> IO (String, Maybe String)
lookupRequiredEnv env = fmap ((,) env) $ lookupEnv env 
