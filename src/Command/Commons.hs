module Command.Commons where

import Data.Text
import Data.Monoid ((<>))
import Options.Applicative


data Command = 
      CommandList 
    | CommandNew { projectName :: Text }
    deriving (Show)


tpProgDesc :: String
tpProgDesc = "use sfroce to handle salesforce migration project " ++
                "when needed"


tpHeader :: String
tpHeader = "Teleport: move around your filesystem"
    
fileNameParser :: Parser Text
fileNameParser = argument  -- :: ReadM String -> Mod ArgumentFields String -> Parser String
    str -- :: ReadM String
    (metavar -- :: String -> Mod ArgumentFields String
    "PROJECT-NAME" <>
    help -- :: String -> Mod ArgumentFields String
    "name of the new sforce project") -- Mod ArgumentFields String