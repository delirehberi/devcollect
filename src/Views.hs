{-#LANGUAGE OverloadedStrings#-}
module Views where

import Types
import           Web.Scotty
import Data.List (sort)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import Text.Mustache
import Text.Parsec.Error

parseTemplate :: FilePath -> [IDPData] -> IO TL.Text 
parseTemplate path xs = do 
    compiled <- automaticCompile ["./templates"] (path ++ ".mustache")
    case compiled of 
        Left e -> return 
                        $ TL.unlines
                        $ map TL.pack ["Can not parse template " ++ path ++ ".mustache", show e]
        Right t' -> return $ TL.fromStrict $ substitute t' (TemplateData $ sort xs)


render :: FilePath -> [IDPData] -> ActionM ()
render templateName xs = do
    s <- liftIO (parseTemplate templateName xs)
    html s

