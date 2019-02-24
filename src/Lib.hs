module Lib where

import GHC.Generics
import Aws.Lambda.Runtime
import qualified Data.Aeson as Aeson

newtype QueryStrings = QueryStrings { name :: String } deriving (Generic, Show)

data Response = Response
  { statusCode :: Int
  , body       :: String
  } deriving (Generic)

instance Aeson.FromJSON QueryStrings
instance Aeson.ToJSON QueryStrings
instance Aeson.FromJSON Response
instance Aeson.ToJSON Response

getQueryStrings :: Aeson.Value -> Maybe QueryStrings
getQueryStrings = Aeson.decode . Aeson.encode

data WebPage = WebPage 
  { head       :: String
  , bodyInLine :: String
  , last       :: String
   } deriving (Show)

class Html a where
  showHtml      :: a -> String
  getHead       :: a -> String
  getBodyInLine :: a -> String
  getLast       :: a -> String

instance Html WebPage where
  showHtml      (WebPage a b c) = a ++ b ++ c
  getHead       (WebPage a _ _) = a
  getBodyInLine (WebPage _ b _) = b
  getLast       (WebPage _ _ c) = c

constructWebPage :: QueryStrings -> WebPage
constructWebPage qs = WebPage h b l
  where
    h = mconcat 
      [ "<head>"
      , "<link rel=\"stylesheet\" href=\"https://s3-eu-west-1.amazonaws.com/lambda-pages-css/lambda-pages-css.css\" >"
      , "</head>"
      ]
    b = mconcat 
      [ "<h1>Hello there " 
      , name qs
      , "!</h1>"
      ]
    l = ""

handler :: Aeson.Value -> Context -> IO (Either String String)
handler event _ = do
  let person  = getQueryStrings event
  case person of 
    Just p  -> do
      let webpage = constructWebPage p
      return $ Right $ showHtml webpage
    Nothing -> return $ Left "Please provide only a name query string"

