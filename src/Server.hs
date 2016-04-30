{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server
  ( startApp )
where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

data User = User
  { name :: String
  , age  :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users1

app1 :: Application
app1 = serve (Proxy :: Proxy UserAPI1) server1

type UserAPI2 = "users"  :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac"  :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac

app2 :: Application
app2 = serve (Proxy :: Proxy UserAPI2) server2

type API =
       "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server3 :: Server API
server3 = position
  :<|> hello
  :<|> marketing
  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello = return   .
          HelloMessage   .
          ("Hello, " ++) .
          fromMaybe "anonymous coward"

        marketing :: ClientInfo -> Handler Email
        marketing = return . emailForClient

app3 :: Application
app3 = serve (Proxy :: Proxy API) server3

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName  person)

  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

server4 :: Server PersonAPI
server4 = return people

app4 :: Application
app4 = serve (Proxy :: Proxy PersonAPI) server4

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String
  } deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = FileContent <$> liftIO (readFile "myfile.txt")

app5 :: Application
app5 = serve (Proxy :: Proxy IOAPI1) server5

server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then FileContent <$> liftIO (readFile "myfile.txt")
    else throwError custom404Err

  where custom404Err = err404 { errBody = "it's just not there…" }

app6 :: Application
app6 = serve (Proxy :: Proxy IOAPI1) server6

type StaticAPI = "static" :> Raw

server7 :: Server StaticAPI
server7 = serveDirectory "static-files"

app7 :: Application
app7 = serve (Proxy :: Proxy StaticAPI) server7

-- Using another monad for handlers

readerToHandler :: Reader String :~> Handler
readerToHandler = let f r = return (runReader r "hi") in Nat f

type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> Get '[JSON] String

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b
  where a :: Reader String Int
        a = return 1797

        b :: Reader String String
        b = ask

readerServer :: Server ReaderAPI
readerServer = enter readerToHandler readerServerT

readerApp :: Application
readerApp = serve (Proxy :: Proxy ReaderAPI) readerServer

startApp :: IO ()
startApp = run 8081 readerApp
