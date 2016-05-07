{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS -fno-warn-orphans #-}

module Docs where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Docs
import Servant.Server

import Server (Email(..), ClientInfo(..), Position(..), HelloMessage(..),
  server3, emailForClient)

type ExampleAPI =
       "position"  :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello"     :> QueryParam "name" String :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

instance ToCapture (Capture "x" Int) where
  toCapture _ =
    DocCapture "x" "(integer) position on the X axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ =
    DocCapture "y" "(integer) position on the Y axis"

instance ToSample Position where
  toSamples _ = singleSample (Position 3 14)

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name" ["Alp", "John Doe", "…"]
                  "Name of person to say hello to."
                  Normal

instance ToSample HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
    , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
    ]

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
  toSamples _ = singleSample ci

instance ToSample Email where
  toSamples _ = singleSample (emailForClient ci)

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] (Proxy :: Proxy ExampleAPI)
  where intro = DocIntro "Welcome"
          ["This is our super webservice's API.", "Enjoy!"]

type DocsAPI = ExampleAPI :<|> Raw

server :: Server DocsAPI
server = Server.server3 :<|> serveDocs
  where serveDocs _ respond =
          respond (responseLBS ok200 [plain] docsBS)
        plain = ("Content-Type", "text/plain")

startApp :: IO ()
startApp = run 8081 $ serve (Proxy :: Proxy DocsAPI) server
