{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Authentication where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp
import Servant (throwError)
import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, Get, JSON)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server
import Servant.Server.Experimental.Auth
import qualified Data.Map as Map

-- | Private data that needs protection.
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | Public data that anyone can use.

newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone

newtype User = User { userName :: Text }
  deriving (Eq, Show)

-- | A type to wrap our public API.

type PublicAPI = Get '[JSON] [PublicData]

-- | A type to wrap our private API.

type PrivateAPI = Get '[JSON] PrivateData

-- | Our API.

type BasicAPI = "public"  :> PublicAPI
           :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

-- | A value holding a proxy of our API type.

basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and
-- password.

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) = return $
        if username == "servant" && password == "server"
          then Authorized (User "servant")
          else Unauthorized
  in BasicAuthCheck check

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck'
-- value tagged with "foo-tag". This context is then supplied to 'server'
-- and threaded to the BasicAuth HasServer handlers.

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

-- | An implementation of our server. Here is where we pass all the handlers
-- to our endpoints. In particular, for the BasicAuth protected handler, we
-- need to supply a function that takes 'User' as an argument.

basicAuthServer :: Server BasicAPI
basicAuthServer =
  let publicAPIHandler = return [PublicData "foo", PublicData "bar"]
      privateAPIHandler (user :: User) = return (PrivateData (userName user))
  in publicAPIHandler :<|> privateAPIHandler

basicAuthMain :: IO ()
basicAuthMain = run 8080
  (serveWithContext basicAuthApi basicAuthServerContext basicAuthServer)

----------------------------------------------------------------------------
-- Custom authentication

-- | A user type that we “fetch from the database” after performing
-- authentication.

newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to users.

database :: Map ByteString Account
database = fromList
  [ ("key1", Account "Anne Briggs")
  , ("key2", Account "Bruce Cockburn")
  , ("key3", Account "Ghédalia Tazartès") ]

-- | A method that, when given a password, will return an Account. This is
-- our bespoke (and bad) authentication logic.

lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing  -> throwError (err403 { errBody = "Invalid Cookie" })
  Just usr -> return usr

-- | The auth handler wraps a function from Request -> Handler Account we
-- look for a Cookie and pass the value of the cookie to lookupAccount.

authHandler :: AuthHandler Request Account
authHandler =
  let handler req = case lookup "servant-auth-cookie" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just authCookieKey -> lookupAccount authCookieKey
  in mkAuthHandler handler

-- | Our API, with auth-protection.

type AuthGenAPI =
       "private" :> AuthProtect "cookie-auth" :> PrivateAPI
  :<|> "public"  :> PublicAPI

-- | A value holding our type-level API.

genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | We need to specify the data returned after authentication.

type instance AuthServerData (AuthProtect "cookie-auth") = Account

-- | The context that will be made available to request handlers. We supply
-- the "cookie-auth"-tagged request handler defined above, so that the
-- 'HasServer' instance of 'AuthProtect' can extract the handler and run it
-- on the request.

genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as
-- an argument. We don't worry about the authentication instrumentation
-- here, that is taken care of by supplying context.

genAuthServer :: Server AuthGenAPI
genAuthServer =
  let privateDataFunc (Account name) =
          return (PrivateData ("this is a secret: " <> name))
      publicData = return [PublicData "this is a public piece of data"]
  in  privateDataFunc :<|> publicData

-- | Run our server.

genAuthMain :: IO ()
genAuthMain =
  run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)
