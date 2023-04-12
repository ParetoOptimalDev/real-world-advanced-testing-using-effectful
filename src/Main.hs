{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.TH
import Prelude hiding (evalState, gets, getEnv)
import qualified Data.ByteString as LBS
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Data.Aeson as Aeson
import Effectful.Environment (Environment, getEnv)

newtype PromptMessage = PromptMessage String deriving newtype (Semigroup)

newtype UserResponse = UserResponse String deriving newtype (Semigroup)

data Cli :: Effect where
  PromptUser :: PromptMessage -> Cli m UserResponse
makeEffect ''Cli

promptProgram :: (Cli :> es) => Eff es UserResponse
promptProgram = do
  username <- promptUser (PromptMessage "Username: ")
  password <- promptUser (PromptMessage "Password: ")
  pure $ username <> password

runCliPureConst :: UserResponse -> Eff (Cli : es) a -> Eff es a
runCliPureConst userResponse = interpret $ \_ -> \case
  PromptUser _msg -> pure userResponse

-- runEnvPureFailedLookup :: Eff (Environment : es) a -> Eff es a
-- runEnvPureFailedLookup = interpret $ \_ -> \case
--   MyGetEnv _name -> pure Nothing

data Http :: Effect where
  HttpPost :: HTTP.Request -> Http m (HTTP.Response LBS.ByteString)
makeEffect ''Http

newtype Playlist = Playlist String deriving stock (Eq, Ord, Show)

data Spotify :: Effect where
  SearchPlaylists :: String -> Spotify m [Playlist]
  
makeEffect ''Spotify

newtype MockEnv = MockEnv
  {mockPlaylists :: [Playlist]}
  deriving newtype (Eq, Ord, Show)

newtype SpotifyError = SpotifyError String deriving newtype (Show)

-- searchPlaylistsProgram :: forall {es :: [Effect]}.
--   (Error SpotifyError :> es, Env :> es, Spotify :> es) =>
--   String -> Eff es [Playlist]
-- searchPlaylistsProgram :: forall {es :: [Effect]}. (Error SpotifyError :> es, Env :> es, Spotify :> es) => String -> Eff es [Playlist]
-- searchPlaylistsProgram
--   :: '[Error SpotifyError, Env, Spotify] :>> es
--   => String
--   -> Eff es [Playlist]
-- searchPlaylistsProgram query = do
--   token <- getEnv "spotify_token"
--   case token of
--     Just _token -> do
--       searchPlaylists query
--     Nothing -> throwError $ SpotifyError "token not set"

searchPlaylistPureFunction :: String -> [Playlist] -> [Playlist]
searchPlaylistPureFunction query playlists = do
  filter (\p -> query `isInfixOf` playlistToString p) playlists

playlistToString :: Playlist -> String
playlistToString (Playlist s) = s


-- httpJSON "POST http://httpbin.org/post"
-- setRequestBodyLBS "This is my request body"
authedSpotifyGetProgram :: '[Error SpotifyError, Environment, Http] :>> es => Aeson.Object -> Eff es (HTTP.Response LBS.ByteString)
authedSpotifyGetProgram requestObject = do
  token <- getEnv "spotify_token" -- TODO use lookupEnv? catch error from this?
  let myRequest = HTTP.defaultRequest
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS $ Aeson.encode requestObject
        , HTTP.requestHeaders =
            [ ("Content-Type", "application/json; charset=utf-8")
            , ("Authorization", ("Bearer " <>  encodeUtf8 token))
            ]
        }
  -- well this can't work because <- is specialized to return type of HTTP.Response LBS.ByteString
  httpPost myRequest 


runSpotifyPureConst :: '[Error SpotifyError] :>> es => MockEnv -> Eff (Spotify : es) a -> Eff es a
runSpotifyPureConst mock0 = interpret $ \_ -> \case
  SearchPlaylists query -> do
    
    let playlists = mockPlaylists mock0
    pure $ searchPlaylistPureFunction query playlists

-- getSpotifyToken

-- searchPlaylistsProgram
--   :: '[Error SpotifyError, Env, Spotify] :>> es
--   => String
--   -> Eff es [Playlist]
-- searchPlaylistsProgram query = do
--   token <- myGetEnv "spotify_token"
--   case token of
--     Nothing -> throwError $ SpotifyError "token not set"
--     Just _token -> searchPlaylists query
    
main :: IO ()
main = do
  putStrLn "done"
  
  searchPlaylists "foo" & runSpotifyPureConst (MockEnv playlists) & runEnvPureFailedLookup & runErrorNoCallStack @SpotifyError & runPureEff & print
    where
      playlists = Playlist <$> ["something", "foofighters", "somethingelse", "food playlist"]


  -- TODO I want to throw a wrench in things here by making the SearchPlaylists function throw an error if there is no auth token
  -- then after that I want to add logic to handle that auth error and prompt the user to login
