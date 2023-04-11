{-# LANGUAGE TemplateHaskell #-}

module Main where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.State.Static.Local
import Effectful.TH
import Data.List
import Prelude hiding (gets, evalState)
import Effectful.Error.Static

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
  PromptUser msg -> pure userResponse

data Env :: Effect where
  GetEnv :: String ->  Env m (Maybe String)
makeEffect ''Env

runEnvPureFailedLookup :: Eff (Env : es) a -> Eff es a
runEnvPureFailedLookup = interpret $ \_ -> \case
  GetEnv name -> pure Nothing

newtype Playlist = Playlist String deriving stock (Eq, Ord, Show)

data Spotify :: Effect where
  SearchPlaylists :: String -> Spotify m [Playlist]
makeEffect ''Spotify

newtype MockEnv = MockEnv
  { mockPlaylists :: [Playlist] }
  deriving newtype (Eq, Ord, Show)

newtype SpotifyError = SpotifyError String deriving newtype Show

searchPlaylistsProgram :: forall {es :: [Effect]}. (Error SpotifyError :> es, Env :> es, Spotify :> es) => String -> Eff es [Playlist]
searchPlaylistsProgram query = do
  token <- getEnv "spotify_token"
  case token of
    Just _token -> do
      searchPlaylists query
    Nothing -> throwError $ SpotifyError "token not set"

searchPlaylistPureFunction :: String -> [Playlist] -> [Playlist]
searchPlaylistPureFunction query playlists = do
  filter (\p -> query `isInfixOf` playlistToString p) playlists

playlistToString :: Playlist -> String
playlistToString (Playlist s) = s

runSpotifyPureConst :: Error SpotifyError :> es => MockEnv -> Eff (Spotify : es) a -> Eff es a
runSpotifyPureConst mock0 = reinterpret (evalState mock0) $ \_ -> \case
  SearchPlaylists query -> do
    playlists <- gets mockPlaylists
    pure $ searchPlaylistPureFunction query playlists
    -- searchPlaylistsProgram query

main :: IO ()
main = do
  -- TODO I want to throw a wrench in things here by making the SearchPlaylists function throw an error if there is no auth token
  -- then after that I want to add logic to handle that auth error and prompt the user to login
  searchPlaylists "foo" & runSpotifyPureConst (MockEnv playlists) & runErrorNoCallStack @SpotifyError & runPureEff & print
    where playlists = Playlist <$> ["something","foofighters","somethingelse","food playlist"]
