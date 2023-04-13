{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Aeson.QQ
import qualified Data.Aeson as Aeson
import Effectful
import Effectful.TH
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.Error.Static
import qualified Data.Map.Strict as M
import Prelude hiding (modify, evalState, gets)
import Optics
import Data.Aeson.Optics

newtype Url = Url String deriving newtype (Eq, Ord, Show)
newtype HttpError = HttpError String deriving newtype Show

data Http :: Effect where
  HttpGet  :: Url -> Http m Aeson.Value
makeEffect ''Http

data Spotify :: Effect where
  SearchTrack :: String -> Spotify m Aeson.Value
  -- Auth :: Spotify m () -- TODO create this and if an http request returns 401 this should get called, not sure how to enforce that?
makeEffect ''Spotify

data Track = Track { trackName :: String } deriving stock (Ord, Eq, Show)

type instance DispatchOf Http = 'Dynamic
type instance DispatchOf Spotify = 'Dynamic

newtype SpotifyError = SpotifyError String deriving newtype Show

runHttpPure
  :: (Error HttpError :> es)
  => M.Map Url Aeson.Value
  -> Eff (Http : es) a
  -> Eff es a
runHttpPure db0 = reinterpret (evalState db0) $ \_ -> \case
  HttpGet url -> gets (M.lookup url) >>= \case
    Just response -> pure response
    Nothing       -> throwError . HttpError $ "no response found"

runHttpPureNull
  :: (Error HttpError :> es)
  => Eff (Http : es) a
  -> Eff es a
runHttpPureNull = interpret $ \_ -> \case
  HttpGet _ -> pure Aeson.Null

playlistSearchUrl :: String
playlistSearchUrl = "https://api.spotify.com/v1/search?type=track&q="

runSpotifyPure
  :: (Http :> es, Error SpotifyError :> es)
  => Eff (Spotify : es) a
  -> Eff es a
runSpotifyPure = interpret $ \_ -> \case
  SearchTrack query -> do
    res <- httpGet (Url (playlistSearchUrl <> query))
    pure res
    -- TODO this logic shouldn't be here, only return the response right?
    -- let tracks = res ^.. _Array % traversed % _String & fmap (Track . toString)
    -- if length tracks == 0 then
    --   throwError (SpotifyError "no tracks found")
    --   else pure tracks
    -- TODO but if this is equivalent to just calling httpGet... what's the point?
    -- I guess we can check if it's well formed and has a 200 or 401 here?

-- the benefit here is that now searchTrackProgram can be used in either pure or impure interpreters
-- no impedance mismatch
-- I *think* this is the correct state of things and why you don't often want to make your own effects
-- for instance... I could have done without the spotify effect I think? Unless maybe I was going to add auth?
-- I was focused on how useful having a data type describing each operation was... too much maybe?
-- or maybe the other was is correct? maybe something different?
searchTrackProgram :: (Http :> es, Error SpotifyError :> es, Spotify :> es) => String -> Eff es [Track]
searchTrackProgram query = do
  trackResponse <- searchTrack query
  let tracks = trackResponse ^.. _Array % traversed % _String & fmap (Track . toString)
  if length tracks == 0 then
    throwError (SpotifyError "no tracks found")
    else pure tracks

main :: IO ()
main = do

  let httpMock :: [(Url, Aeson.Value)] = [((Url (playlistSearchUrl <> "effectful")),[aesonQQ|["foo"]|])]
  -- data HttpMockResponse = Http200 [(Url, Aeson.Value)] | Http401
  -- TODO then modify the http interpreter to look for the status code first and throw an error for Http401
  -- let httpMock :: [(Url, Aeson.Value)] = [((Url (playlistSearchUrl <> "effectful")),[aesonQQ|["foo"]|])]

  print $ searchTrackProgram "effectful"
    & runSpotifyPure
    & runErrorNoCallStack @SpotifyError
    & runHttpPure (M.fromList httpMock)
    -- & runHttpPureNull
    & runErrorNoCallStack @HttpError
    & runPureEff
