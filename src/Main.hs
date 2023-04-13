import qualified Data.Aeson as Aeson
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Control.Exception (IOException)
import Control.Monad.Catch (catch)
import qualified System.IO as IO
import Effectful.Error.Static
import qualified Data.Map.Strict as M
import Effectful.State.Static.Local
import Prelude hiding (modify, evalState)

data Http :: Effect where
  HttpGet   :: String -> Http m Aeson.Value
  HttpPost  :: String -> Aeson.Object -> Http m Aeson.Value

newtype HttpError = HttpError String deriving Show

data Spotify :: Effect where
  SearchTrack :: String -> Spotify m Aeson.Value

type instance DispatchOf Http = Dynamic
type instance DispatchOf Spotify = Dynamic

newtype SpotifyError = SpotifyError String deriving Show

runHttpPure
  :: (Error HttpError :> es, Error SpotifyError :> es)
  => M.Map String Aeson.Value
  -> Eff (Http : Spotify : es) a
  -> Eff es a
runHttpPure db0 = reinterpret (evalState db0) $ \_ -> \case
  HttpPost path req -> case req of
    Aeson.Object obj -> do
      modify $ M.insert path (Aeson.String "success")
      case path of
        "https://api.spotify.com/v1/search?type=track&q=" -> do
          let query = case M.lookup "q" obj of
                Just (Aeson.String q) -> q
                _                     -> ""
          pure $ Aeson.object ["tracks" Aeson..= Aeson.object ["items" Aeson..= Aeson.String query]]
        _ -> throwError $ HttpError $ "Invalid path: " ++ path
    _ -> throwError $ HttpError "Invalid request"

runSpotifyPure
  :: (Http :> es, Error SpotifyError :> es)
  => Eff (Spotify : es) a
  -> Eff es a
runSpotifyPure = interpret $ \_ -> \case
  SearchTrack query -> do
    let req = Aeson.object ["q" Aeson..= query]
    res <- httpPost "https://api.spotify.com/v1/search?type=track&q=" req
           `catchError` \e -> throwError $ SpotifyError e
    case M.lookup "tracks" (Aeson.object ["tracks" Aeson..= res]) of
      Just tracks -> pure tracks
      Nothing     -> throwError $ SpotifyError "Failed to parse response"

action :: (Http :> es, Spotify :> es) => Eff es Bool
action = do
  res <- searchTrack "effectful"
  case M.lookup "items" res of
    Just (Aeson.String query) -> httpPost "https://example.com/" (Aeson.object ["q" Aeson..= query]) >> pure True
    _                         -> pure False

main = do
  let db = M.empty
  print $ runPureEff $ runSpotifyPure $ runHttpPure db $ action
