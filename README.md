# real-world-advanced-testing-using-effectful

context: https://www.reddit.com/r/haskell/comments/12im0z9/effectful_and_polysemy_users_how_do_you_test_any/

```
I'm thinking of a CLI app that does this:

$ ./app search-playlist foo
HttpError: 401
Please login again
Username: <enter username, enter>
Password: <enter password, enter>

Playlists:

- foofighters
- Food music
So the SpotifyEffect would use the HttpEffect. Then the CLI would call App.searchProgram :: Eff '[SpotifyEffect, CliEffect] [Playlist] which would call Spotify.searchProgram :: Eff '[SpotifyEffect] [Playlist].
```

I'd also like to various kinds of testing like "all spotify effects respond to Http401Error by prompting user for login/password" and things like that.
