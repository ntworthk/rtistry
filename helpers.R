get_spotify_authorization_code <- function() {
  
  scope <- c("ugc-image-upload", "user-modify-playback-state", "user-follow-modify", "user-read-recently-played", "user-read-playback-position", "playlist-read-collaborative", "app-remote-control", "user-read-playback-state", "user-read-email", "streaming", "user-top-read", "playlist-modify-public", "user-library-modify", "user-follow-read", "user-read-currently-playing", "user-library-read", "playlist-read-private", "user-read-private", "playlist-modify-private")
  client_id = "47b81983b4bf4875ab7d0e02427cde08"
  client_secret = "d02b2a54067f445c8fbe6e6f339c5186"
  
  endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
                             access = "https://accounts.spotify.com/api/token")
  app <- oauth_app("spotifyr", client_id, client_secret)
  token <- (purrr::safely(.f = oauth2.0_token))(endpoint = endpoint, 
                                                app = app, scope = scope)
  if (!is.null(token$error)) {
    token$error
  }
  else {
    token$result
  }
}
