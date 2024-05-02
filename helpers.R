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

#--- Helpers for TfNSW departures ----------------------------------------------

source("credentials.R")

get_next_departures <- function(stop_id = 213010) {
  
  nonce <- runif(n = 1)
  
  # Get data from TfNSW
  resp <- GET(
    glue("https://api.transport.nsw.gov.au/v1/tp/departure_mon?outputFormat=rapidJSON&coordOutputFormat=EPSG%3A4326&mode=direct&type_dm=stop&name_dm={stop_id}&departureMonitorMacro=true&limit={nonce}&TfNSWDM=true&version=10.2.1.42"),
    add_headers(
      "Accept" = "application/json",
      "Cache-Control" = "no-store",
      "Authorization" = glue("apikey {api_key}")
    )
  )
  
  resp_content <- content(resp)
  stop_events <- resp_content$stopEvents
  
  # Extract and return relevant data
  stop_data <- map_dfr(stop_events, function(z) {
    as_tibble(
      c(
        z['departureTimePlanned'],
        departureTimeEstimated = z[['departureTimeEstimated']],
        z$transportation[c('number', 'description')],
        stop = z$location$parent$disassembledName,
        origin = z$transportation$origin$name,
        dest = z$transportation$destination$name
      )
    )
  })
  
  # Add column if not exists
  if (!"departureTimeEstimated" %in% colnames(stop_data)) {
    stop_data <- stop_data %>% 
      mutate(departureTimeEstimated = departureTimePlanned)
  }
  
  return(stop_data)
  
}

format_stop_data <- function(stop_data, route = NULL, limit = 5) {
  
  if (!all(c("departureTimePlanned", "departureTimeEstimated", "number", "description") %in% colnames(stop_data))) {
    stop("Badly formed stop data")
  }
  
  if (!is.null(route)) {
    stop_data <- stop_data %>%
      filter(description == route)
  }
  
  # Format for output
  stop_data %>% 
    mutate(
      departureTimeEstimated = if_else(is.na(departureTimeEstimated), departureTimePlanned, departureTimeEstimated),
      departure_time = as_datetime(departureTimeEstimated, tz = "UTC"),
      departureTimePlanned = as_datetime(departureTimePlanned, tz = "UTC"),
      across(c(departure_time, departureTimePlanned), \(x) with_tz(x, tzone = "Australia/Sydney")),
      minutes_late = as.numeric(departure_time - departureTimePlanned, units = "mins"),
      minutes_late = case_when(
        minutes_late == 0 ~  "On time",
        minutes_late < 0 ~ as.character(glue("{scales::date_format('%H:%M', tz = 'Australia/Sydney')(departureTimePlanned)} running {-minutes_late} minute{if_else(abs(minutes_late) == 1, '', 's')} early")),
        minutes_late > 0 ~ as.character(glue("{scales::date_format('%H:%M', tz = 'Australia/Sydney')(departureTimePlanned)} running {minutes_late} minute{if_else(abs(minutes_late) == 1, '', 's')} late"))
      ),
      minutes_from_now = floor(as.numeric(departure_time - Sys.time(), units = "mins")),
      departure_time = scales::date_format("%H:%M", tz = "Australia/Sydney")(departure_time),
      id = row_number()
    ) %>% 
    head(limit) %>% 
    select(id, due_in = minutes_from_now, departure_time, minutes_late, route = description, dest) |> 
    arrange(due_in)
  
  
}

get_formatted_data <- function(stop_id, limit = 5, route = NULL) {
  
  get_next_departures(stop_id = stop_id) |> 
    format_stop_data(route = route, limit = limit)
  
}
