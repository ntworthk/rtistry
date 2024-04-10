library(httr)
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
PATH <- dirname(script.name)
PATH <- ifelse(length(PATH) == 0, '.httr-strava-oauth', file.path(PATH, '.httr-strava-oauth'))

tkn <- readRDS(PATH)
ftkn <- tkn[[1]]
# if (ftkn$credentials$expires_at < as.integer(Sys.time())) {
res <- POST(
  "https://www.strava.com/api/v3/oauth/token",
  body = list(
    client_id = app_client_id,
    client_secret = app_secret,
    grant_type = "refresh_token",
    refresh_token = ftkn$credentials$refresh_token
  )
)
new_token <- content(res)
ftkn$credentials$expires_at <- new_token$expires_at
ftkn$credentials$expires_in <- new_token$expires_in
ftkn$credentials$refresh_token <- new_token$refresh_token
ftkn$credentials$access_token <- new_token$access_token
tkn[[1]] <- ftkn
saveRDS(tkn, file.path(PATH, '.httr-strava-oauth'))
# }
stoken <- config(token = ftkn)
