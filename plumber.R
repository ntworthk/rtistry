#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(ggplot2)
library(readr)
library(stringr)
library(readabs)
library(dplyr)
library(scales)
library(ggalt)
library(ggdirectlabel)
library(httr)
library(jsonlite)
library(rvest)
library(ggtext)
library(purrr)
library(lubridate)
library(glue)
library(openai)
library(knitr)
library(DBI)
library(uuid)

source("helpers.R")

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "https://ntworthk.github.io")
  res$setHeader("Access-Control-Allow-Methods", "POST")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @apiTitle Personal APIs
#* @apiTag cardioid APIs to make cardioids
#* @apiTag random Random bunch of items
#* @apiTag data Public data
#* @apiTag exercise Exercise stats
#* @apiTag music Spotify stats

#* Plot a cardioid as an svg
#* @serializer svg
#* @get /plot
#* @tag cardioid
function(){
  col <- runif(3)
  
  colour <- rgb(col[1], col[2], col[3])
  
  t <- seq.int(0, 6280, 10)/1000
  
  a <- 3
  x <- 2 * a * (1 - cos(t)) * cos(t)
  y <- 2 * a * (1 - cos(t)) * sin(t)
  tbl <- data.frame(x, y)
  
  g <- ggplot(tbl, aes(x, y)) +
    geom_polygon(fill = colour) +
    theme_void() +
    theme(panel.background = element_blank(), plot.background = element_blank())
  
  print(g)
}

#* Return a cardioid as an svg
#* @serializer contentType list(type="image/svg+xml")
#* @get /cardioid
#* @tag cardioid
function(){
  
  img <- paste0(readLines(con = "cardioid.svg"), collapse = "")
  col <- round(runif(3)*100000)/1000
  img <- gsub("COLRED", col[1], img)
  img <- gsub("COLGRN", col[2], img)
  img <- gsub("COLBLU", col[3], img)
  
  img
  
}

#* Return a cardioid outline as an svg
#* @serializer contentType list(type="image/svg+xml")
#* @param width Width of the outline. Default 2.
#* @param colour Colour of the outline as a hex code. Defaults to random colour.
#* @get /cardioid_outline
#* @tag cardioid
function(width = 2, colour = NULL){
  
  img <- paste0(readLines(con = "cardioid.svg"), collapse = "")
  
  if (is.null(colour) || !str_detect(colour, "^#*[A-Fa-f0-9]{6,8}$")) {
    
    col <- round(runif(3)*100000)/1000
    
  } else {
    
    if (!str_detect(colour, "#")) {
      colour <- paste0("#", colour)
    }
    
    if (str_detect(colour, "^#*[A-Fa-f0-9]{8}$")) {
      colour <- paste0("#", str_sub(colour, 4, 9))
    }
    
    col <- as.vector(col2rgb(colour)) / 2.55
    
  }
  
  stroke <- paste0("stroke:rgb(", col[1], "%,", col[2], "%,", col[3], "%);stroke-width:",width,";")
  img <- gsub("stroke:none;", stroke, img)
  img <- gsub("fill:rgb\\(COLRED%,COLGRN%,COLBLU%\\)", "fill:none", img)
  
  img
  
}



#* Return a cardioid outline as a png
#* @serializer png
#* @param colour Colour of the outline as a hex code. Defaults to random colour.
#* @get /png
#* @tag cardioid
function(colour = NULL){
  
  
  if (is.null(colour) || !str_detect(colour, "^#*[A-Fa-f0-9]{6}$")) {
    
    col <- runif(3)
    colour <- rgb(col[1], col[2], col[3])
    
  } else {
    
    if (!str_detect(colour, "#")) {
      colour <- paste0("#", colour)
    }
    
    colour <- as.vector(col2rgb(colour)) / 255
    
  }
  
  t <- seq.int(0, 6280, 10)/1000
  
  a <- 3
  x <- 2 * a * (1 - cos(t)) * cos(t)
  y <- 2 * a * (1 - cos(t)) * sin(t)
  tbl <- data.frame(x, y)
  
  g <- ggplot(tbl, aes(x, y)) +
    geom_polygon(fill = colour) +
    theme_void() +
    theme(panel.background = element_blank(), plot.background = element_blank()) +
    theme(
      
    )
  
  print(g)
}

#* Get some wisdom
#* @serializer json
#* @param max_length Maximum length of the quote to return.
#* @get /wisdom
#* @tag random
function(max_length = 143, short = FALSE){
  
  max_length <- as.integer(max_length)
  
  wisdom <- read_file("https://github.com/merlinmann/wisdom/raw/master/wisdom.md")
  wisdom <- str_extract(wisdom, "The Management(.|\n)*")
  wisdom <- str_split(wisdom, "\n")
  wisdom <- unlist(wisdom)
  wisdom <- str_subset(wisdom, "^- ")
  wisdom <- str_subset(wisdom, "elated:", negate = TRUE)
  wisdom <- str_remove(wisdom, "- ")
  
  if (short) {
    wisdom_lengths <- sapply(wisdom, nchar, simplify = TRUE, USE.NAMES = FALSE)
    
    wisdom <- wisdom[wisdom_lengths < 30]
    
  } 
  
  wisdom <- sample(wisdom, 1)
  
  wisdom_short <- ifelse(nchar(wisdom) > max_length + 3, paste0(str_extract(wisdom, paste0(".{", max_length, "}")), "..."), wisdom)
  
  wisdom_short_html <- str_replace_all(wisdom_short, "\\*\\*", "~")
  wisdom_short_html <- str_replace_all(wisdom_short_html, "\\*", "⍨")
  
  wisdom_short_html <- str_replace_all(wisdom_short_html, "~([[:alnum:]|[[:punct:]]|[:space:]]*)~", "[b]\\1[/b]")
  wisdom_short_html <- str_replace_all(wisdom_short_html, "⍨([[:alnum:]|[[:punct:]]|[:space:]]*)⍨", "[i]\\1[/i]")
  
  url_find_text <- str_extract(wisdom_short, paste0(".{", min(nchar(wisdom_short), 30), "}"))
  url_find_text <- unlist(strsplit(url_find_text, " "))
  url_find_text <- url_find_text[1:(length(url_find_text) - 1)]
  url_find_text <- paste(url_find_text, collapse = " ")
  url_find_text <- URLencode(url_find_text, reserved = TRUE)
  url <- paste0("https://github.com/merlinmann/wisdom/blob/master/wisdom.md#:~:text=", url_find_text)
  
  list(
    wisdom = wisdom,
    wisdom_short = wisdom_short,
    wisdom_short_html = wisdom_short_html,
    author = "Merlin Mann",
    url = url
  )
  
}

#* Plot Australia's CPI as an svg
#* @serializer svg list(width = 17.28, height = 9.72)
#* @param since Plot CPI starting from this year.
#* @get /cpi/svg
#* @tag data
function(since = 2000){
  
  cpi <- read_cpi()
  
  cpi$inflation <- cpi$cpi / lag(cpi$cpi, 4) - 1
  cpi$month <- abs(parse_number(str_extract(as.character(cpi$date), "-[0-9]{2}-")))
  cpi <- cpi[!is.na(cpi$inflation), ]
  cpi$type <- "Quarterly"
  annual <- cpi[cpi$month == 6, ]
  annual$type <- "Annual"
  cpi <- rbind(cpi, annual)
  cpi$type <- factor(cpi$type, c("Quarterly", "Annual"), ordered = TRUE)
  cpi$inflation_label <- percent(cpi$inflation, accuracy = 0.1)
  
  cpi <- cpi[parse_number(as.character(cpi$date)) >= as.numeric(since), ]
  
  g <- ggplot(cpi, aes(x = date, y = inflation, colour = type, size = type)) +
    geom_xspline() +
    geom_text(data = function(x) {filter(x, date == max(date), type == "Quarterly")}, aes(label = inflation_label), hjust = 0, nudge_x = 100, size = 5, show.legend = FALSE) +
    scale_x_date(name = NULL) +
    scale_y_continuous(name = "CPI inflation (YoY)", labels = scales::percent_format(accuracy = 1)) + 
    scale_colour_manual(name = NULL, values = c("#008698", "#232C31")) +
    scale_size_manual(values = c("Quarterly" = 0.7, "Annual" = 1), guide = guide_none()) +
    theme_classic(base_size = 16, base_family = "Lato") +
    theme(
      panel.grid.major.y = element_line(),
      plot.background = element_rect("lightyellow"),
      panel.background = element_blank(),
      legend.position = "bottom", legend.background = element_blank()
    )
  
  print(g)
}

#* Plot Australia's CPI
#* @serializer png list(width = 1920, height = 1080, res = 200)
#* @param since Plot CPI starting from this year.
#* @get /cpi
#* @tag data
function(since = 2000, monthly = TRUE){
  
  cpi <- read_cpi()
  
  cpi$inflation <- cpi$cpi / lag(cpi$cpi, 4) - 1
  cpi$month <- abs(parse_number(str_extract(as.character(cpi$date), "-[0-9]{2}-")))
  cpi <- cpi[!is.na(cpi$inflation), ]
  cpi$type <- "Quarterly"
  annual <- cpi[cpi$month == 6, ]
  annual$type <- "Annual (June)"
  cpi <- rbind(cpi, annual)
  cpi$type <- factor(cpi$type, c("Quarterly", "Annual (June)"), ordered = TRUE)
  cpi$inflation_label <- percent(cpi$inflation, accuracy = 0.1)
  
  cpi <- cpi[parse_number(as.character(cpi$date)) >= as.numeric(since), ]
  
  headline_inflation <- last(cpi[cpi$type == "Quarterly", ]$inflation_label)
  last_date <- format(last(cpi[cpi$type == "Quarterly", ]$date), "%B %Y")
  
  if (monthly) {
    
    monthly_cpi <- read_abs(cat_no = "6484.0", tables = "1")
    
    monthly_cpi <- monthly_cpi[monthly_cpi$series == "Index Numbers ;  All groups CPI ;  Australia ;", ]
    
    monthly_cpi$inflation <- monthly_cpi$value / lag(monthly_cpi$value, n = 12) - 1
    monthly_cpi$month <- as.numeric(str_sub(as.character(monthly_cpi$date), 6, 7))
    monthly_cpi$type <- "Monthly"
    monthly_cpi$inflation_label <- percent(monthly_cpi$inflation, accuracy = 0.1)
    monthly_cpi$cpi <- monthly_cpi$value
    
    monthly_cpi <- monthly_cpi[!is.na(monthly_cpi$inflation), ]
    
    monthly_cpi <- monthly_cpi[, c("date", "cpi", "inflation", "month", "type", "inflation_label")]
    
    monthly_cpi <- monthly_cpi[parse_number(as.character(monthly_cpi$date)) >= as.numeric(since), ]
    
    cpi <- bind_rows(cpi, monthly_cpi)
    headline_inflation <- last(cpi[cpi$type == "Monthly", ]$inflation_label)
    last_date <- format(last(cpi[cpi$type == "Monthly", ]$date), "%B %Y")
    
    
  }
  
  g <- ggplot(cpi, aes(x = date, y = inflation, colour = type, size = type)) +
    annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03, fill = "lightblue", alpha = 0.4) +
    geom_xspline() +
    geom_point() +
    geom_finallabel(data = function(x) {filter(x, type %in% c("Monthly", "Quarterly"))}, aes(label = inflation_label), size = 5, show.legend = FALSE, nudge_x_perc = 0.5) +
    # geom_text(data = function(x) {filter(x, date == max(date), type == "Quarterly")}, aes(label = inflation_label), hjust = 0, nudge_x = 50, size = 5, show.legend = FALSE) +
    scale_x_date(name = NULL, expand = expansion(mult = c(0.05, 0.06))) +
    scale_y_continuous(name = "CPI inflation (YoY)", labels = scales::percent_format(accuracy = 1)) + 
    scale_colour_manual(name = NULL, values = c("Monthly" = "#c94b20", "Quarterly" = "#008698", "Annual (June)" = "#232C31")) +
    scale_size_manual(values = c("Monthly" = 0.5, "Quarterly" = 0.7, "Annual (June)" = 1), guide = guide_none()) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 16, base_family = "Lato") +
    theme(
      panel.grid.major.y = element_line(),
      plot.background = element_rect("lightyellow"),
      panel.background = element_blank(),
      legend.position = "bottom", legend.background = element_blank(),
      legend.margin = margin(t = -10)
    ) +
    labs(
      title = paste0("CPI inflation in Australia is currently at ", headline_inflation),
      subtitle = paste0("Data up to ", last_date),
      caption = "Data: ABS. Chart: @nwbort"
    )
  
  print(g)
}

#* Plot Australia's trimmed mean inflation
#* @serializer png list(width = 1920, height = 1080, res = 200)
#* @param since Plot trimmed mean inflation starting from this year.
#* @get /trimmed
#* @tag data
function(since = 2000, monthly = TRUE){
  
  cpi <- read_abs("6401.0")
  
  cpi <- cpi |>
    filter(str_detect(series, "Index Numbers ;  Trimmed Mean ;  Australia ;"), !is.na(value))
  
  cpi$inflation <- cpi$value / lag(cpi$value, 4) - 1
  cpi$month <- abs(parse_number(str_extract(as.character(cpi$date), "-[0-9]{2}-")))
  cpi <- cpi[!is.na(cpi$inflation), ]
  cpi$type <- "Quarterly"
  annual <- cpi[cpi$month == 6, ]
  annual$type <- "Annual (June)"
  cpi <- rbind(cpi, annual)
  cpi$type <- factor(cpi$type, c("Quarterly", "Annual (June)"), ordered = TRUE)
  cpi$inflation_label <- percent(cpi$inflation, accuracy = 0.1)
  
  headline_inflation <- last(cpi[cpi$type == "Quarterly", ]$inflation_label)
  last_date <- format(last(cpi[cpi$type == "Quarterly", ]$date), "%B %Y")
  
  if (monthly) {
    
    monthly_cpi <- read_abs(cat_no = "6484.0") |>
      filter(str_detect(series, "Percentage Change from Corresponding Month of Previous Year ;  Annual Trimmed Mean"))
    
    monthly_cpi$inflation <- monthly_cpi$value / 100
    monthly_cpi$month <- as.numeric(str_sub(as.character(monthly_cpi$date), 6, 7))
    monthly_cpi$type <- "Monthly"
    monthly_cpi$inflation_label <- percent(monthly_cpi$inflation, accuracy = 0.1)
    monthly_cpi$cpi <- monthly_cpi$value
    
    monthly_cpi <- monthly_cpi[!is.na(monthly_cpi$inflation), ]
    
    monthly_cpi <- monthly_cpi[, c("date", "cpi", "inflation", "month", "type", "inflation_label")]
    
    monthly_cpi <- monthly_cpi[parse_number(as.character(monthly_cpi$date)) >= as.numeric(since), ]
    
    cpi <- bind_rows(cpi, monthly_cpi)
    headline_inflation <- last(cpi[cpi$type == "Monthly", ]$inflation_label)
    last_date <- format(last(cpi[cpi$type == "Monthly", ]$date), "%B %Y")
    
    
  }
  
  cpi <- cpi[parse_number(as.character(cpi$date)) >= as.numeric(since), ]
  
  g <- ggplot(cpi, aes(x = date, y = inflation, colour = type, size = type)) +
    annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = 0.02, ymax = 0.03, fill = "lightblue", alpha = 0.4) +
    geom_xspline() +
    geom_point() +
    geom_finallabel(data = function(x) {filter(x, type %in% c("Monthly", "Quarterly"))}, aes(label = inflation_label), size = 5, show.legend = FALSE, nudge_x_perc = 0.5) +
    scale_x_date(name = NULL, expand = expansion(mult = c(0.05, 0.06))) +
    scale_y_continuous(name = "Trimmed mean inflation (YoY)", labels = scales::percent_format(accuracy = 1)) + 
    scale_colour_manual(name = NULL, values = c("Monthly" = "#c94b20", "Quarterly" = "#008698", "Annual (June)" = "#232C31")) +
    scale_size_manual(values = c("Monthly" = 0.5, "Quarterly" = 0.7, "Annual (June)" = 1), guide = guide_none()) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 16, base_family = "Lato") +
    theme(
      panel.grid.major.y = element_line(),
      plot.background = element_rect("lightyellow"),
      panel.background = element_blank(),
      legend.position = "bottom", legend.background = element_blank(),
      legend.margin = margin(t = -10)
    ) +
    labs(
      title = paste0("Trimmed mean inflation in Australia is currently at ", headline_inflation),
      subtitle = paste0("Data up to ", last_date),
      caption = "Data: ABS. Chart: @nwbort"
    )
  
  print(g)
}


#* Exercise summary
#* @serializer json
#* @param sport running or cycling.
#* @get /exercise/summary
#* @tag exercise
function(sport = "running"){
  
  if (sport == "running") {
    file <- "data/running.csv"
  } else {
    file <- "data/cycling.csv"
  }
  
  data <- read_csv(
    file = file,
    skip = 1,
    col_names = c("week", "blank_1", "blank_2", "distance", "distance_per_day"),
    col_types = "clld"
  )
  
  data$rel_distance <- data$distance / max(c(data$distance, 1)) * 100
  
  data <- data[, c("week", "distance", "rel_distance")]
  
  list(
    sport = sport,
    week = data$week,
    distance = data$distance,
    rel_distance = round(data$rel_distance, digits = 1)
  )
  
}

#* Exercise - distance to go to goal
#* @serializer json
#* @param time_period Distance to go for the rest of the "year", or "day", or "week" averages. Default "year".
#* @param per_day Deprecated. Use time_period = "day"
#* @get /exercise/distance
#* @tag exercise
function(time_period = "year", per_day = FALSE){
  
  data <- read_csv(
    file = "data/distance_to_go.csv",
    skip = 1,
    col_names = c("distance_to_go")
  )
  
  days_test <- {
    
    cur_date <- Sys.Date()
    
    if (Sys.timezone() == "Etc/UTC") {
      cur_date <- as.Date(Sys.time() + 11 * 60 * 60)
    }
    
    as.numeric(as.Date("2026-01-01") - cur_date, unit = "days")
  }
  
  days_to_go <- case_when(
    
    per_day | time_period == "day" ~ days_test,
    time_period == "week" ~ days_test / 7,
    TRUE ~ 1
    
  )
  
  
  list(
    distance = round(data$distance_to_go / days_to_go, digits = 2)
  )
  
}

#* Get now playing
#* @serializer unboxedJSON
#* @get /nowplaying
#* @tag music
function() {
  authorization <- get_spotify_authorization_code()
  base_url <- "https://api.spotify.com/v1/me/player/currently-playing"
  params <- list(market = NULL, additional_types = "episode")
  res <- RETRY("GET", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  
  if (res$status_code == 204) {
    res <- list(
      item = list(
        name = "Nothing playing",
        artists = list(
          name = ""
        ),
        album = list(
          name = "",
          images = list(
            url = "images/nothing_playing.png"
          ),
          uri = ""
        ),
        is_playing = FALSE,
        external_urls = list(
          spotify = "https://open.spotify.com/playlist/0K4OmUbmC92owGTqSRczaE"
        )
      )
    )
  } else {
    res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                    flatten = TRUE)
    
    if (res$currently_playing_type == "episode") {
      
      # reformat into style of song
      res <- list(
        item = list(
          name = res$item$name,
          artists = list(
            name = res$item$show$publisher
          ),
          album = list(
            name = res$item$show$name,
            images = list(
              url = res$item$show$images$url
            ),
            uri = res$item$show$uri
          ),
          external_urls = list(
            spotify = res$item$external_urls$spotify
          )
        ),
        is_playing = res$is_playing
      )
      
    }
    
  }
  
  if ("is_local" %in% names(res$item)) {
    
    if (res$item$is_local) {
      
      res$item$album$images <- ""
      res$item$album$uri <- ""
    }
    
  }
  
  list(
    name = res$item$name,
    artist = paste0(res$item$artists$name, collapse = ", "),
    album = res$item$album$name,
    album_art = res$item$album$images$url[1],
    is_playing = res$is_playing,
    url_spotify = res$item$external_urls$spotify,
    uri = res$item$album$uri
  )
  
}

#* Get top artists
#* @serializer unboxedJSON
#* @param time_range One of "short_term", "medium_term" or "long_term"
#* @param limit Number of results to return
#* @get /top_artists
#* @tag music
function(time_range = "short_term", limit = 1L) {
  authorization <- get_spotify_authorization_code()
  
  if (!time_range %in% c("short_term", "medium_term", "long_term")) {
    time_range <- "short_term"
  }
  limit <- as.integer(limit)
  if (is.na(limit) | limit < 1 | limit > 50) {
    limit <- 1
  }
  
  base_url <- paste0("https://api.spotify.com/v1/me/top/artists?time_range=", time_range, "&limit=", limit)
  res <- RETRY("GET", base_url, config(token = authorization), 
               encode = "json")
  stop_for_status(res)
  
  list(url = unlist(lapply(content(res)$items, function(d) {d$images[[1]]$url})))
  
}

#* Get top tracks
#* @serializer unboxedJSON
#* @param time_range One of "short_term", "medium_term" or "long_term"
#* @param limit Number of results to return
#* @get /top_tracks
#* @tag music
function(time_range = "short_term", limit = 1) {
  authorization <- get_spotify_authorization_code()
  
  if (!time_range %in% c("short_term", "medium_term", "long_term")) {
    time_range <- "short_term"
  }
  if (!is.numeric(limit) | limit < 1 | limit > 50) {
    limit <- 1
  }
  
  base_url <- paste0("https://api.spotify.com/v1/me/top/tracks?time_range=", time_range, "&limit=", limit)
  res <- RETRY("GET", base_url, config(token = authorization), 
               encode = "json")
  stop_for_status(res)
  
  list(
    url = unlist(lapply(content(res)$items, function(d) {d$album$images[[1]]$url})),
    title = unlist(lapply(content(res)$items, function(d) {d$name}))
  )
  
}

#* Get parcel
#* @serializer unboxedJSON
#* @param parcel_id
#* @get /parcel
#* @tag random
function(parcel_id = "MZ8500709501000964506", wrap = 10) {
  
  cookies = c(
    `datadome` = "2fIPF4P99okdf1Ey_plGkBeHMXGkciGt_ZntX4l0h1vxSbxTcw0GQy7nnNFbV78qqomU8qi2oF2y2JLQnOpUHDhyAHZw73MQMTwxni4eAANqiPDJ8mAmtekaZSOhGiju",
    `at_check` = "true",
    `AMCVS_0A2D38B352782F1E0A490D4C%40AdobeOrg` = "1",
    `mbox` = "session#229e41e595b44c63af8680d7769ec190#1698114793",
    `AMCV_0A2D38B352782F1E0A490D4C%40AdobeOrg` = "1176715910|MCIDTS|19655|MCMID|91415930523822547019171594295863950380|MCAID|NONE|MCOPTOUT-1698120149s|NONE|vVersion|5.4.0"
  )
  
  
  headers = c(
    `AP_CHANNEL_NAME` = "WEB_DETAIL",
    `Accept` = "application/json, text/plain, */*",
    `Accept-Language` = "en-AU,en-NZ;q=0.9,en-GB;q=0.8,en-US;q=0.7,en;q=0.6",
    `Cache-Control` = "no-cache",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36",
    `api-key` = "d11f9456-11c3-456d-9f6d-f7449cb9af8e",
    `Referer` = "https://auspost.com.au/"
  )
  
  res <- httr::GET(
    url = paste0("https://digitalapi.auspost.com.au/shipments-gateway/v1/watchlist/shipments/", parcel_id),
    httr::add_headers(.headers=headers),
    httr::set_cookies(.cookies = cookies)
  )
  
  if (status_code(res) != 200) {
    return(
      list(
        id = parcel_id,
        status = "Not found",
        status_format = gsub(" ", "\n", "Not found"),
        descrip = "Not found",
        descrip_format = gsub(" ", "\n", "Not found")
      )
    )
  }
  
  status <- content(res)$articles[[1]]$trackStatusOfArticle
  status <- ifelse(is.null(status), "Waiting", status)
  
  num_events <- length(content(res)$articles[[1]]$details[[1]]$events)
  
  if (num_events > 0) {
    descrip <- content(res)$articles[[1]]$details[[1]]$events[[1]]$description
  } else {
    descrip <- ""
  }
  
  descrip_format <- str_wrap(descrip, width = as.integer(wrap))
  
  
  list(
    id = parcel_id,
    status = status,
    status_format = gsub(" ", "\n", status),
    descrip = descrip,
    descrip_format = descrip_format
  )
  
}

#* Get ACCC chart
#* @serializer png list(width = 15, height = 15, units = "cm", res = 200)
#* @param since Plot decisions starting from this year.
#* @get /accc
#* @tag data
function(since = 2014){
  
  url <- "https://www.accc.gov.au/public-registers/browse-public-registers?f%5B0%5D=type%3Aacccgov_informal_merger_review"
  
  pg <- read_html(url)
  
  decision_types <- html_attr(html_elements(pg, xpath = '//*[(@id = "accc-facet-area__title--acccgov_outcome")]//a'), "href")
  
  urls <- paste0("https://www.accc.gov.au/", decision_types)
  
  decisions <- map_dfr(urls, function(url) {
    
    pg <- read_html(url)
    pages <- html_elements(pg, css = ".page-item--last")
    pages <- html_element(pages, "a")
    pages <- html_attr(pages, "href")
    pages <- str_extract(pages, "[0-9]+$")
    pages <- as.integer(pages)
    
    map_dfr(0:pages, function(page_num) {
      
      current_url <- paste0(url, "&page=", page_num)
      pg <- read_html(current_url)
      
      accc_card_full_width <- html_elements(pg, css = ".accc-card--full-width")
      
      titles <- html_element(accc_card_full_width, css = ".field--name-node-title")
      titles <- html_text2(titles)
      
      links <- html_element(accc_card_full_width, css = ".field--name-node-title")
      links <- html_children(links)
      links <- html_children(links)
      links <- html_attr(links, "href")
      
      outcomes <- accc_card_full_width
      outcomes <- html_element(outcomes, css = ".accc-card__metadata")
      outcomes <- html_element(outcomes, css = ".field--name-field-acccgov-pub-reg-outcome")
      outcomes <- html_element(outcomes, css = ".field__item")
      outcomes <- html_text2(outcomes)
      
      date_completed <- accc_card_full_width
      date_completed <- html_element(date_completed, css = ".accc-card__metadata")
      date_completed <- html_element(date_completed, css = ".field--name-field-acccgov-pub-reg-end-date")
      date_completed <- html_element(date_completed, css = ".field__item")
      date_completed <- html_text2(date_completed)
      
      industry <- accc_card_full_width
      industry <- html_element(industry, css = ".accc-card__metadata")
      industry <- html_element(industry, css = ".field--name-field-acccgov-industry")
      industry <- map(industry, function(node) {
        
        html_text2(html_elements(node, css = ".field__item"))
        
      })
      
      status <- accc_card_full_width
      status <- html_element(status, css = ".accc-card__metadata")
      status <- html_element(status, css = ".field--name-field-acccgov-pub-reg-status")
      status <- html_element(status, css = ".field__item")
      status <- html_text2(status)
      
      tibble(
        title = titles,
        link = links,
        outcome = outcomes,
        date_completed = date_completed,
        industry = industry,
        status = status
      )
      
    })
    
    
  })
  
  url <- "https://www.accc.gov.au/public-registers/browse-public-registers?f%5B0%5D=acccgov_status%3A423&f%5B1%5D=type%3Aacccgov_informal_merger_review"
  
  pg <- read_html(url)
  
  accc_card_full_width <- html_elements(pg, css = ".accc-card--full-width")
  
  titles <- accc_card_full_width %>% 
    html_element(css = ".field--name-node-title") %>% 
    html_text2()
  
  links <- accc_card_full_width %>% 
    html_element(css = ".field--name-node-title") %>% 
    html_children() %>% 
    html_children() %>% 
    html_attr("href")
  
  date_commenced <- accc_card_full_width %>% 
    html_element(css = ".accc-card__metadata") %>% 
    html_element(css = ".field--name-field-acccgov-pub-reg-date") %>% 
    html_element(css = ".field__item") %>% 
    html_text2()
  
  industry <- accc_card_full_width %>% 
    html_element(css = ".accc-card__metadata") %>% 
    html_element(css = ".field--name-field-acccgov-industry") %>% 
    map(function(node) {
      
      node %>% 
        html_elements(css = ".field__item") %>% 
        html_text2()
      
    })
  
  status <- accc_card_full_width %>% 
    html_element(css = ".accc-card__metadata") %>% 
    html_element(css = ".field--name-field-acccgov-pub-reg-status") %>% 
    html_element(css = ".field__item") %>% 
    html_text2()
  
  under_condsideration <- tibble(
    title = titles,
    link = links,
    date_commenced = date_commenced,
    industry = industry,
    status = status
  )
  
  decisions_by_year <- decisions %>% 
    bind_rows(under_condsideration) %>% 
    mutate(
      date_completed = ifelse(is.na(date_completed), date_commenced, date_completed),
      outcome = ifelse(is.na(outcome), status, outcome)
    ) %>%  
    mutate(date_completed2 = parse_date(date_completed, format = "%d %B %Y")) %>%
    count(year = year(date_completed2), outcome) %>%
    arrange(desc(n)) %>% 
    mutate(outcome = factor(outcome,
                            levels = c(
                              "Under consideration", 
                              "No decision", 
                              "Withdrawn", 
                              "Not opposed subject to undertakings", 
                              "Not opposed", 
                              "Opposed"
                            ),
                            ordered = TRUE)) %>% 
    group_by(year) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup()
  
  outcomes <- decisions_by_year %>% pull(outcome) %>% levels()
  
  outcome_colours <- c(
    "Opposed" = "#E14D18",
    "Not opposed" = "#008698",
    "Not opposed subject to undertakings" = "#8AC1AF",
    "No decision" = "#232C31",
    "Withdrawn" = "#ECAA2B",
    "Under consideration" = "#AA2E60"
  )
  
  st <- decisions_by_year %>%
    filter(year == max(year), outcome == "Opposed") %>%
    pull(n)
  
  st <- ifelse(length(st) == 0, "No", st) |> 
    paste0(" mergers opposed")
  
  st_year <- max(decisions_by_year$year)
  
  st <- paste0("<span style = 'color:", outcome_colours[["Opposed"]], ";'>**", st, "**</span> in ", st_year)
  
  g <- decisions_by_year %>% 
    filter(year >= since) %>% 
    ggplot(aes(x = year, y = n, fill = outcome)) +
    geom_col(position = position_stack()) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_text(
      data = \(x) filter(x, year == max(year)) %>% mutate(year = Inf),
      aes(label = str_wrap(outcome, width = 15), colour = outcome),
      hjust = 0,
      position = position_stack(vjust = 0.5),
      size = 2,
      lineheight = 0.7
    ) +
    geom_text(
      data = \(x) filter(x, year >= max(year) - 1 | year == min(year)),
      aes(label = scales::number(n, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      colour = "white", size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq.int(2000, year(Sys.Date()), 1),
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.003, 0.05))
    ) +
    scale_fill_manual(values = outcome_colours, aesthetics = c("colour", "fill"), guide = guide_none()) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "Informal merger reviews", fill = NULL,
         title = "Historic number of ACCC informal merger oppositions",
         subtitle = st,
         caption = "Year of decision based on 'date completed' for concluded reviews and 'date commenced' for ongoing reviews.\nSource: HoustonKemp analysis of ACCC website. Chart by Nick Twort") +
    theme_minimal(base_family = "Aptos", base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      strip.background = element_rect(fill = "#E6E7E8"),
      plot.margin = margin(2, 55, 2, 2, unit = "pt"),
      plot.caption = element_text(size = 6),
      plot.subtitle = element_markdown()
    )
  
  
  print(g)
}

#* Get departure board
#* @param stop_ids The stop ids to lookup
#* @param route Optional, name of route to filter to
#* @get /departures
#* @serializer json
#* @tag data
get_departure_board <- function(stop_ids = 213052, route = NULL) {
  
  stop_data <- map_dfr(stop_ids, function(x) { get_formatted_data(x, limit = 5, route = route)})
  
  # Output as htmlTable
  stop_data %>% 
    select(
      `Due in` = due_in,
      `Departure time` = departure_time,
      Delay = minutes_late,
      Route = route,
      Destination = dest
    )
  
}

#* Update Strava activity
#* @param id The activity ID
#* @param name New activity name
#* @param description New activity description
#* @param key Authentication key
#* @param activity URI encoded 
#* @param use_gpt Use openAI to generate title
#* @get /strava
#* @serializer json
#* @tag data
update_strava <- function(id, name = NULL, description = NULL, key, activity = NULL, use_gpt = TRUE) {
  
  source("strava_creds.R")
  
  if (file.exists("previous_strava.txt")) {
    
    previous_names <- read_lines("previous_strava.txt")
    
  } else {
    previous_names <- c("Morning Glide to the Office Odyssey", "Saturday Sprocket Sprint: A Short Sojourn to Adventure", "Whimsical Wheels Wednesday: Commute Carousel!", "Pedal Power Parade to the Office", "Whirlwind Commute: The Race from the Office", "Pedal Power: Commute Chronicles Edition 22", "Monday Magic: The Daily Ascent to Adventure", "Mystical Morning Commute: The 8.4km Odyssey")
  }
  
  if (key != strava_creds) {
    return(list("status" = "error - not authorised"))
  }
  
  
  if (is.null(name)) {
    day_name <- as.character(wday(with_tz(Sys.time(), tzone = "Australia/Sydney"), label = TRUE, abbr = FALSE))
    day_adjective <- tibble(
      adjective = c(
        "Magical", "Magnificent", "Majestic", "Manageable", "Maneuvering", "Manoeuvrable", "Martial", "Marvelous", "Massive", "Masterful", "Matchless", "Meandering", "Meaningful", "Measured", "Meditative", "Mellow", "Melodic", "Memorable", "Menacing", "Merciful", "Mercurial", "Merry", "Mesmerizing", "Metaphorical", "Meticulous", "Mettlesome", "Mighty", "Mind-blowing", "Mindful", "Minimalist", "Miraculous", "Mischievous", "Miserable", "Misleading", "Misty", "Modest", "Momentous", "Monotonous", "Monumental", "Moody", "Moonlit", "Motivating", "Motivational", "Mountainous", "Mournful", "Moving", "Muddy", "Muffled", "Multifaceted", "Mundane", "Murky", "Muscular", "MuscularlyDemanding", "Musical", "Musing", "Myriad", "Mysterious", "Mystical", "Technical", "Tense", "Terrific", "Therapeutic", "Thrilling", "Thumping", "Thunderous", "Timeless", "Tireless", "TirelesslyPrecise", "Tiresome", "Tiring", "Titrating", "Toasty", "Tolerable", "Torrid", "Tortuous", "Torturous", "Touching", "Tough", "Towering", "Tragic", "Trailblazing", "Trampolining", "Tranquil", "Tranquilizing", "Transcendent", "Transformative", "Transient", "Transparent", "Traveled", "Treacherous", "Tremendous", "Trendy", "Tricky", "Trippy", "Triumphant", "Tropical", "Troublesome", "Truthful", "Tumultuous", "Turbulent", "Twirling", "Twisting", "Twitchy", "Typical", "Warm", "Wearisome", "Weary", "Weightless", "Welcoming", "Wet", "Whimsical", "Whirling", "White-knuckle", "Whizzing", "Wholesome", "Wicked", "Wide-ranging", "Wild", "Wince-inducing", "Winding", "Wind-powered", "Wintry", "Wistful", "Wobbly", "Wonderful", "Wondrous", "Workaholic", "World-class", "Worrisome", "Worthwhile", "Wrenching", "Wretched", "Writhing", "Fabled", "Faint", "Fair", "Faithful", "Fancy", "Fantastic", "Far-reaching", "Fascinating", "Fashionable", "Fast", "Fast-paced", "Fatiguing", "Faultless", "Favorable", "Fearless", "Feasible", "Featherlight", "Feisty", "Felicitous", "Feral", "Festive", "Fetching", "Fiery", "Fine", "Finicky", "Firm", "First-rate", "Fit", "Fitting", "Flamboyant", "Flashy", "Flat", "Flawless", "Fleeting", "Flexible", "Flimsy", "Flirtatious", "Flowing", "Fluent", "Fluid", "Fluttering", "Focused", "Foolhardy", "Foolproof", "Forbidden", "Forceful", "Forcing", "Foreboding", "Forgettable", "Formative", "Formidable", "Fortifying", "Fortuitous", "Forward", "Fragile", "Fragrant", "Frank", "Frantic", "Free", "Free-flowing", "Free-wheeling", "Freezing", "Frenetic", "Frenzied", "Fresh", "Friendly", "Frightening", "Frightful", "Frilly", "Frolicsome", "Frosty", "Fruitful", "Frustrating", "Fulfilling", "Full", "Full-bodied", "Fun", "Functional", "Fundamental", "Fun-loving", "Funny", "Furious", "Furtive", "Fussy", "Futile", "Futuristic", "Fuzzy", "Satisfying", "Scenic", "Serene", "Skillful", "Slippery", "Slow-paced", "Smooth", "Soggy", "Solitary", "Soothing", "Soulful", "Speedy", "Spirited", "Splitting", "Spontaneous", "Sporty", "Sprightly", "Sprinting", "Squally", "Squishy", "Staggering", "Stagnant", "Stamina-Building", "Stamina-draining", "Starlit", "Startling", "Static", "Statuesque", "Steady", "Stealthy", "Steep", "Stimulating", "Stormy", "Strategic", "Strengthening", "Strenuous", "Stretching", "Striking", "Structured", "Stunning", "Sublime", "Sudden", "Suffocating", "Sunny", "Superb", "Surging", "Suspenseful", "Svelte", "Sweaty", "Sweeping", "Sweltering", "Swerving", "Swift", "Synchronized", "Synergistic", "Systematic"
      )
    ) |> 
      distinct() |> 
      mutate(first_letter = str_to_upper(str_sub(adjective, 1, 1))) |> 
      semi_join(tibble(first_letter = str_sub(day_name, 1, 1)), by = join_by(first_letter)) |> 
      slice_sample(n = 1) |> 
      pull(adjective)
    
    name <- paste(str_to_sentence(day_adjective), day_name, "ride")
    
  }
  
  if (is.null(description)) {
    description <- "The name of this activity has been powered by GPTwortv3."
  }
  
  if (use_gpt) {
    if (!is.null(activity)) {
      activity <- paste0('{"', str_replace_all(activity, c("%3D" = '":"', "%3B" = '","', "%20" = ' ', "%2C" = ",", "%3" = ':')), '","Weekday":"', day_name, '"}')
    }
    response <- create_chat_completion(
      model = "gpt-4o-mini",
      messages = list(
        list(
          "role" = "system",
          "content" = "You take in json information about a Strava activity (usually a bike ride) and generate a short whimsical title about the activity. If the distance is around 8-9km it is probably the user's commute to work. Ideally the title will be unique from day to day and vary in terms of sentence construction. The route is almost the exact same every day. You should limit your response to ONLY YOUR SUGGESTED TITLE with no other text and do not enclose it in quotes as the output will be used verbatim as the new title."
        ),
        list(
          "role" = "system",
          "content" = paste0("Here are some recent previous titles (so as not to repeat them): ", paste0(previous_names, collapse = ";"))
        ),
        list(
          "role" = "user",
          "content" = activity
        )
      )
    )
    name <- response$choices$message.content
    
    if (length(previous_names) >= 15) {
      previous_names <- c(previous_names[1:14], name)
    } else {
      previous_names <- c(previous_names, name)
    }
    
    write_lines(previous_names, "previous_strava.txt")
    
    
  }
  

  
  if (str_detect(id, "http")) {
    id <- str_extract(id, "activities(%2F|/)[0-9]+") |> str_remove("activities%2F|activities/")
  }
  
  source("refresh_strava.R")
  
  body <- list(
    name = name,
    description = description
  )
  
  res <- PUT(
    paste0("https://www.strava.com/api/v3/activities/", id),
    stoken,
    body = body
  )
  
  http_status(res)
  
  
}


#* Update Strava activity songs
#* @param id The activity ID
#* @param key Authentication key
#* @get /strava_song
#* @serializer json
#* @tag data
update_strava_songs <- function(id, key) {
  
  source("strava_creds.R")
  authorization <- get_spotify_authorization_code()
  
  
  base_url <- glue("https://api.spotify.com/v1/playlists/{playlist_id}")
  res <- RETRY("GET", base_url, config(token = authorization), 
               encode = "json")
  stop_for_status(res)
  
  playlist <- content(res)
  
  songs <- map_dfr(playlist$tracks$items, function(y) {
    
    track_details <- y$track[c("artists", "name")]
    
    artists <- map(track_details$artists, \(z) z$name)
    track_name <- track_details$name
    
    artists <- combine_words(artists, oxford_comma = FALSE)
    
    tibble(
      artist = artists,
      name = track_name
    )
    
  }) |> 
    mutate(artist = as.character(artist)) |> 
    mutate(artist = case_match(
      artist,
      "boygenius, Julien Baker, Phoebe Bridgers and Lucy Dacus" ~ "boygenuis",
      "Racing and Peter Urlich" ~ "Racing",
      .default = artist
    ))
  
  current_strava_song_index <- read_lines("strava_song_index.txt") |> 
    as.numeric()
  current_strava_song_index <- current_strava_song_index + 1

  new_title <- songs |> 
    mutate(title = glue("🎵 Today's tune: {artist} - {name} 🎵")) |> 
    filter(row_number() == current_strava_song_index) |> 
    pull(title)
  
  if (key != strava_creds) {
    return(list("status" = "error - not authorised"))
  }
  
  write_lines(current_strava_song_index, "strava_song_index.txt")
  
  if (str_detect(id, "http")) {
    id <- str_extract(id, "activities(%2F|/)[0-9]+") |> str_remove("activities%2F|activities/")
  }
  
  source("refresh_strava.R")
  
  body <- list(name = new_title)
  
  res <- PUT(
    paste0("https://www.strava.com/api/v3/activities/", id),
    stoken,
    body = body
  )
  
  http_status(res)
  
  
}

#* Get UN International Days
#* @param date Optional, Date to check
#* @get /days
#* @serializer json
#* @tag data
get_days <- function(date = Sys.Date(), limit = 1) {
  
  test_date <- date
  
  un_days |> 
    filter(date >= test_date) |> 
    head(limit) |> 
    mutate(title = str_replace(title, "International", "Intl")) |> 
    mutate(date_format = format(date, "%d %b"), pretty = paste0(date_format, "\n", str_wrap(title, width = 12))) |>
    as.list()

}

#* Submit picks to the database
#* @param pick_base64 The base64 encoded pick data
#* @post /submit_pick
#* @serializer unboxedJSON
#* @tag antitrusties
submit_picks <- function(pick_base64) {
  tryCatch({
    # Validate input exists
    if (is.null(pick_base64) || pick_base64 == "") {
      return(list(
        status = "error",
        message = "Missing pick_base64 parameter"
      ))
    }
    
    # Try to decode base64
    decoded_raw <- tryCatch({
      base64_dec(pick_base64)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(decoded_raw)) {
      return(list(
        status = "error",
        message = "Invalid base64 encoding"
      ))
    }
    
    # Try to convert to JSON
    picks <- tryCatch({
      rawToChar(decoded_raw) |> 
        fromJSON()
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(picks) || !all(c("picks", "name", "timestamp") %in% names(picks))) {
      return(list(
        status = "error",
        message = "Invalid JSON structure"
      ))
    }
    
    # Database operations in another tryCatch block
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "antitrusties.sqlite")
      on.exit(dbDisconnect(con))
      
      if (!dbExistsTable(conn = con, name = "picks")) {
        template_table <- tibble(
          id = character(),
          text = character(),
          points = integer(),
          risky = logical(),
          name = character(),
          timestamp = character()
        )
        
        dbCreateTable(conn = con, name = "picks", template_table)
      }
      
      rows_added <- as_tibble(picks$picks) |> 
        mutate(
          name = picks$name,
          timestamp = picks$timestamp
        ) |> 
        dbAppendTable(conn = con, name = "picks", value = _)
      
      return(list(
        status = "success",
        rows_added = rows_added,
        message = "Picks successfully submitted"
      ))
      
    }, error = function(e) {
      return(list(
        status = "error",
        message = paste("Database error:", e$message)
      ))
    })
    
  }, error = function(e) {
    return(list(
      status = "error",
      message = paste("Unexpected error:", e$message)
    ))
  })
}

#* Get most recent picks for each person
#* @get /latest_picks
#* @serializer unboxedJSON
#* @tag antitrusties
get_picks <- function() {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), "antitrusties.sqlite")
    on.exit(dbDisconnect(con))
    
    if (!dbExistsTable(conn = con, name = "picks")) {
      return(list(
        status = "error",
        message = "No picks table exists"
      ))
    }
    
    picks_sql <- tbl(con, "picks")
    
    picks <- picks_sql |> 
      group_by(name) |> 
      filter(timestamp == max(timestamp, na.rm = TRUE)) |> 
      ungroup() |>
      collect()
    
    return(list(
      status = "success",
      picks = picks
    ))
    
  }, error = function(e) {
    return(list(
      status = "error",
      message = paste("Database error:", e$message)
    ))
  })
}

#* Submit predictions to the database
#* @param pick_base64 The base64 encoded prediction data
#* @post /submit_prediction
#* @serializer unboxedJSON
#* @tag antitrusties
submit_predictions <- function(prediction_base64) {
  tryCatch({
    # Validate input exists
    if (is.null(prediction_base64) || prediction_base64 == "") {
      return(list(
        status = "error",
        message = "Missing prediction_base64 parameter"
      ))
    }
    
    # Try to decode base64
    decoded_raw <- tryCatch({
      base64_dec(prediction_base64)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(decoded_raw)) {
      return(list(
        status = "error",
        message = "Invalid base64 encoding"
      ))
    }
    
    # Try to convert to JSON
    predictions <- tryCatch({
      rawToChar(decoded_raw) |> 
        fromJSON()
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(predictions)) {
      return(list(
        status = "error",
        message = "Invalid JSON structure"
      ))
    }
    
    # Database operations in another tryCatch block
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "antitrusties.sqlite")
      on.exit(dbDisconnect(con))
      
      if (!dbExistsTable(conn = con, name = "predictions")) {
        template_table <- tibble(
          id = character(),
          text = character(),
          status = integer(),
          notes = character(),
          timestamp = POSIXct()
        )
        
        dbCreateTable(conn = con, name = "predictions", template_table)
      }
      
      rows_added <- tibble(text = predictions) |> 
        mutate(
          id = UUIDgenerate(n = n()),
          status = "pending",
          notes = NA_character_,
          timestamp = Sys.time()
        ) |> 
        dbAppendTable(conn = con, name = "predictions", value = _)
      
      return(list(
        status = "success",
        rows_added = rows_added,
        message = "Predictions successfully submitted"
      ))
      
    }, error = function(e) {
      return(list(
        status = "error",
        message = paste("Database error:", e$message)
      ))
    })
    
  }, error = function(e) {
    return(list(
      status = "error",
      message = paste("Unexpected error:", e$message)
    ))
  })
}

#* Get all predictions
#* @get /predictions
#* @serializer unboxedJSON
#* @tag antitrusties
get_predictions <- function() {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), "antitrusties.sqlite")
    on.exit(dbDisconnect(con))
    
    if (!dbExistsTable(conn = con, name = "predictions")) {
      return(list(
        status = "error",
        message = "No predictions table exists"
      ))
    }
    
    predictions_sql <- tbl(con, "predictions")
    
    predictions <- collect(predictions_sql)
    
    return(list(
      status = "success",
      predictions = predictions
    ))
    
  }, error = function(e) {
    return(list(
      status = "error",
      message = paste("Database error:", e$message)
    ))
  })
}

#* Update the status of a prediction
#* @param id The ID of the prediction to update
#* @param status The new status (pending, correct, or incorrect)
#* @param notes Optional notes about the status update
#* @param auth_code Authentication code for validation
#* @post /update_prediction_status
#* @serializer unboxedJSON
#* @tag antitrusties
update_prediction_status <- function(id, status, auth_code, notes = "") {
  tryCatch({
    # Validate auth code
    source("antitrusties_creds.R")
    if (is.null(auth_code) || auth_code != expected_code) {
      return(list(
        status = "error",
        message = "Invalid authentication code"
      ))
    }
    
    # Validate inputs
    if (is.null(id) || id == "") {
      return(list(
        status = "error",
        message = "Missing prediction ID"
      ))
    }
    
    if (is.null(status) || !status %in% c("pending", "correct", "incorrect")) {
      return(list(
        status = "error",
        message = "Invalid status value - must be 'pending', 'correct', or 'incorrect'"
      ))
    }
    
    # Database operations
    con <- dbConnect(RSQLite::SQLite(), "antitrusties.sqlite")
    on.exit(dbDisconnect(con))
    
    if (!dbExistsTable(conn = con, name = "predictions")) {
      return(list(
        status = "error",
        message = "No predictions table exists"
      ))
    }
    
    # Update the prediction status
    update_query <- dbSendQuery(
      conn = con,
      "UPDATE predictions SET status = $1, notes = $2 WHERE id = $3",
      params = list(status, notes, id)
    )
    rows_affected <- dbGetRowsAffected(update_query)
    dbClearResult(update_query)
    
    if (rows_affected == 0) {
      return(list(
        status = "error",
        message = "No prediction found with the specified ID"
      ))
    }
    
    return(list(
      status = "success",
      message = "Prediction status updated successfully"
    ))
    
  }, error = function(e) {
    return(list(
      status = "error",
      message = paste("Database error:", e$message)
    ))
  })
}
