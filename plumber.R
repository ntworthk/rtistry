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

source("helpers.R")

#* @apiTitle Personal APIs

#* Plot a cardioid as an svg
#* @serializer svg
#* @get /plot
#* @apiTag cardioid
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
#* @apiTag cardioid
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
#* @apiTag cardioid
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



#* Return a cardioid outlnie as a png
#* @serializer png
#* @param colour Colour of the outline as a hex code. Defaults to random colour.
#* @get /png
#* @apiTag cardioid
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
#* @apiTag random
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
#* @apiTag data
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
#* @apiTag data
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
    
  }
  
  g <- ggplot(cpi, aes(x = date, y = inflation, colour = type, size = type)) +
    geom_xspline() +
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

#* Exercise summary
#* @serializer json
#* @param sport running or cycling.
#* @get /exercise/summary
#* @apiTag exercise
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
#* @param per_day Distance to go per day (or total). Default FALSE.
#* @get /exercise/distance
#* @apiTag exercise
function(per_day = FALSE){
  
  data <- read_csv(
    file = "data/distance_to_go.csv",
    skip = 1,
    col_names = c("distance_to_go")
  )
  
  days_to_go <- 1
  
  if (per_day) {
    
    cur_date <- Sys.Date()
    if (Sys.timezone() == "Etc/UTC") {
      cur_date <- as.Date(Sys.time() + 11 * 60 * 60)
    }
    
    days_to_go <- as.numeric(as.Date("2024-01-01") - cur_date, unit = "days")
    
  }
  
  list(
    distance = round(data$distance_to_go / days_to_go, digits = 2)
  )
  
}

#* Get now playing
#* @serializer unboxedJSON
#* @get /nowplaying
#* @apiTag music
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
        is_playing = "FALSE",
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
#* @apiTag music
function(time_range = "short_term", limit = 1) {
  authorization <- get_spotify_authorization_code()
  
  if (!time_range %in% c("short_term", "medium_term", "long_term")) {
    time_range <- "short_term"
  }
  if (!is.numeric(limit) | limit < 1 | limit > 50) {
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
#* @apiTag music
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
  
  list(url = unlist(lapply(content(res)$items, function(d) {d$album$images[[1]]$url})))
  
}
