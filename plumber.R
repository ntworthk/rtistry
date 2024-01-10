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

source("helpers.R")

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



#* Return a cardioid outlnie as a png
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
#* @param per_day Deprecated. Use time_period = "daily"
#* @get /exercise/distance
#* @tag exercise
function(time_period = "year", per_day = FALSE){
  
  data <- read_csv(
    file = "data/distance_to_go.csv",
    skip = 1,
    col_names = c("distance_to_go")
  )
  
  days_to_go <- case_when(
    
    per_day | time_period == "day" ~ {
      
      cur_date <- Sys.Date()
      
      if (Sys.timezone() == "Etc/UTC") {
        cur_date <- as.Date(Sys.time() + 11 * 60 * 60)
      }
      
      as.numeric(as.Date("2024-01-01") - cur_date, unit = "days")
    },
    time_period == "week" ~ 7,
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
    url = paste0("https://digitalapi.auspost.com.au/shipmentsgatewayapi/watchlist/shipments/", parcel_id),
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
      data = \(x) filter(x, year == max(year) | year == min(year)),
      aes(label = scales::number(n, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      colour = "white", size = 2.5
    ) +
    scale_x_continuous(
      breaks = seq.int(2000, 2023, 1),
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

