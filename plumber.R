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
library(markdown)

#* @apiTitle Plumber Example API

#* Plot a histogram
#* @serializer svg
#* @get /plot
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

#* Plot a histogram
#* @serializer contentType list(type="image/svg+xml")
#* @get /cardioid
function(){
  
  img <- paste0(readLines(con = "cardioid.svg"), collapse = "")
  col <- round(runif(3)*100000)/1000
  img <- gsub("COLRED", col[1], img)
  img <- gsub("COLGRN", col[2], img)
  img <- gsub("COLBLU", col[3], img)
  
  img
  
}

#* Plot a histogram
#* @serializer contentType list(type="image/svg+xml")
#* @param width Width of the outline. Default 2.
#* @param colour Colour of the outline as a hex code. Defaults to random colour.
#* @get /cardioid_outline
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



#* Plot a histogram
#* @serializer png
#* @param colour Colour of the outline as a hex code. Defaults to random colour.
#* @get /png
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
function(max_length = 143){
  
  max_length <- as.integer(max_length)
  
  wisdom <- read_file("https://github.com/merlinmann/wisdom/raw/master/wisdom.md")
  wisdom <- str_extract(wisdom, "The Management(.|\n)*")
  wisdom <- str_split(wisdom, "\n")
  wisdom <- unlist(wisdom)
  wisdom <- str_subset(wisdom, "^- ")
  wisdom <- str_subset(wisdom, "elated:", negate = TRUE)
  wisdom <- str_remove(wisdom, "- ")
  wisdom <- sample(wisdom, 1)
  wisdom_short <- ifelse(nchar(wisdom) > max_length + 3, paste0(str_extract(wisdom, paste0(".{", max_length, "}")), "..."), wisdom)
  wisdom_short_html <- markdownToHTML(text = wisdom_short, fragment.only = TRUE)
  
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
