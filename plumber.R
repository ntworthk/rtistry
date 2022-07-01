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
#* @get /cardioid_outline
function(width = 2){
  
  img <- paste0(readLines(con = "cardioid.svg"), collapse = "")
  col <- round(runif(3)*100000)/1000
  stroke <- paste0("stroke:rgb(", col[1], "%,", col[2], "%,", col[3], "%);stroke-width:",width,";")
  img <- gsub("stroke:none;", stroke, img)
  img <- gsub("fill:rgb\\(COLRED%,COLGRN%,COLBLU%\\)", "fill:none", img)
  
  img
  
}



#* Plot a histogram
#* @serializer png
#* @get /png
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
    theme(panel.background = element_blank(), plot.background = element_blank()) +
    theme(
      
    )
  
  print(g)
}

#* Get some wisdom
#* @serializer json
#* @get /wisdom
function(){
  
  wisdom <- read_file("https://github.com/merlinmann/wisdom/raw/master/wisdom.md")
  wisdom <- str_extract(wisdom, "The Management(.|\n)*")
  wisdom <- str_split(wisdom, "\n")
  wisdom <- unlist(wisdom)
  wisdom <- str_subset(wisdom, "^- ")
  wisdom <- str_subset(wisdom, "elated:", negate = TRUE)
  wisdom <- str_remove(wisdom, "- ")
  
  sample(wisdom, 1)
  
}
