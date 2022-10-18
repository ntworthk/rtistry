#--- Script details ------------------------------------------------------------
# Creation date: 13 October 2022
# Description:   Generate maps
# Author:        Nick Twort

library(tidyverse)
library(magrittr)
library(sf)
library(osmdata)
library(rsvg)
library(digest)

get_roads <- function(city) {
  
  suppressWarnings(
    opq(st_bbox(city)) |> 
      add_osm_feature("highway") |>
      osmdata_sf() |>
      extract2("osm_lines") |>
      select(osm_id, name, highway) |>
      as_tibble() |>
      st_sf() |>
      st_set_crs(4326) |>
      st_intersection(city)
  )
  
}

set_road_width <- function(roads) {
  
  roads |> 
    mutate(size = case_when(
      highway %in% c("motorway", "trunk", "motorway_link", "trunk_link") ~ 0.25,
      highway %in% c("primary", "secondary", "primary_link", "secondary_link") ~ 0.18,
      highway %in% c("tertiary", "unclassified", "residential", "service", "living_street", "tertiary_link", "track", "platform") ~ 0.12,
      highway %in% c("footway", "cycleway", "steps", "pedestrian", "construction", "path", "corridor") ~ 0.05,
      TRUE ~ 0
    )) |> 
    filter(size != 0)
  
}

buffer_point <- function(point, country, radius) {
  
  espg <- c("AUS" = 3577, "NZL" = 27200)
  
  if (!country %in% names(espg)) {
    stop("Error - country not coded")
  }
  
  to_unit <- espg[[country]]
  
  point |> 
    st_transform(to_unit) |> 
    st_buffer(radius) |> 
    st_transform(4326)
  
}

generate_radius_map <- function(coords, title, country, radius = 5000, base_size = 16, theme = "darkmode", quiet = FALSE) {
  
  # Assume wrong way around if so - eg if copied from Google Maps
  if (coords[1] < 0) {
    coords <- rev(coords)
  }
  
  centre_point <- st_sfc(st_point(coords), crs = 4326)
  
  centre_area <- buffer_point(centre_point, country, radius)
  
  point_size <- 100
  
  centre_point_circle <- buffer_point(centre_point, country, point_size)
  centre_point_circle_outline <- buffer_point(centre_point, country, point_size * 1.2)
  centre_point_circle_outline_outline <- buffer_point(centre_point, country, point_size * 1.25)
  
  if (!quiet) {
    cat("Getting road data...\n")
  }
  # Check if already created
  hash <- paste0("hash_", digest(centre_area))
  if (file.exists(hash)) {
    cat("Already downloaded the deta...loading from cache\n")
    roads <- read_rds(hash)
  } else {
    roads <- get_roads(centre_area) |> 
      set_road_width()
    cat("Saving the data to cache for reuse\n")
    write_rds(roads, hash)
  }
  
  if (!quiet) {
    cat("Got road data...creating map\n\n")
  }
  
  background_colour <- ifelse(theme == "darkmode", "black", "white")
  foreground_colour <- ifelse(theme == "darkmode", "white", "black")
  
  ggplot() +
    geom_sf(data = roads, aes(size = size), colour = foreground_colour) +
    geom_sf(data = centre_area, fill = NA, colour = foreground_colour, size = 0.2) +
    geom_sf(data = centre_point_circle_outline_outline,
            fill = foreground_colour, colour = foreground_colour,
            size = 0.2) +
    geom_sf(data = centre_point_circle_outline,
            fill = background_colour, colour = background_colour,
            size = 0.2) +
    geom_sf(data = centre_point_circle,
            fill = foreground_colour, colour = foreground_colour,
            size = 0.2) +
    scale_size_identity() +
    labs(
      title = title
    ) +
    theme_void(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = background_colour, colour = NA),
      plot.background = element_rect(fill = background_colour, colour = NA),
      text = element_text(colour = foreground_colour),
      plot.title = element_text(family = "Goldenbook", hjust = 0.5)
    )
  
}

export <- function(g, w = 10, h = 10, name = NULL) {
  
  if (is.null(name)) {
    name <- g$labels$title
  }
  
  ggsave(filename = glue::glue("{name}.png"),
         plot = g,
         width = w,
         height = h,
         dpi = 300,
         units = "cm",
         bg = "transparent"
  )
  ggsave(filename = glue::glue("{name}.svg"),
         plot = g,
         width = w,
         height = h,
         dpi = 300,
         units = "cm",
         bg = "transparent"
  )
  rsvg_pdf(glue::glue("{name}.svg"), glue::glue("{name}.pdf"))
  
  invisible(TRUE)
  
}


details <- tibble(
  coords = list(c(-37.082, 174.962),
                c(-36.884, 174.723),
                c(-33.887, 151.140)),
  title = c("PAPAKURA",
            "MOUNT ALBERT",
            "HOME"),
  country = c("NZL", "NZL", "AUS")
)

graphs <- pmap(details, generate_radius_map, theme = "light", base_size = 50)

walk(graphs, export, w = 42.0, h = 59.4)


