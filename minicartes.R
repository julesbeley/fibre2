rm(list = ls())
library(ggplot2)
library(httr) 
library(sf)
library(ggmap)
France <- map_data("world", region = "France")

place <- function(search) {
    library(httr)
    library(sf)
    url <- "https://api-adresse.data.gouv.fr/search" 
    query <- GET(url, query = list(q = search, limit = 1))
    geojson <- read_sf(content(query, as = "text"))
    out <- cbind(geojson[, c(1:2)],
                 geojson$geometry[[1]][1],
                 geojson$geometry[[1]][2])
    colnames(out) <- c("label", "score", "long", "lat", "geometry")
    out$geometry <- NULL
    France <- map_data("world", region = "France")
    ggplot() +
        geom_polygon(data = France, 
                     aes(x = long, y = lat, group = group), 
                     fill = NA, 
                     color = "black") +
        theme_void() +
        coord_map(projection = "lambert", 
                  parameters = c(10, 49)) +
        geom_point(data = out,
                   aes(x = long, y = lat)) -> plot
    return(list(out, plot))
}

place("dasle")
