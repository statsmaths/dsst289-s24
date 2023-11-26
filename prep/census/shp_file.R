library(tidyverse)
library(rmapshaper)
library(sf)

# mkdir temp; cd temp
# curl -O https://www2.census.gov/geo/tiger/TIGER2022/STATE/tl_2022_us_state.zip
# unzip tl_2022_us_state.zip
# cd ..

simplify_shape <- function(shp_obj, crs, plot_name = NULL)
{
  # Transform to desired CRS
  shp_simple <- st_transform(shp_obj, crs)

  # Remove small parts of polygons (islands)
  num_polygon <- sapply(st_geometry(shp_simple), length)
  for ( i in which(num_polygon > 1) )
  {
    z <- st_cast(shp_simple$geometry[i], "POLYGON")
    areas <- as.numeric(st_area(z)) / 10000
    index <- which((areas >= 0.05 * sum(areas)) | (areas == max(areas)))
    z <- z[index]
    shp_simple$geometry[i] <- st_union(z)
  }

  # Simplify and validate
  shp_simple <- rmapshaper::ms_simplify(shp_simple, keep = 0.03)
  shp_simple <- st_make_valid(shp_simple)

  # Plot to verify
  if (!is.null(plot_name))
  {
    dir.create("temp", FALSE)
    p <- ggplot(shp_simple) + geom_sf(size = 0.1) + theme_void()
    ggsave(file.path("temp", sprintf("%s.png", plot_name)), p)
  }

  # Back to Lat/Lon
  shp_simple <- st_transform(shp_simple, 4326)

  return(shp_simple)
}

shp <- read_sf("temp/tl_2022_us_state.shp")
shp_sml <- simplify_shape(shp, crs = 4326)

names(shp_sml) <- tolower(names(shp_sml))
shp_sml <- filter(shp_sml, !(name %in% c("Commonwealth of the Northern Mariana Islands",
  "Guam", "American Samoa", "United States Virgin Islands")))
shp_sml <- arrange(shp_sml, stusps)

# test
# ggplot(shp_sml) + geom_sf() + theme_minimal()

file.remove("../../data/us_state_dcpr.geojson")
write_sf(shp_sml, "../../data/us_state_dcpr.geojson")


