library(tidyverse)
library(lubridate)
library(sf)

spatial_join <- function(...) {
  return(st_as_sf(as_tibble(st_join(..., left = FALSE))))
}


################################################################
# Melospiza melodia in the US
z <- read_delim("input/0135414-220831081235567.csv", delim = "\t")

# take subset of the variables
z$date <- make_date(z$year, z$month, z$day)
z <- filter(z, stateProvince %in% c("Virginia"))
x <- select(z, date, lon = decimalLongitude, lat = decimalLatitude, count = individualCount)
x <- na.omit(x)
x$amount <- if_else(x$count == 1, "single", "pair")
x$amount[x$count > 2] <- "group"
x <- select(x, -count)

# save
write_csv(x, "../../data/melospiza_melodia_us.csv.bz2")

################################################################
# French bird data
cb <- function(z, pos)
{
  # take subset of the variables
  z$date <- make_date(z$year, z$month, z$day)
  x <- select(z, date, lon = decimalLongitude, lat = decimalLatitude, 
                 # kingdom, phylum, class,
                 # order, family, genus, species,
                 scientific_name = scientificName)
  x <- na.omit(x)
  x
}

z <- read_delim_chunked(
  "input/occurrence.txt",
  DataFrameCallback$new(cb),
  delim = "\t",
  chunk_size = 100000
)

################################################################
# Species Data
species <- read_delim("input/0135402-220831081235567.csv", delim = "\t")
species <- select(species, scientific_name = scientificName,
  kingdom, phylum, class, order, family, genus, species,
  iucn = iucnRedListCategory
)

# only keep birds/species in the other set
species <- species %>%
  semi_join(z, by = "scientific_name") %>%
  filter(!duplicated(scientific_name))
species <- na.omit(species)
z <- z %>%
  semi_join(species, by = "scientific_name")

# only keep birds that are seen on land
d <- distinct(z, lon, lat)
d <- d %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
france <- read_sf("../../data/france_departement.geojson")
d <- d %>% spatial_join(france)
z <- z %>%
  semi_join(d, by = c("lon", "lat"))

# only keep the most common birds
tabs <- z %>%
  group_by(scientific_name) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
tabs <- slice_head(tabs, n = 100)

set.seed(1)
x <- z %>%
  filter(!(month(date) == 1 & day(date) == 1)) %>%
  semi_join(tabs, by = "scientific_name") %>%
  slice_sample(n = 100000) %>%
  ungroup()
x2 <- species %>% semi_join(x, by = "scientific_name")

# order the data
x <- arrange(x, date, scientific_name, lon, lat)
x2 <- arrange(x2, scientific_name)

write_csv(x, "../../data/french_birds.csv.bz2")
write_csv(x2, "../../data/french_bird_species.csv.bz2")

