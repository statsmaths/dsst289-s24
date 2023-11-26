library(tidyverse)
library(stringi)
library(readxl)
library(lubridate)

# https://datadryad.org/stash/dataset/doi:10.5061/dryad.8rr0498

# flight_call => short, high frequency vocalizations characteristic
# of birds in sustained flight
# Load Chicago Species Data
x <- read_lines("chicago_species.txt")
x <- stri_split(x, fixed = " ")
df1 <- tibble(
  state = "IL",
  family = map_chr(x, ~..1[[3]]),
  genus = map_chr(x, ~..1[[1]]),
  species = stri_trans_tolower(map_chr(x, ~..1[[2]])),
  collisions = stri_trans_tolower(map_chr(x, ~..1[[4]])),
  flight_call = stri_trans_tolower(map_chr(x, ~..1[[5]])),
  habitat = stri_trans_tolower(map_chr(x, ~..1[[6]])),
  stratum = stri_trans_tolower(map_chr(x, ~..1[[7]])),
)

# Load Cleveland Collision Data
x <- read_lines("clevelend_species.txt")
x <- stri_split(x, fixed = " ")
df2 <- tibble(
  state = "OH",
  species = stri_trans_tolower(map_chr(x, ~..1[[2]])),
  genus = map_chr(x, ~..1[[1]]),
  family = map_chr(x, ~..1[[3]]),
  collisions = stri_trans_tolower(map_chr(x, ~..1[[4]])),
  flight_call = stri_trans_tolower(map_chr(x, ~..1[[5]])),
  habitat = stri_trans_tolower(map_chr(x, ~..1[[6]])),
  stratum = stri_trans_tolower(map_chr(x, ~..1[[7]])),
)
df <- bind_rows(df1, df2)
df <- select(df, -collisions)


chicago <- read_csv("Chicago_collision_data.csv")
names(chicago) <- stri_trans_tolower(names(chicago))
chicago$state <- "IL"
cleveland <- read_csv("Cleveland_collision_data.csv")
names(cleveland) <- stri_trans_tolower(names(cleveland))
cleveland$date <- mdy(cleveland$date)
cleveland$state <- "OH"
light <- read_csv("Light_levels_dryad.csv")
names(light) <- stri_trans_tolower(names(light))

z <- read_excel("PopEsts ProvState 2021.02.05.xlsx")
names(z) <- stri_trans_tolower(names(z))
names(z) <- stri_replace_all(names(z), "", regex = "[^a-z ]+")
names(z) <- stri_replace_all(names(z), "_", regex = "[ ]+")
z <- z[,!duplicated(names(z))]
z <- filter(z, province_state_territory %in% c("IL", "OH"))
z$genus <- map_chr(stri_split(z$scientific_name, fixed = " "), ~..1[[1]])
z$species <- map_chr(stri_split(z$scientific_name, fixed = " "), ~..1[[2]])
z <- select(z, state = province_state_territory, species, genus, english_name, population_estimate)
z <- filter(z, !is.na(population_estimate))

# Actually, remove OH for now
z <- select(filter(z, state == "IL"), -state)
chicago <- select(filter(chicago, locality == "MP"), -state, -locality)
chicago <- chicago %>% semi_join(z)
chicago <- chicago %>% semi_join(light)
light <- light %>% semi_join(chicago)
z <- z %>% semi_join(chicago)
df <- select(filter(df, state == "IL"), -state)
df <- df %>% semi_join(z)
df <- group_by(select(df, -species), genus) %>% slice_head(n = 1) %>% ungroup()

chicago$year <- year(chicago$date)
chicago$month <- month(chicago$date)

chicago$species <- sprintf("%s. %s", stri_sub(chicago$genus, 1, 1), chicago$species)
z$species <- sprintf("%s. %s", stri_sub(z$genus, 1, 1), z$species)
chicago <- select(chicago, species, genus, date, year, month)
df <- select(df, genus, family, flight_call, habitat, stratum)

light$light_level <- if_else(light$light_score < 14, "low", "high")

# Save output
write_csv(chicago, "../../data/bird_coll_chicago.csv.bz2")
write_csv(light, "../../data/bird_coll_light.csv.bz2")
write_csv(z, "../../data/bird_coll_species.csv.bz2")
write_csv(df, "../../data/bird_coll_genus.csv.bz2")
