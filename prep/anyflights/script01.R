library(tidyverse)
library(anyflights)

options(timeout = 600)
z <- anyflights("RIC", year = 2019)

for (nom in names(z)) {
  write_csv(z[[nom]], sprintf("../../data/flightsrva_%s.csv.gz", nom))
}