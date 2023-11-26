http_cache_get <- function(url, cache_dir, force = FALSE)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)
  
  # create a cache of the query
  cache_file <- file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  # check if file exists and either load or query and save
  if (file.exists(cache_file) & !force)
  {
    res <- readRDS(cache_file)
  } else {
    res <- httr::GET(url)
    saveRDS(res, cache_file)
    system("sleep 1")
  }
  
  return(res)
}

get_common_names <- function(these_names)
{
  these_names_set <- unique(these_names)
  these_names_set_common <- rep("", length(these_names_set))
  for (j in seq_along(these_names_set))
  try({
    url <- sprintf("https://en.wikipedia.org/wiki/%s", these_names_set[j])
    res <- http_cache_get(url, "cache")
    obj <- httr::content(res, type = "text/html", encoding = "UTF-8")
    these_names_set_common[j] <- xml_text(xml_find_all(obj, "..//span[@class='mw-page-title-main']"))
  })

  index <- match(these_names, these_names_set)
  these_names_set_common <- stri_replace_all(these_names_set_common, "", fixed = "(bird)")
  output <- these_names_set_common[index]
  output[is.na(output)] <- ""
  output[output == these_names] <- ""
  return(output)  
}

get_iucnredlist <- function(these_names)
{
  link <- rep("", length(these_names))
  for (j in seq_along(these_names))
  try({
    url <- sprintf("https://en.wikipedia.org/wiki/%s", these_names[j])
    res <- http_cache_get(url, "cache")
    obj <- httr::content(res, type = "text/html", encoding = "UTF-8")
    these <- xml_attr(xml_find_all(obj, "..//a"), "href")
    these <- these[!is.na(these)]
    these <- these[stri_sub(these, 1L, 29L) == "https://apiv3.iucnredlist.org"]
    link[j] <- these[1]
  })

  link[these_names == "Saxicola_rubicola"] <- "https://www.iucnredlist.org/species/22710184/181614254"
  for (j in seq_along(these_names))
  try({
    res <- http_cache_get(link[j], "cache")
    obj <- httr::content(res, type = "text/html", encoding = "UTF-8")

  })
}


library(tidyverse)
library(stringi)
library(writexl)
library(xml2)

species <- read_csv("../../data/french_bird_species.csv.bz2")
species_name <- stri_extract(species$scientific_name, regex = "[A-Za-z]+ [a-z]+")
species_name <- stri_replace_all(species_name, "_", fixed = " ")

#class_common <- get_common_names(species$class)
order_common <- get_common_names(species$order)
#family_common <- get_common_names(species$family)
#genus_common <- get_common_names(species$genus)
species_common <- get_common_names(species_name)

# manual changes
order_common[order_common == "Passerine"] <- "Sparrow"
order_common[order_common == "Columbidae"] <- "Pigeon"
order_common[species$order == "Charadriiformes"] <- "Shorebird"
order_common[species$order == "Anseriformes"] <- "Waterfowl"
order_common[species$order == "Accipitriformes"] <- "Hawk"
order_common[species$order == "Pelecaniformes"] <- "Pelican"
order_common[species$order == "Galliformes"] <- "Landfowl"
order_common[species$order == "Piciformes"] <- "Woodpecker"
order_common[species$order == "Gruiformes"] <- "Crane"
order_common[species$order == "Falconiformes"] <- "Falcon"
order_common[species$order == "Procellariiformes"] <- "Seabird"
order_common[species$order == "Apodiformes"] <- "Hummingbird"
order_common[species$order == "Suliformes"] <- "Pelican"
order_common[species$order == "Gaviiformes"] <- "Loon"
order_common[species$order == "Coraciiformes"] <- "Kingfisher"
order_common[species$order == "Phoenicopteriformes"] <- "Flamingo"
order_common[species$order == "Bucerotiformes"] <- "Hornbill"

species$species_common <- species_common
species$order_common <- order_common
species <- select(species, scientific_name, species_common, order_common, everything())

write_csv(species, "../../data/french_bird_species.csv.bz2")






