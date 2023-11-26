library(tidyverse)
library(stringi)

# read the datasets
comics <- read_csv("~/gh/projects/2022_comics/output/comic_data_20220927.csv.bz2")
panels <- read_csv("~/gh/projects/2022_comics/output/panels_fill.csv.bz2")
captions <- read_csv("~/gh/projects/2022_comics/output/captions.csv.bz2")

# filter
z <- comics %>%
  filter(feature_name %in% c("Peanuts")) %>%
  semi_join(panels, by = "image_path") %>%
  semi_join(captions, by = "image_path") %>%
  select(image_path, feature_name, creator, date, nrow, ncol)

panels <- panels %>%
  semi_join(z, by = "image_path") %>%
  mutate(temp_min = ny - ymax) %>%
  mutate(temp_max = ny - ymin) %>%
  mutate(ymin = temp_min) %>%
  mutate(ymax = temp_max) %>%
  select(-temp_min, -temp_max) %>%
  rename(ncol = nx, nrow = ny)

captions <- captions %>%
  inner_join(unique(select(panels, image_path, nrow, ncol)), by = "image_path") %>%
  mutate(xmin = x1) %>%
  mutate(xmax = x2) %>%
  mutate(temp_min = nrow - y1) %>%
  mutate(temp_max = nrow - y3) %>%
  mutate(ymin = temp_min) %>%
  mutate(ymax = temp_max) %>%
  select(-temp_min, -temp_max) %>%
  mutate(xmid = (xmin + xmax) / 2) %>%
  mutate(ymid = (ymin + ymax) / 2) %>%
  select(image_path, xmid, ymid, xmin_cap = xmin, xmax_cap = xmax, ymin_cap = ymin, ymax_cap = ymax)

panels <- panels %>%
  select(image_path, panel_id, xmin, xmax, ymin, ymax)

# Save output
write_csv(z, "../../data/comics_meta.csv.bz2")
write_csv(panels, "../../data/comics_panels.csv.bz2")
write_csv(captions, "../../data/comics_captions.csv.bz2")
