library(tidyverse)
library(lubridate)

demo <- read_csv("data/P_DEMO.csv.bz2")

demo <- filter(demo, !is.na(RIDAGEYR), RIDAGEYR < 80)
demo <- filter(demo, RIDSTATR == 2)

demo$id <- demo$SEQN
demo$gender <- if_else(demo$RIAGENDR == 1, "Male", "Female")
demo$age <- demo$RIDAGEYR
demo$edu_level <- demo$DMDEDUC2
demo$race <- c("Mexican American", "Other Hispanic", "White", "Black", "Asian", "Other", "Missing")[demo$RIDRETH3]
demo$family_status <- if_else(demo$DMDMARTZ == 1, "Living with Partner", "Other")
demo$ratio_to_poverty <- demo$INDFMPIR
demo$lang_interview <- if_else(demo$SIALANG == 1, "English", "Spanish")
demo <- select(demo, id, age, gender, edu_level, race, family_status, ratio_to_poverty, lang_interview)
write_csv(demo, "../../data/wweia_demo.csv.bz2")

food <- read_csv("data/P_DR1IFF.csv.bz2")
food$id <- food$SEQN
food <- semi_join(food, demo, by = "id")
food <- filter(food, DR1DRSTZ == 1)

food <- select(
  food, id,
  food_code = DR1IFDCD,
  #food_comp = DR1ILINE, food_comb = DR1CCMTX,
  day_of_week = DR1DAY, time = DR1_020, meal_name = DR1_030Z,
  food_source = DR1FS, at_home = DR1_040Z, grams = DR1IGRMS,
  kcal = DR1IKCAL, protein = DR1IPROT, carbs = DR1ICARB,
  sugar = DR1ISUGR, fat = DR1ITFAT, caffeine = DR1ICAFF,
  alcohol = DR1IALCO
)

food$time <- hour(food$time)
food$day_of_week[food$day_of_week == 1] <- 8
food$day_of_week <- food$day_of_week - 1
food$at_home <- if_else(food$at_home == 1, "Yes", "No") 

food$meal_name[food$meal_name > 19] <- 20
food$meal_name <- c("Breakfast", "Lunch", "Dinner", "Supper", "Brunch", "Snack",
"Drink", "Infant feeding", "Extended consumption", "Desayano",
"Almuerzo", "Comida", "Merienda", "Cena", "Entre comida", "Botana",
"Bocadillo", "Tentempie", "Bebida", "Other")[food$meal_name]

food$food_source[food$food_source > 28] <- 29
food$food_source <- c("Store - grocery/supermarket", "Restaurant with waiter/waitress",
"Restaurant fast food/pizza", "Bar/tavern/lounge", "Restaurant no additional information",
"Cafeteria NOT in a K-12 school", "Cafeteria in a K-12 school",
"Child/Adult care center", "Child/Adult home care", "Soup kitchen/shelter/food pantry",
"Meals on Wheels", "Community food program - other", "Community program no additional information",
"Vending machine", "Common coffee pot or snack tray", "From someone else/gift",
"Mail order purchase", "Residential dining facility", "Grown or caught by you or someone you know",
"Fish caught by you or someone you know", "", "", "", "Sport, recreation, or entertainment facility",
"Street vendor, vending truck", "Fundraiser sales", "Store - convenience type",
"Store - no additional info", "Other, specify", "Don't know")[food$food_source]
write_csv(food, "../../data/wweia_food.csv.bz2")


cw <- read_csv("data/cw.csv")
write_csv(cw, "../../data/wweia_meta.csv.bz2")
