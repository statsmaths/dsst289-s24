library(tidyverse)
library(stringi)

# http://www.doug-webber.com/data.html

z <- read_delim("input_data_utf8.csv", delim = "\t")
z <- pivot_longer(z, -major, names_to = "percentile")
z <- filter(z, major != "Number of Records")
z <- mutate(z, percentile = as.numeric(percentile))
z <- mutate(z, earnings = value / 1e6)
z <- select(z, -value)

ggplot(z, aes(percentile, earnings)) + geom_line(aes(color = major), show.legend = FALSE)

# just a quick EDA
engin <- filter(z, major == "Chemical Engineering", percentile == 33)

z %>%
  filter(percentile == 96) %>%
  arrange(earnings)

z %>%
  filter(earnings > 3.67) %>%
  group_by(major) %>%
  summarize(percentile_min = min(percentile)) %>%
  arrange(desc(percentile_min)) %>%
  print(n = Inf)


engin <- filter(z, major == "Accounting", percentile == 50)

z %>%
  filter(earnings > 3.01) %>%
  group_by(major) %>%
  summarize(percentile_min = min(percentile)) %>%
  arrange(desc(percentile_min)) %>%
  print(n = Inf)