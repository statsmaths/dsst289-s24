library(tidyverse)
library(stringi)
library(xml2)

plays <- file.path("xml", dir("xml"))

df_people <- vector("list", length(plays))
df_aliases <- vector("list", length(plays))
df_lines <- vector("list", length(plays))
df_sdall <- vector("list", length(plays))

for (j in seq_along(plays))
{
  obj <- read_xml(plays[j])
  play <- xml_attr(xml_find_first(obj, "title"), "short")
  play <- if_else(play == "Ricahrd III", "Richard III", play)

  people <- xml_find_all(obj, "..//persona")
  df_people[[j]] <- tibble(
    play = play,
    full_name = xml_text(xml_find_first(people, "./persname")),
    short_name = xml_attr(xml_find_first(people, "./persname"), "short"),
    gender = xml_attr(people, "gender")
  )

  aliases <- map(people, ~ xml_attr(xml_find_all(..1, "./persaliases/persname"), "short"))
  df_aliases[[j]] <- tibble(
    play = play,
    full_name = rep(df_people[[j]]$full_name, map_int(aliases, length)),
    alias = flatten_chr(aliases)
  )

  acts <- xml_find_all(obj, ".//act")

  df <- NULL
  df_sd <- NULL
  for (i in seq_along(acts)) {
    scenes <- xml_find_all(acts[i], ".//scene")
    for (k in seq_along(scenes)) {
      speech <- xml_find_all(scenes[k], ".//speech")
      speaker <- xml_text(xml_find_first(speech, "speaker"))
      line <- map(speech, ~ xml_find_all(..1, "line"))
      sdir <- xml_find_all(scenes[k], ".//stagedir")

      df <- c(df, list(tibble(
        play = play,
        act = xml_attr(acts[i], "num"),
        scene = xml_attr(scenes[k], "num"),
        alias = rep(speaker, map_int(line, length)),
        globalnumber = flatten_chr(map(line, ~ xml_attr(..1, "globalnumber"))),
        number = flatten_chr(map(line, ~ xml_attr(..1, "number"))),
        text = flatten_chr(map(line, ~ xml_text(..1)))
      )))

      if (length(sdir))
      {
        df_sd <- c(df_sd, list(tibble(
          play = play,
          act = xml_attr(acts[i], "num"),
          scene = xml_attr(scenes[k], "num"),
          sdglobalnumber = flatten_chr(map(sdir, ~ xml_attr(..1, "sdglobalnumber"))),
          sdnumber = flatten_chr(map(sdir, ~ xml_attr(..1, "sdnumber"))),
          direction = flatten_chr(map(sdir, ~ xml_text(..1)))
        )))
      }      
    }
  }

  df_lines[[j]] <- bind_rows(df)
  df_sdall[[j]] <- bind_rows(df_sd)
}

df_people <- bind_rows(df_people)
df_aliases <- bind_rows(df_aliases)
df_lines <- bind_rows(df_lines)
df_sdall <- bind_rows(df_sdall)

# df_lines %>%
#   anti_join(df_aliases, by = c("play", "alias")) %>%
#   group_by(play, alias) %>%
#   summarize(n = n()) %>%
#   print(n = Inf)

# Combine the alias information
df_lines <- df_lines %>%
  inner_join(df_aliases, by = c("play", "alias")) %>%
  mutate(act_scene = sprintf("%s.%s", act, scene)) %>%
  select(play, act, scene, act_scene, globalnumber, number, full_name, text)

# Text conversion
df_lines$text <- stri_trans_general(df_lines$text, 'latin-ascii')
df_lines$text <- stri_replace_all(df_lines$text, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")
df_lines$play <- stri_trans_general(df_lines$play, 'latin-ascii')
df_lines$play <- stri_replace_all(df_lines$play, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")
df_lines$full_name <- stri_trans_general(df_lines$full_name, 'latin-ascii')
df_lines$full_name <- stri_replace_all(df_lines$full_name, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")

# Text conversion
df_people$play <- stri_trans_general(df_people$play, 'latin-ascii')
df_people$play <- stri_replace_all(df_people$play, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")
df_people$full_name <- stri_trans_general(df_people$full_name, 'latin-ascii')
df_people$full_name <- stri_replace_all(df_people$full_name, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")
df_people$short_name <- stri_trans_general(df_people$short_name, 'latin-ascii')
df_people$short_name <- stri_replace_all(df_people$short_name, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")

# Text conversion
df_sdall$play <- stri_trans_general(df_sdall$play, 'latin-ascii')
df_sdall$play <- stri_replace_all(df_sdall$play, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")
df_sdall$direction <- stri_trans_general(df_sdall$direction, 'latin-ascii')
df_sdall$direction <- stri_replace_all(df_sdall$direction, '', regex = "[^\u0001-\u007F]+|<U\\+\\w+>")

# Add speech id
df_lines$speech_id <- NA_integer_
for (play in df_lines$play)
{
  these <- which(df_lines$play == play)
  df_lines$speech_id[these] <- cumsum(
    df_lines$full_name[these] != lag(df_lines$full_name[these], n = 1, default = "")
  )
}

# Save output
write_csv(df_lines, "../../data/shakespeare_lines.csv.bz2")
write_csv(df_people, "../../data/shakespeare_people.csv.bz2")
write_csv(df_sdall, "../../data/shakespeare_sd.csv.bz2")
write_csv(unique(select(df_lines, play)), "move_me.csv")
