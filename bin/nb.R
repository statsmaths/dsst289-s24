suppressPackageStartupMessages(library(tidyverse))
library(stringi)

args <- commandArgs(trailingOnly = TRUE)

if (args[1] == "ALL") {
  files <- dir("src")
  files <- files[stri_sub(files, 1, 8) == "notebook"]
  files <- as.numeric(stri_sub(files, 9, 10))
  args <- files
}

trim_chr <- function(v)
{
  index <- cumsum(v != "")
  v <- v[index > 0]
  index <- rev(cumsum(rev(v) != ""))
  v <- v[index > 0]
  v
}

args <- as.numeric(args)
for (ar in args)
{
  cat(sprintf("NOTEBOOK %02d\n", ar))

  # file names
  fin <- sprintf("notebook%02d_solutions.Rmd", ar)
  nout <- sprintf("notebook%02d.Rmd", ar)
  sout <- sprintf("notebook%02d_solutions.html", ar)

  # read the file
  x <- read_lines(file.path("src", fin))

  # replace the answers flags and the name in the header
  x[stri_sub(x, 1L, 11L) == "**Answer**:"] <- "**Answer**:"
  x <- stri_replace_all(x, "", fixed = " -- Solutions")

  # find the start and end of the questions that need to be cleared
  qstart <- which(stri_detect(x, regex = 'question-[0-9]+'))
  qnums <- stri_sub(stri_extract(x[qstart], regex = 'question-[0-9]+'), 10L, 11L)
  qend <- which(x == "```")
  qend <- map_int(qstart, ~ qend[which(..1 < qend)[1]])
  index <- flatten_int(map2(qstart, qend, ~ seq(..1 + 1L, ..2 - 1L)))
  x[index] <- ""

  # reformat
  x <- stri_paste(x, collapse = "\n")
  x <- stri_replace_all(x, "\n\n", regex = "\n[\n]+")

  # write the output
  write_lines(x, file.path("nb", nout))

  # make the solutions
  x <- read_lines(file.path("src", fin))
  qstart <- which(stri_detect(x, regex = 'question-[0-9]+'))
  qnums <- stri_sub(stri_extract(x[qstart], regex = 'question-[0-9]+'), 10L, 11L)
  x[qstart] <- sprintf("%s\n# Question %s", x[qstart], qnums)
  write_lines(x, 'src/temp.Rmd')

  system(
    "R -e \"rmarkdown::render('src/temp.Rmd')\"",
    ignore.stdout=TRUE,
    ignore.stderr=TRUE
  )
  file.rename(
    file.path("src", "temp.html"),
    file.path("stage", sout)
  )
  file.remove("src/temp.Rmd")
}
