library(readr)
library(purrr)
library(tibble)
library(here)
setwd(here())

files <- Sys.glob("data-raw/JHU/*.csv")

jhu_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, '/'),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) readr::read_csv(filename))
)

save(jhu_data, file = 'data/jhu_data.rdata')
