## code to prepare `us_deaths` dataset goes here

library(readr)
library(dplyr)

us_deaths <- read_tsv("data-raw/Multiple Cause of Death, 1999-2020.txt",
                      col_types = cols_only(
                        Gender = col_factor(),
                        `Ten-Year Age Groups Code` = col_factor(),
                        Year = col_integer(),
                        Deaths = col_number(),
                        Population = col_number()),
                      n_max = 140L) |>
  rename(gender = Gender,
         age_group = `Ten-Year Age Groups Code`,
         year = Year,
         deaths = Deaths,
         population = Population) |>
  mutate(qx = deaths / population)

usethis::use_data(us_deaths, overwrite = TRUE)
