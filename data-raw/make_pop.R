# make NE population data for package
library(dplyr)
library(readr)
census_conus <- read_csv(here::here("data-raw/census_2000-2019.csv")) %>% 
  tidyr::separate("location", into = c("State", "County"), sep = "-") %>% # runs with a warning because miami-dade
  mutate(County = trimws(County),
         County = gsub(" ", "",County))
county_area <- read_csv(here::here("data-raw/NE_areas.csv")) %>% 
  rename(County = NAME)
NE_county_pops <- census_conus %>% 
  filter(State == "Nebraska") %>% 
  left_join(county_area, by = "County") %>% 
  mutate(density = POP / area,
         pop100K = POP / 1e5) %>% 
  select(County, fips, year, pop100K, density)

usethis::use_data(NE_county_pops, overwrite = TRUE)
