
# US Census Data Collection -----------------------------------------------


library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

acs_vars <- c(
  pop           = "B01003_001",
  median_hhinc  = "B19013_001",
  poverty       = "B17001_002",
  pop_poverty   = "B17001_001",
  unemp         = "B23025_005",
  laborforce    = "B23025_002",
  median_rent   = "B25064_001",
  median_home   = "B25077_001",
  no_hs         = "B15003_002",
  bachelors     = "B15003_022",
  pop_25plus    = "B15003_001",
  white         = "B03002_003",
  black         = "B03002_004",
  hispanic      = "B03002_012",
  asian         = "B03002_006",
  snap          = "B22010_002",
  snap_total    = "B22010_001",
  gini          = "B19083_001",
  foreign_born  = "B05002_013",
  no_health_ins = "B27010_033"
)

nyc_puma_filter <- function(df) {
  df |>
    filter(substr(GEOID, 1, 2) == "36")
}

get_valid_acs_vars <- function(year, survey, vars) {
  v <- load_variables(year, survey, cache = TRUE)
  
  vars[names(vars)][vars %in% v$name]
}

get_acs_safe <- function(year, survey, vars) {
  valid_vars <- get_valid_acs_vars(year, survey, vars)
  
  dropped <- setdiff(vars, valid_vars)
  if (length(dropped) > 0) {
    message("Year ", year, " ", survey, " dropped vars: ",
            paste(names(vars)[vars %in% dropped], collapse = ", "))
  }
  
  get_acs(
    geography = "public use microdata area",
    variables = valid_vars,
    state = "NY",
    year = year,
    survey = survey,
    output = "wide"
  ) |>
    nyc_puma_filter() |>
    mutate(year = year, survey = survey)
}

acs5_years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015,
                2016, 2017, 2018, 2019, 2021, 2022, 2023)

acs5_data <- map_dfr(acs5_years, \(yr) {
  message("Downloading ACS5 ", yr)
  get_acs_safe(yr, "acs5", acs_vars)
})

acs1_years <- c(2005, 2006, 2007, 2008)

acs1_data <- map_dfr(acs1_years, \(yr) {
  message("Downloading ACS1 ", yr)
  tryCatch(
    get_acs_safe(yr, "acs1", acs_vars),
    error = function(e) {
      message("Skipped ", yr, ": ", e$message)
      NULL
    }
  )
})


acs1_data <- acs1_data |> 
  relocate(year)
acs5_data <- acs5_data |> 
  relocate(year)

acs1_data |> 
  write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs1-data.csv')
acs5_data |> 
  write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs5-data.csv')

rm(acs1_data, acs5_data)

acs1 <- read_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs1-data.csv')
acs5 <- read_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs5-data.csv')

acs_all <- bind_rows(acs1, acs5)



# Arrest Data -------------------------------------------------------------

library(tidyverse)
library(sf)
library(tigris)
library(lubridate)

options(tigris_use_cache = TRUE)

# ACS files
acs1 <- read_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs1-data.csv') |>
  mutate(source = "acs1")

acs5 <- read_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/acs5-data.csv') |>
  mutate(source = "acs5")

acs_all <- bind_rows(
  acs1 |> filter(year <= 2008),
  acs5 |> filter(year >= 2009)
) |>
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  )

acs_all |>
  count(GEOID, year) |>
  filter(n > 1)

# Arrest points
arrests_sf <- arrests |>
  filter(!is.na(Lon_Lat)) |>
  mutate(Lon_Lat = str_squish(Lon_Lat)) |>
  st_as_sf(wkt = "Lon_Lat", crs = 4326)

# PUMA polygons
puma_sf <- pumas(state = "NY", year = 2020, cb = TRUE) |>
  st_transform(4326) |>
  rename(
    GEOID = GEOID20,
    PUMACE = PUMACE20,
    NAME = NAMELSAD20
  ) |>
  select(GEOID, PUMACE, NAME, geometry)

# Spatial join + ACS merge
arrests_acs <- arrests_sf |>
  st_join(puma_sf, join = st_within, left = TRUE) |>
  mutate(
    GEOID = as.character(GEOID),
    arrest_year = lubridate::year(lubridate::mdy(ARREST_DATE))
  ) |>
  left_join(
    acs_all,
    by = c("GEOID", "arrest_year" = "year")
  )




# overdose ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(tidyr)

# ------------------------------------------------------------
# 1. Read and stack yearly overdose xlsx files
# ------------------------------------------------------------
od_dir <- '/Users/bchoe/My Drive/suny-geneseo/spring2026/data-overdose'

od_files <- list.files(
  path = od_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

read_overdose_file <- function(path) {
  yr <- str_extract(basename(path), "\\d{4}") |> as.integer()
  
  x <- read_excel(path) |>
    clean_names() |>
    rename(cd_raw = community_district) |>
    mutate(
      cd_raw = str_squish(cd_raw),
      year = yr
    )
  
  borough_headers <- c("MANHATTAN", "BRONX", "BROOKLYN", "QUEENS", "STATEN ISLAND")
  
  x |>
    mutate(
      borough_header = cd_raw %in% borough_headers,
      boro_name = case_when(
        cd_raw == "MANHATTAN" ~ "Manhattan",
        cd_raw == "BRONX" ~ "Bronx",
        cd_raw == "BROOKLYN" ~ "Brooklyn",
        cd_raw == "QUEENS" ~ "Queens",
        cd_raw == "STATEN ISLAND" ~ "Staten Island",
        TRUE ~ NA_character_
      )
    ) |>
    fill(boro_name, .direction = "down") |>
    filter(!borough_header) |>
    mutate(
      cd_raw = na_if(cd_raw, ""),
      cd_raw = str_squish(cd_raw)
    ) |>
    filter(!is.na(cd_raw)) |>
    group_by(year, boro_name) |>
    mutate(cd_num = row_number()) |>
    ungroup() |>
    mutate(
      boro_cd = case_when(
        boro_name == "Manhattan"     ~ 100L + cd_num,
        boro_name == "Bronx"         ~ 200L + cd_num,
        boro_name == "Brooklyn"      ~ 300L + cd_num,
        boro_name == "Queens"        ~ 400L + cd_num,
        boro_name == "Staten Island" ~ 500L + cd_num
      )
    )
}

overdose_cd <- map_dfr(od_files, read_overdose_file)

# quick check
overdose_cd |>
  count(year, boro_name)

overdose_cd |>
  count(boro_cd, year) |>
  filter(n > 1) |>
  arrange(year, boro_cd)

overdose_cd_one <- overdose_cd |>
  distinct(boro_cd, year, .keep_all = TRUE) |> 
  select(
    boro_cd,
    year,
    contains("substance")
  )

# ------------------------------------------------------------
# 2. Join with arrests_acs
# ------------------------------------------------------------


cd_sf <- cd_sf |>
  mutate(
    boro_cd = as.integer(boro_cd),
    boro_name = case_when(
      boro_cd %/% 100 == 1 ~ "Manhattan",
      boro_cd %/% 100 == 2 ~ "Bronx",
      boro_cd %/% 100 == 3 ~ "Brooklyn",
      boro_cd %/% 100 == 4 ~ "Queens",
      boro_cd %/% 100 == 5 ~ "Staten Island",
      TRUE ~ NA_character_
    )
  ) |>
  select(boro_cd, boro_name, geometry)

arrests_acs <- arrests_acs |>
  st_join(
    cd_sf |>
      select(boro_cd, boro_name),
    join = st_within,
    left = TRUE
  )

arrests_acs_overdose <- arrests_acs |>
  mutate(
    arrest_year = as.integer(arrest_year),
    boro_cd = as.integer(boro_cd)
  ) |>
  left_join(
    overdose_cd_one,
    by = c("boro_cd", "arrest_year" = "year")
  )


arrests_acs_overdose |> 
  write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest_acs_overdose.csv')
