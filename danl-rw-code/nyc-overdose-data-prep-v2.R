library(tidyverse)
library(sf)
library(tigris)
library(lubridate)
library(readxl)
library(janitor)



# socioeconomic data ------------------------------------------------------


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


# Arrest Data -------------------------------------------------------------

arrests <- read_csv(
  '/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/NYPD_Arrests_Data_(Historic)_-_Dangerous_Drugs_20260331.csv'
)

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

puma_sf_nyc <- pumas(state = "NY", year = 2020, cb = TRUE) |>
  st_transform(4326) |>
  rename(
    GEOID = GEOID20,
    PUMACE = PUMACE20,
    NAME = NAMELSAD20
  ) |>
  filter(
    str_detect(NAME, "Bronx|Brooklyn|Manhattan|Queens|Staten Island")
  ) |>
  select(GEOID, PUMACE, NAME, geometry)

# arrests_cd_plain <- arrests_cd |>
  # as_tibble()

arrests_puma_sf <- arrests_sf |>
  filter(!is.na(Longitude), !is.na(Latitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
  st_join(
    puma_sf_nyc,
    join = st_within,
    left = TRUE
  )

# Spatial join 
arrests_cd <- arrests_sf |>
  st_join(puma_sf_nyc, join = st_within, left = TRUE) |>
  mutate(
    GEOID = as.character(GEOID),
    arrest_year = lubridate::year(lubridate::mdy(ARREST_DATE))
  ) 


# arrest data cleaning ----------------------------------------------------


arrests_cd |> 
  as_tibble() |> 
  count(PD_DESC)

arrests_cd <- arrests_cd |>
  mutate(
    drug_category = case_when(
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 7" ~ "Controlled substance possession: 7th degree",
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 5" ~ "Controlled substance possession: 5th degree",
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 4" ~ "Controlled substance possession: 4th degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 3"    ~ "Controlled substance possession: 3rd degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 2"    ~ "Controlled substance possession: 2nd degree",
      PD_DESC == "CONTROLLED SUBSTANCE,POSSESS. 1"    ~ "Controlled substance possession: 1st degree",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE, POSSESSI",
        "CONTROLLED SUBSTANCE,POSSESS."
      ) ~ "Controlled substance possession: unspecified degree",
      PD_DESC == "DRUG, INJECTION OF" ~ "Drug injection offense",
      PD_DESC == "MARIJUANA, POSSESSION 4 & 5" ~ "Marijuana possession: lower degree",
      PD_DESC == "MARIJUANA, POSSESSION 1, 2 & 3" ~ "Marijuana possession: higher degree",
      PD_DESC == "USE CHILD TO COMMIT CONT SUB OFF" ~ "Aggravated drug offense involving child",
      TRUE ~ "Other or unknown drug offense"
    ),
    .before = PD_DESC
  )

arrests_cd <- arrests_cd |>
  mutate(
    drug_category_broad = case_when(
      PD_DESC == "CONTROLLED SUBSTANCE, POSSESSION 7" ~ "controlled_substance_possession_low",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE, POSSESSION 5",
        "CONTROLLED SUBSTANCE, POSSESSION 4"
      ) ~ "controlled_substance_possession_mid",
      PD_DESC %in% c(
        "CONTROLLED SUBSTANCE,POSSESS. 3",
        "CONTROLLED SUBSTANCE,POSSESS. 2",
        "CONTROLLED SUBSTANCE,POSSESS. 1"
      ) ~ "controlled_substance_possession_high",
      PD_DESC == "MARIJUANA, POSSESSION 4 & 5" ~ "marijuana_possession_low",
      PD_DESC == "MARIJUANA, POSSESSION 1, 2 & 3" ~ "marijuana_possession_high",
      PD_DESC %in% c(
        "DRUG, INJECTION OF",
        "USE CHILD TO COMMIT CONT SUB OFF",
        "CONTROLLED SUBSTANCE, POSSESSI",
        "CONTROLLED SUBSTANCE,POSSESS."
      ) ~ "other_drug_offense",
      TRUE ~ "other_or_unknown_drug_offense"
    ),
    .before = PD_DESC
  )

arrests_cd |> 
  as_tibble() |> 
  count(drug_category)

arrests_cd |> 
  as_tibble() |> 
  count(drug_category_broad)

arrests_cd |> 
  as_tibble() |> 
  count(OFNS_DESC) # single category

arrests_cd |> 
  as_tibble() |> 
  count(LAW_CAT_CD)

arrests_cd <- arrests_cd |>
  mutate(
    law_cat_label = case_when(
      LAW_CAT_CD == "F" ~ "Felony",
      LAW_CAT_CD == "M" ~ "Misdemeanor",
      LAW_CAT_CD == "V" ~ "Violation",
      TRUE ~ "Other / missing"
    ),
    .after = LAW_CAT_CD
  )

arrests_AGE_GROUP <- arrests_cd |> 
  as_tibble() |> 
  count(AGE_GROUP)

arrests_cd <- arrests_cd |>
  mutate(
    age_group_clean = case_when(
      AGE_GROUP %in% c("<18", "18-24", "25-44", "45-64", "65+") ~ AGE_GROUP,
      AGE_GROUP == "UNKNOWN" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    age_group_clean = factor(
      age_group_clean,
      levels = c("<18", "18-24", "25-44", "45-64", "65+", "Unknown")
    ),
    .after = AGE_GROUP
  )

arrests_cd |> 
  as_tibble() |> 
  count(age_group_clean)

arrests_cd |> 
  as_tibble() |> 
  count(PERP_RACE)

arrests_cd <- arrests_cd |>
  mutate(
    perp_race_acs = case_when(
      PERP_RACE == "WHITE" ~ "white",
      PERP_RACE == "BLACK" ~ "black",
      PERP_RACE == "ASIAN / PACIFIC ISLANDER" ~ "asian",
      PERP_RACE %in% c("WHITE HISPANIC", "BLACK HISPANIC") ~ "hispanic",
      PERP_RACE %in% c("AMERICAN INDIAN/ALASKAN NATIVE", "OTHER", "UNKNOWN") ~ "other_or_unknown",
      TRUE ~ NA_character_
    ),
    .after = PERP_RACE
  )

arrests_cd |> 
  as_tibble() |> 
  count(perp_race_acs)

arrests_cd |> 
  as_tibble() |> 
  count(PERP_SEX)




cd_sf <- st_read(
  "https://data.cityofnewyork.us/resource/5crt-au7u.geojson?$limit=1000",
  quiet = TRUE
) |>
  st_transform(4326) |>
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


# drop broken sf attributes completely
arrests_cd_plain <- arrests_cd |>
  as_tibble()

# rebuild sf from numeric lon/lat
arrests_cd_sf <- arrests_cd_plain |>
  filter(!is.na(Longitude), !is.na(Latitude)) |>
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_join(
    cd_sf |>
      select(boro_cd, boro_name),
    join = st_within,
    left = TRUE
  )


arrests_cd_agg <- arrests_cd_sf |>
  st_drop_geometry() |>
  rename(perp_sex_label = PERP_SEX) |> 
  group_by(GEOID, boro_cd, boro_name, NAME, arrest_year) |>
  summarize(
    arrests_total = n(),
    arrests_felony = sum(law_cat_label == "Felony", na.rm = TRUE),
    arrests_misdemeanor = sum(law_cat_label == "Misdemeanor", na.rm = TRUE),
    
    cs_possession_low = sum(drug_category_broad == "controlled_substance_possession_low", na.rm = TRUE),
    cs_possession_mid = sum(drug_category_broad == "controlled_substance_possession_mid", na.rm = TRUE),
    cs_possession_high = sum(drug_category_broad == "controlled_substance_possession_high", na.rm = TRUE),
    mj_possession_low = sum(drug_category_broad == "marijuana_possession_low", na.rm = TRUE),
    mj_possession_high = sum(drug_category_broad == "marijuana_possession_high", na.rm = TRUE),
    other_drug_offense = sum(drug_category_broad == "other_drug_offense", na.rm = TRUE),
    
    arrests_female = sum(perp_sex_label == "Female", na.rm = TRUE),
    arrests_male = sum(perp_sex_label == "Male", na.rm = TRUE),
    
    arrests_under18 = sum(age_group_clean == "<18", na.rm = TRUE),
    arrests_18_24 = sum(age_group_clean == "18-24", na.rm = TRUE),
    arrests_25_44 = sum(age_group_clean == "25-44", na.rm = TRUE),
    arrests_45_64 = sum(age_group_clean == "45-64", na.rm = TRUE),
    arrests_65plus = sum(age_group_clean == "65+", na.rm = TRUE),
    
    arrests_white = sum(perp_race_acs == "white", na.rm = TRUE),
    arrests_black = sum(perp_race_acs == "black", na.rm = TRUE),
    arrests_asian = sum(perp_race_acs == "asian", na.rm = TRUE),
    arrests_hispanic = sum(perp_race_acs == "hispanic", na.rm = TRUE),
    
    .groups = "drop"
  )





# ACS merge
arrests_acs <- arrests_cd_agg |>
  left_join(
    acs_all,
    by = c("GEOID", "arrest_year" = "year")
  )


# overdose ----------------------------------------------------------------


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



arrests_cd_overdose <- arrests_cd_agg |>
  left_join(
    overdose_cd_one,
    by = c("boro_cd", "arrest_year" = "year")
  )


colnames(arrests_cd_overdose)


arrests_cd_overdose |> 
  write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/arrest_acs_overdose_clean.csv')


# arrests_acs <- arrests_acs |>
#   st_join(
#     cd_sf |>
#       select(boro_cd, boro_name),
#     join = st_within,
#     left = TRUE
#   )


# # arrests_acs_sf <- arrests_acs |>
#   filter(!is.na(Lon_Lat)) |>
#   mutate(Lon_Lat = str_squish(Lon_Lat)) |>
#   st_as_sf(wkt = "Lon_Lat", crs = 4326)
# 
# arrests_acs_sf <- arrests_acs_sf |>
#   st_join(
#     cd_sf |>
#       select(boro_cd, boro_name),
#     join = st_within,
#     left = TRUE
#   )
# 
# arrests_acs_overdose <- arrests_acs |>
#   mutate(
#     arrest_year = as.integer(arrest_year),
#     boro_cd = as.integer(boro_cd)
#   ) |>
#   left_join(
#     overdose_cd_one,
#     by = c("boro_cd", "arrest_year" = "year")
#   )
# 
# unique(arrests_acs_overdose$arrest_year)
# colnames(arrests_acs_overdose)
# 
# arrests_acs_overdose |> 
#   skimr::skim(NAME.x, NAME.y)
# 
# 
# arrests_acs_overdose <- 
#   arrests_acs_overdose |> 
#   relocate(
#     arrest_year, boro_cd, boro_name, GEOID, 
#     NAME.x, NAME.y, 
#     Lon_Lat, Longitude, Latitude,
#     X_COORD_CD, Y_COORD_CD
#   ) |> 
#   mutate(NAME = NAME.x, .before = NAME.x,
#          NAME = ifelse(is.na(NAME.x) & !is.na(NAME.y),
#                        NAME.y, NAME.x)
#          )
# 
# arrests_acs_overdose |> 
#   skimr::skim(NAME, NAME.x, NAME.y)
# 
# arrests_acs_overdose <- 
#   arrests_acs_overdose |> 
#   select(-NAME.x, -NAME.y)

# 
# arrests_acs_overdose |> 
#   write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest_acs_overdose.csv')
