library(tidyverse)
library(sf)
library(tigris)
library(readxl)
library(janitor)
library(lubridate)

options(tigris_use_cache = TRUE)

# ============================================================
# 0. PATHS
# ============================================================

arrest_path <- "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/NYPD_Arrests_Data_(Historic)_-_Dangerous_Drugs_20260331.csv"
acs1_path   <- "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/acs1-data.csv"
acs5_path   <- "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/acs5-data.csv"
od_dir      <- "/Users/bchoe/My Drive/suny-geneseo/spring2026/data-overdose"

# ============================================================
# 1. READ ARREST DATA
# ============================================================

arrests <- read_csv(arrest_path, show_col_types = FALSE) |>
  mutate(
    ARREST_DATE = mdy(ARREST_DATE),
    arrest_year = year(ARREST_DATE)
  )

# build clean sf from numeric lon/lat, not WKT
arrests_sf <- arrests |>
  filter(!is.na(Longitude), !is.na(Latitude)) |>
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )

# ============================================================
# 2. GEOGRAPHIES
# ============================================================

# NYC-only PUMAs
puma_sf_nyc <- pumas(state = "NY", year = 2020, cb = TRUE) |>
  st_transform(4326) |>
  rename(
    GEOID  = GEOID20,
    PUMACE = PUMACE20,
    NAME   = NAMELSAD20
  ) |>
  filter(str_detect(NAME, "Bronx|Brooklyn|Manhattan|Queens|Staten Island")) |>
  select(GEOID, PUMACE, NAME, geometry)

# NYC Community Districts
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

# ============================================================
# 3. SPATIAL JOINS
# ============================================================

# arrest-level data with both PUMA and CD
arrests_geo <- arrests_sf |>
  st_join(puma_sf_nyc, join = st_within, left = TRUE) |>
  st_join(cd_sf, join = st_within, left = TRUE)

# ============================================================
# 4. CLEAN ARREST VARIABLES
# ============================================================

arrests_geo <- arrests_geo |>
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
    law_cat_label = case_when(
      LAW_CAT_CD == "F" ~ "Felony",
      LAW_CAT_CD == "M" ~ "Misdemeanor",
      LAW_CAT_CD == "V" ~ "Violation",
      TRUE ~ "Other / missing"
    ),
    age_group_clean = case_when(
      AGE_GROUP %in% c("<18", "18-24", "25-44", "45-64", "65+") ~ AGE_GROUP,
      AGE_GROUP == "UNKNOWN" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    perp_race_acs = case_when(
      PERP_RACE == "WHITE" ~ "white",
      PERP_RACE == "BLACK" ~ "black",
      PERP_RACE == "ASIAN / PACIFIC ISLANDER" ~ "asian",
      PERP_RACE %in% c("WHITE HISPANIC", "BLACK HISPANIC") ~ "hispanic",
      PERP_RACE %in% c("AMERICAN INDIAN/ALASKAN NATIVE", "OTHER", "UNKNOWN") ~ "other_or_unknown",
      TRUE ~ NA_character_
    ),
    perp_sex_label = case_when(
      PERP_SEX == "F" ~ "Female",
      PERP_SEX == "M" ~ "Male",
      PERP_SEX == "U" ~ "Unknown",
      TRUE ~ "Other / missing"
    )
  )

# ============================================================
# 5. ACS: BUILD PUMA-YEAR PANEL SEPARATELY
# ============================================================

acs1 <- read_csv(acs1_path, show_col_types = FALSE) |>
  mutate(source = "acs1")

acs5 <- read_csv(acs5_path, show_col_types = FALSE) |>
  mutate(source = "acs5")

# keep non-overlapping years
acs_all <- bind_rows(
  acs1 |> filter(year <= 2008),
  acs5 |> filter(year >= 2009)
) |>
  mutate(
    GEOID = as.character(GEOID),
    year = as.integer(year)
  )

# aggregate arrests to PUMA-year for ACS work
arrests_puma_agg <- arrests_geo |>
  st_drop_geometry() |>
  filter(!is.na(GEOID)) |>
  group_by(GEOID, arrest_year, NAME) |>
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

arrests_acs <- arrests_puma_agg |>
  left_join(
    acs_all,
    by = c("GEOID", "arrest_year" = "year")
  )

# ============================================================
# 6. OVERDOSE: READ AND STACK CD-YEAR FILES
# ============================================================

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
      cd_raw = na_if(cd_raw, "")
    ) |>
    filter(!is.na(cd_raw))
  
  # keep only the first 59 CD rows in borough order
  x |>
    slice(1:59) |>
    mutate(
      row_id = row_number(),
      boro_name = case_when(
        row_id <= 12 ~ "Manhattan",
        row_id <= 24 ~ "Bronx",
        row_id <= 42 ~ "Brooklyn",
        row_id <= 56 ~ "Queens",
        row_id <= 59 ~ "Staten Island"
      ),
      cd_num = case_when(
        row_id <= 12 ~ row_id,
        row_id <= 24 ~ row_id - 12,
        row_id <= 42 ~ row_id - 24,
        row_id <= 56 ~ row_id - 42,
        row_id <= 59 ~ row_id - 56
      ),
      boro_cd = case_when(
        boro_name == "Manhattan"     ~ 100L + cd_num,
        boro_name == "Bronx"         ~ 200L + cd_num,
        boro_name == "Brooklyn"      ~ 300L + cd_num,
        boro_name == "Queens"        ~ 400L + cd_num,
        boro_name == "Staten Island" ~ 500L + cd_num
      ),
      year = yr
    ) |>
    select(-row_id)
}

valid_boro_cd <- c(101:112, 201:212, 301:318, 401:414, 501:503)

overdose_cd <- map_dfr(od_files, read_overdose_file) |>
  filter(boro_cd %in% valid_boro_cd)

# overdose_cd_one <- overdose_cd |>
#   distinct(boro_cd, year, .keep_all = TRUE)

overdose_cd_one <- overdose_cd |>
  distinct(boro_cd, year, .keep_all = TRUE) |>
  select(
    boro_cd,
    year,
    contains("substance")
  )

# ============================================================
# 7. BUILD CD-YEAR ARREST PANEL AND JOIN OVERDOSE
# ============================================================

arrests_cd_agg <- arrests_geo |>
  st_drop_geometry() |>
  filter(!is.na(boro_cd)) |>
  group_by(boro_cd, boro_name, arrest_year) |>
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

arrests_cd_overdose <- arrests_cd_agg |>
  left_join(
    overdose_cd_one,
    by = c("boro_cd", "arrest_year" = "year")
  )

# ============================================================
# 8. SAVE
# ============================================================

write_csv(
  arrests_acs,
  "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/arrests_puma_acs_clean.csv"
)

cd_lookup <- tribble(
  ~boro_cd, ~cd_name,
  101, "Battery Park / Tribeca",
  102, "Greenwich Village / SoHo",
  103, "Lower East Side / Chinatown",
  104, "Chelsea / Clinton",
  105, "Midtown Business District",
  106, "Murray Hill / Stuyvesant Town",
  107, "Upper West Side",
  108, "Upper East Side",
  109, "Manhattanville / West Harlem",
  110, "Central Harlem",
  111, "East Harlem",
  112, "Washington Heights / Inwood",
  201, "Mott Haven / Melrose",
  202, "Hunts Point / Longwood",
  203, "Morrisania / Crotona",
  204, "Concourse / Highbridge",
  205, "Fordham / University Heights",
  206, "Belmont / East Tremont",
  207, "Kingsbridge Heights / Bedford",
  208, "Riverdale / Fieldston",
  209, "Parkchester / Soundview",
  210, "Throgs Neck / Co-op City",
  211, "Morris Park / Bronxdale",
  212, "Williamsbridge / Baychester",
  301, "Greenpoint / Williamsburg",
  302, "Fort Greene / Brooklyn Heights",
  303, "Bedford-Stuyvesant",
  304, "Bushwick",
  305, "East New York / Starrett City",
  306, "Park Slope / Carroll Gardens",
  307, "Sunset Park",
  308, "Crown Heights North / Prospect Heights",
  309, "Crown Heights South / Lefferts Gardens",
  310, "Bay Ridge / Dyker Heights",
  311, "Bensonhurst",
  312, "Borough Park",
  313, "Coney Island",
  314, "Flatbush / Midwood",
  315, "Sheepshead Bay",
  316, "Brownsville",
  317, "East Flatbush",
  318, "Flatlands / Canarsie",
  401, "Long Island City / Astoria",
  402, "Woodside / Sunnyside",
  403, "Jackson Heights",
  404, "Elmhurst / Corona",
  405, "Ridgewood / Maspeth",
  406, "Rego Park / Forest Hills",
  407, "Flushing",
  408, "Hillcrest / Fresh Meadows",
  409, "Kew Gardens / Woodhaven",
  410, "South Ozone Park / Howard Beach",
  411, "Bayside / Little Neck",
  412, "Jamaica / Hollis",
  413, "Queens Village",
  414, "Rockaway / Broad Channel",
  501, "St. George / Stapleton",
  502, "South Beach / Willowbrook",
  503, "Tottenville / Great Kills"
)

arrests_cd_overdose <- arrests_cd_agg |>
  left_join(
    overdose_cd_one,
    by = c("boro_cd", "arrest_year" = "year")
  ) |>
  left_join(cd_lookup, by = "boro_cd") |>
  relocate(cd_name, .after = boro_name)


arrests_cd_overdose <- arrests_cd_overdose |> 
  relocate(cd_name, .after = boro_cd)

arrests_cd_overdose_NA <- arrests_cd_overdose |> 
  filter(is.na(cd_name)) |> 
  distinct(boro_cd)
arrests_cd_overdose_NA

arrests_cd_overdose_2006_2021 <- arrests_cd_overdose |> 
  filter(arrest_year <= 2021)

arrests_cd_overdose_2022_2024 <- arrests_cd_overdose |> 
  filter(arrest_year > 2021)

arrests_cd_overdose_2006_2021 |> 
  write_csv(
  "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/arrests_cd_overdose_clean_2006_2021.csv"
)


sum_2006_2021 <- skimr::skim(arrests_cd_overdose_2006_2021)
sum_2022_2024 <- skimr::skim(arrests_cd_overdose_2022_2024)

overdose_cd_one |>
  count(boro_cd, year) |>
  filter(n > 1)

overdose_cd_one |>
  count(is.na(boro_cd))