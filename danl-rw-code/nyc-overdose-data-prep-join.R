library(tidyverse)

df <- read_csv(
  "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/arrests_puma_acs_clean.csv"
)


df_06_21 <- read_csv(
  "/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/arrests_cd_overdose_clean_2006_2021.csv"
)

df_deaths <- df_06_21 |> 
  select(boro_cd, arrest_year,
         starts_with("death"))

colnames(df_deaths) <- str_replace(
  colnames(df_deaths),
  "deaths_due_to_use_of_or_poisoning_by_psychoactive_substance_excluding_alcohol_and_tobacco",
  "overdose_death"
)
colnames(df_deaths)
  

df <- df |> 
  mutate(boro_cd = GEOID - 3604000,
         .before = GEOID) |> 
  select(-ends_with("M"))

df_all_inner <- df |> 
  inner_join(df_06_21)

df_all_left <- df |> 
  left_join(df_06_21)

df_all <- df |> 
  full_join(df_deaths)

df_all |> 
  write_csv('/Users/bchoe/My Drive/suny-geneseo/spring2026/arrest-overdose/model_data_raw.csv')




