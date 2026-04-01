library(sf)
library(tidyverse)

gdb_path_FHSZ <- "/Users/bchoe/Downloads/FHSZSRA_23_3/FHSZSRA_23_3.gdb"

# see available layers
st_layers(gdb_path_FHSZ)

# read one layer
gdf_FHSZ <- st_read(gdb_path_FHSZ, 
                    layer = "FHSZSRA_23_3")

df_FHSZ <- gdf_FHSZ |> 
  as_tibble() |> 
  select(SRA:FHSZ_Description) 

df_FHSZ |> 
  count(FHSZ_Description)



gdb_path_fire <- "/Users/bchoe/Downloads/fire24_1.gdb"

# see available layers
st_layers(gdb_path_fire)


gdf_rxburn <- st_read(gdb_path_fire, 
                      layer = "rxburn24_1")

gdf_firep <- st_read(gdb_path_fire, 
                     layer = "firep24_1")

