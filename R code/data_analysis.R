library(tidyverse)
library(vegan)
library(ape)
library(diverse)
library(abdiv)


df_percentcover<- read.csv("combined_df_percent_cover.csv",  row.names = 1)
df_biomass<- read.csv("combined_df_biomass.csv",  row.names = 1)

df_pc <- df_percentcover %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))


df_biomass_clean <- df_biomass %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))

which(rowSums(df_biomass_clean) ==0)
which(rowSums(df_pc) ==0)

metadata <- df_percentcover %>% 
  select(id_id) %>% 
  mutate(         treatment = ifelse(grepl("GA", .$id_id), "grazing", "exclosure"),
         site = ifelse(grepl("kho", .$id_id), "kho",
                       ifelse(grepl("maz_po", .$id_id), "maz_po",
                              ifelse(grepl("masouleh", .$id_id), "masouleh",
                                     ifelse(grepl("maz_java", .$id_id), "maz_java","ramian")))))



###########################################################################################
############################### Alpha diversity ###########################################
###########################################################################################

 div_metricx<- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(df_pc, index = "shannon"),
  simpson_index = diversity(df_pc, index = "simpson"),
  
)

df_pc %>% 
  group_by(row_number()) %>% 

