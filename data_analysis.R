library(tidyverse)
library(vegan)
library(ape)




df<- read.csv("combined_df_percent_cover.csv",  row.names = 1) 


df_pc <- df %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  filter_if(rowSums(.)=0)



metadata <- df %>% 
  select(id_id) %>% 
  mutate(         treatment = ifelse(grepl("GA", .$id_id), "grazing", "exclosure"),
         site = ifelse(grepl("kho", .$id_id), "kho",
                       ifelse(grepl("maz_po", .$id_id), "maz_po",
                              ifelse(grepl("masouleh", .$id_id), "masouleh",
                                     ifelse(grepl("maz_java", .$id_id), "maz_java","ramian")))))

  
  
  

