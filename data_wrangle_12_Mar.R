
# Import packages ---------------------------------------------------------

library(tidyverse)


# Import data -------------------------------------------------------------

df_biomass_clean <- read.csv("biomass_March1.csv",  row.names = 1) 


df_pc<- read.csv("percentcover_March1.csv",  row.names = 1) 

df_rangescore <- readxl::read_xlsx("df_range_score.xlsx") %>% 
  separate(plot_indicator,into = c('col', 'row'), sep = 1)

metadata<- read.csv("metadata.csv")


# Data wrangle ------------------------------------------------------------



metadata$elevation <- as.numeric(metadata$elevation)
metadata$slope <- as.numeric(metadata$slope)
metadata$avg_temp <- as.numeric(metadata$avg_temp)
metadata$avg_ppt <- as.numeric(metadata$avg_ppt)

str(metadata)

names(df_biomass_clean)
which(rowSums(df_biomass_clean[, -106]) == 0)
which(rowSums(df_pc[, -106]) == 0)



df_pc <- df_pc %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>%
  filter(str_detect(id_id, "kho_") == FALSE) %>% select(-id_id)

df_biomass_clean <- df_biomass_clean %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>%
  filter(str_detect(id_id, "kho_") == FALSE) %>% select(-id_id)

metadata <- metadata %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))%>%
  filter(str_detect(id_id, "kho_") == FALSE)

df_rangescore <- df_rangescore %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))%>%
  filter(str_detect(id_id, "kho_") == FALSE)

which(rowSums(df_biomass_clean) == 0)
which(rowSums(df_pc) == 0)

metadata$grid <- paste(df_rangescore$col, df_rangescore$row)
metadata$bm <- rowSums(df_biomass_clean)

which(is.numeric(df_pc))


write.csv(df_pc, file="pc_12_Mar.csv")
write.csv(df_biomass_clean, file="biomass_12_Mar.csv")
write.csv(metadata, file="meta_12_Mar.csv")
write.csv(df_rangescore, file="df_range_score_12_Mar.csv")
