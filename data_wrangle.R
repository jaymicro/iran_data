library(tidyverse)
library(janitor)
library(readxl)


GM_E1 <- read_excel("GM_E1.xlsx") %>% 
  clean_names() %>% 
  select(sort(tidyselect::peek_vars()))
GM_E2 <- read_excel("GM_E2.xlsx") %>% 
  clean_names() %>% 
  select(sort(tidyselect::peek_vars()))
GM_GA1 <- read_excel("GM_GA1.xlsx") %>% 
  clean_names() %>% 
  select(sort(tidyselect::peek_vars()))
GM_GA2 <- read_excel("GM_GA2.xlsx") %>% 
  clean_names() %>% 
  select(sort(tidyselect::peek_vars()))



all_data <- bind_rows(GM_E1,GM_E2,GM_GA1,GM_GA2)



GM_E1_col_name <- str_extract(colnames(GM_E1), "^[a-z]+_[a-z]+")
GM_E2_col_name <- str_extract(colnames(GM_E2), "^[a-z]+_[a-z]+")
GM_GA1_col_name <- str_extract(colnames(GM_GA1), "^[a-z]+_[a-z]+")
GM_GA2_col_name <- str_extract(colnames(GM_GA2), "^[a-z]+_[a-z]+")


colnames(GM_E1) = GM_E1_col_name
colnames(GM_E2) = GM_E2_col_name
colnames(GM_GA1) = GM_GA1_col_name
colnames(GM_GA2) = GM_GA2_col_name

df_extracted_names <- bind_rows(GM_E1,GM_E2,GM_GA1,GM_GA2)

df_percentcover <- read_excel("Iran-Gillan-Masouleh.xls", sheet = "Exclosure area1", cell_rows(10:74) ) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))



sp_name <- colnames(df_percentcover)

df_biomass <- read_excel("Iran-Gillan-Masouleh.xls", sheet = "Exclosure area1", cell_rows(80:144) ) %>% 
  janitor::clean_names() 

