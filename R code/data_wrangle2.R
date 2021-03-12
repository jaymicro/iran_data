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



df_extracted_names <- bind_rows(GM_E1,GM_E2,GM_GA1,GM_GA2)

df_percentcover <- read_excel("Iran-Gillan-Masouleh.xlsx", sheet = "Exclosure area1", cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percentcover)
  

df_biomass <- read_excel("Iran-Gillan-Masouleh.xlsx", sheet = "Exclosure area1", cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)

df_plant_metadata <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                  sheet = "Exclosure area1",  fillMergedCells = TRUE, 
                                  colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)

