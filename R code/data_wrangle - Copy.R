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

<<<<<<< HEAD
##add a column to include site and exclosure treatment

df_percentcover <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(10:74)) %>% 
=======
df_percentcover <- read_excel("Iran-Gillan-Masouleh.xlsx", sheet = "Exclosure area1", cell_rows(10:74)) %>% 
>>>>>>> af36d46aa1706b8868adbe1bae99839ded4fd1a9
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percentcover)
  
<<<<<<< HEAD
##add a column to include site and exclosure treatment
df_biomass <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
=======

df_biomass <- read_excel("Iran-Gillan-Masouleh.xlsx", sheet = "Exclosure area1", cell_rows(81:144), col_names = FALSE) %>% 
>>>>>>> af36d46aa1706b8868adbe1bae99839ded4fd1a9
  janitor::clean_names() %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)

##add a column to include site and exclosure treatment
df_plant_metadata <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                  sheet = 1,  fillMergedCells = TRUE, 
                                  colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)

