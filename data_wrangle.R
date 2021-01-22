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

##add a column to include site and exclosure treatment

df_percentcover <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percentcover)
  
##add a column to include site and exclosure treatment
df_biomass <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
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



###############################################
###############  Masouleh ####################
##############################################



###Excel sheet number 1##
#########################

##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_EA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percent_cover_masouleh_EA1)

##add a column to include site and exclosure treatment
df_biomass_masouleh_EA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 2##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percent_cover_masouleh_EA2)

##add a column to include site and exclosure treatment
df_biomass_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 3##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percent_cover_masouleh_GA1)

##add a column to include site and exclosure treatment
df_biomass_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)



###Excel sheet number 4##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))


sp_name <- colnames(df_percent_cover_masouleh_GA2)

##add a column to include site and exclosure treatment
df_biomass_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 4, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###############################################
###############  Ramian ######################
##############################################

###Excel sheet number 1##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_ramian_GA1)

##add a column to include site and exclosure treatment
df_biomass_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_ramian_GA2)

##add a column to include site and exclosure treatment
df_biomass_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


############################################################
###############  Mazandaran-Javaherdeh  ####################
############################################################

###Excel sheet number 1##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_java_EA1)

##add a column to include site and exclosure treatment
df_biomass_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_java_GA1)

##add a column to include site and exclosure treatment
df_biomass_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 3##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_java_GA2)

##add a column to include site and exclosure treatment
df_biomass_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)




############################################################
###############  Mazandaran-Polour ####################
############################################################


###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_po_GA1)

##add a column to include site and exclosure treatment
df_biomass_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 2##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_po_GA2)

##add a column to include site and exclosure treatment
df_biomass_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)



###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))


sp_name <- colnames(df_percent_cover_maz_po_EA1)

##add a column to include site and exclosure treatment
df_biomass_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)



############################################################
###############  Khorasan ##################################
############################################################



###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_kho_GA1 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g_m2:litter_dry_biomass_g_m2))


sp_name <- colnames(df_percent_cover_kho_GA1)

##add a column to include site and exclosure treatment
df_biomass_kho_GA1 <- read_xlsx("Iran-North Khorasan.xlsx", sheet =1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA2 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))


sp_name <- colnames(df_percent_cover_kho_GA2)

##add a column to include site and exclosure treatment
df_biomass_kho_GA2 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA3 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))


sp_name <- colnames(df_percent_cover_kho_GA3)

##add a column to include site and exclosure treatment
df_biomass_kho_GA3 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)


###Excel sheet number 4##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA4 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))


sp_name <- colnames(df_percent_cover_kho_GA4)

##add a column to include site and exclosure treatment
df_biomass_kho_GA4 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 4, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)
