library(tidyverse)
library(janitor)
library(readxl)


###############################################
###############  Masouleh ####################
##############################################



###Excel sheet number 1##
#########################

##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_EA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class)) %>% 
  mutate(id_id = rep("masouleh_EA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_masouleh_EA1)

##add a column to include site and exclosure treatment
df_biomass_masouleh_EA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("masouleh_EA1", times = nrow(.)))


###Excel sheet number 2##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_WORK", times = nrow(.)))

sp_name <- colnames(df_percent_cover_masouleh_EA2)

##add a column to include site and exclosure treatment
df_biomass_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name) %>% 
  mutate(id_id = rep("masouleh_EA2", times = nrow(.)))



###Excel sheet number 3##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_masouleh_GA1)

##add a column to include site and exclosure treatment
df_biomass_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("masouleh_GA1", times = nrow(.)))



###Excel sheet number 4##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_GA2", times = nrow(.)))



sp_name <- colnames(df_percent_cover_masouleh_GA2)

##add a column to include site and exclosure treatment
df_biomass_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 4, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("masouleh_GA2", times = nrow(.)))


###############################################
###############  Ramian ######################
##############################################

###Excel sheet number 1##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("ramian_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_ramian_GA1)

##add a column to include site and exclosure treatment
df_biomass_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("ramian_GA1", times = nrow(.)))


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("ramian_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_ramian_GA2)

##add a column to include site and exclosure treatment
df_biomass_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("ramian_GA2", times = nrow(.)))


############################################################
###############  Mazandaran-Javaherdeh  ####################
############################################################

###Excel sheet number 1##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_java_EA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_EA1)

##add a column to include site and exclosure treatment
df_biomass_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name) %>% 
  mutate(id_id = rep("maz_java_EA1", times = nrow(.)))


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g)) %>% 
  mutate(id_id = rep("maz_java_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_GA1)

##add a column to include site and exclosure treatment
df_biomass_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("maz_java_GA1", times = nrow(.)))


###Excel sheet number 3##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_java_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_GA2)

##add a column to include site and exclosure treatment
df_biomass_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("maz_java_GA2", times = nrow(.)))




############################################################
###############  Mazandaran-Polour ####################
############################################################


###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_GA1)

##add a column to include site and exclosure treatment
df_biomass_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("maz_po_GA1", times = nrow(.)))


###Excel sheet number 2##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_GA2)

##add a column to include site and exclosure treatment
df_biomass_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("maz_po_GA2", times = nrow(.)))



###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_EA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_EA1)

##add a column to include site and exclosure treatment
df_biomass_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("maz_po_EA1", times = nrow(.)))



############################################################
###############  Khorasan ##################################
############################################################



###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_kho_GA1 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g_m2:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA1)

##add a column to include site and exclosure treatment
df_biomass_kho_GA1 <- read_xlsx("Iran-North Khorasan.xlsx", sheet =1, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("kho_GA1", times = nrow(.)))


###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA2 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA2)

##add a column to include site and exclosure treatment
df_biomass_kho_GA2 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 2, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("kho_GA2", times = nrow(.)))


###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA3 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA3", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA3)

##add a column to include site and exclosure treatment
df_biomass_kho_GA3 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 3, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("kho_GA3", times = nrow(.)))


###Excel sheet number 4##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_percent_cover_kho_GA4 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA4", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA4)

##add a column to include site and exclosure treatment
df_biomass_kho_GA4 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 4, cell_rows(81:144), col_names = FALSE) %>% 
  janitor::clean_names() %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  mutate(id_id = rep("kho_GA4", times = nrow(.)))





##############################################################################################################################
######################################    Combine the datasets  ##############################################################
##############################################################################################################################

#-----Percent cover------

df_percent_cover_kho_GA1_name <- str_extract(colnames(df_percent_cover_kho_GA1), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_kho_GA1) = df_percent_cover_kho_GA1_name

df_percent_cover_kho_GA2_name <- str_extract(colnames(df_percent_cover_kho_GA2), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_kho_GA2) = df_percent_cover_kho_GA2_name

df_percent_cover_kho_GA3_name <- str_extract(colnames( df_percent_cover_kho_GA3), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_kho_GA3) =  df_percent_cover_kho_GA3_name

df_percent_cover_kho_GA4_name <- str_extract(colnames(df_percent_cover_kho_GA4), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_kho_GA4) =  df_percent_cover_kho_GA4_name

df_percent_cover_masouleh_EA1_name <- str_extract(colnames(df_percent_cover_masouleh_EA1), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_masouleh_EA1) =  df_percent_cover_masouleh_EA1_name

df_percent_cover_masouleh_GA1_name <- str_extract(colnames(df_percent_cover_masouleh_GA1), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_masouleh_GA1) =  df_percent_cover_masouleh_GA1_name

df_percent_cover_masouleh_GA2_name <- str_extract(colnames(df_percent_cover_masouleh_GA2), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_masouleh_GA2) =  df_percent_cover_masouleh_GA2_name

df_percent_cover_maz_java_EA1_name <- str_extract(colnames(df_percent_cover_maz_java_EA1), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_maz_java_EA1) =  df_percent_cover_maz_java_EA1_name

df_percent_cover_maz_java_GA1_name <- str_extract(colnames(df_percent_cover_maz_java_GA1), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_maz_java_GA1) =  df_percent_cover_maz_java_GA1_name

df_percent_cover_maz_java_GA2_name <- str_extract(colnames(df_percent_cover_maz_java_GA2), "^[a-z]+_[a-z]+")
colnames(df_percent_cover_maz_java_GA2) =  df_percent_cover_maz_java_GA2_name

df_percent_cover_maz_po_EA1_name <- str_extract(colnames( df_percent_cover_maz_po_EA1), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_maz_po_EA1) =   df_percent_cover_maz_po_EA1_name


df_percent_cover_maz_po_GA1_name <- str_extract(colnames( df_percent_cover_maz_po_GA1), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_maz_po_GA1) =   df_percent_cover_maz_po_GA1_name

df_percent_cover_maz_po_GA2_name <- str_extract(colnames( df_percent_cover_maz_po_GA2), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_maz_po_GA2) =   df_percent_cover_maz_po_GA2_name

df_percent_cover_ramian_GA1_name <- str_extract(colnames( df_percent_cover_ramian_GA1), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_ramian_GA1) =   df_percent_cover_ramian_GA1_name


df_percent_cover_ramian_GA2_name <- str_extract(colnames( df_percent_cover_ramian_GA2), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_ramian_GA2) =   df_percent_cover_ramian_GA2_name

df_percent_cover_masouleh_EA1_name <- str_extract(colnames( df_percent_cover_masouleh_EA1), "^[a-z]+_[a-z]+")
colnames( df_percent_cover_masouleh_EA1) =   df_percent_cover_masouleh_EA1_name


combined_df_percent_cover <- bind_rows(df_percent_cover_kho_GA1 ,
                                       df_percent_cover_kho_GA2 ,
                                       df_percent_cover_kho_GA3 ,
                                       df_percent_cover_kho_GA4 , 
                                       df_percent_cover_masouleh_EA1 ,
                                       df_percent_cover_maz_java_GA1,  
                                       df_percent_cover_masouleh_GA1 ,
                                       df_percent_cover_masouleh_GA2 ,
                                       df_percent_cover_maz_java_EA1 ,
                                       df_percent_cover_maz_java_GA1 ,
                                       df_percent_cover_maz_java_GA2 ,
                                       df_percent_cover_maz_po_EA1 ,
                                       df_percent_cover_maz_po_GA1 ,
                                       df_percent_cover_maz_po_GA2 ,
                                       df_percent_cover_ramian_GA1 ,
                                       df_percent_cover_ramian_GA2 ,
                                       df_percent_cover_masouleh_EA1)%>% 
  select(sort(tidyselect::peek_vars()))


#-----Biomass------

df_biomass_kho_GA1_name <- str_extract(colnames(df_biomass_kho_GA1), "^[a-z]+_[a-z]+")
colnames(df_biomass_kho_GA1) = df_biomass_kho_GA1_name

df_biomass_kho_GA2_name <- str_extract(colnames(df_biomass_kho_GA2), "^[a-z]+_[a-z]+")
colnames(df_biomass_kho_GA2) = df_biomass_kho_GA2_name

df_biomass_kho_GA3_name <- str_extract(colnames( df_biomass_kho_GA3), "^[a-z]+_[a-z]+")
colnames( df_biomass_kho_GA3) =  df_biomass_kho_GA3_name

df_biomass_kho_GA4_name <- str_extract(colnames(df_biomass_kho_GA4), "^[a-z]+_[a-z]+")
colnames( df_biomass_kho_GA4) =  df_biomass_kho_GA4_name

df_biomass_masouleh_EA1_name <- str_extract(colnames(df_biomass_masouleh_EA1), "^[a-z]+_[a-z]+")
colnames(df_biomass_masouleh_EA1) =  df_biomass_masouleh_EA1_name

df_biomass_masouleh_GA1_name <- str_extract(colnames(df_biomass_masouleh_GA1), "^[a-z]+_[a-z]+")
colnames(df_biomass_masouleh_GA1) =  df_biomass_masouleh_GA1_name

df_biomass_masouleh_GA2_name <- str_extract(colnames(df_biomass_masouleh_GA2), "^[a-z]+_[a-z]+")
colnames(df_biomass_masouleh_GA2) =  df_biomass_masouleh_GA2_name

df_biomass_maz_java_EA1_name <- str_extract(colnames(df_biomass_maz_java_EA1), "^[a-z]+_[a-z]+")
colnames(df_biomass_maz_java_EA1) =  df_biomass_maz_java_EA1_name

df_biomass_maz_java_GA1_name <- str_extract(colnames(df_biomass_maz_java_GA1), "^[a-z]+_[a-z]+")
colnames(df_biomass_maz_java_GA1) =  df_biomass_maz_java_GA1_name

df_biomass_maz_java_GA2_name <- str_extract(colnames(df_biomass_maz_java_GA2), "^[a-z]+_[a-z]+")
colnames(df_biomass_maz_java_GA2) =  df_biomass_maz_java_GA2_name

df_biomass_maz_po_EA1_name <- str_extract(colnames( df_biomass_maz_po_EA1), "^[a-z]+_[a-z]+")
colnames( df_biomass_maz_po_EA1) =   df_biomass_maz_po_EA1_name


df_biomass_maz_po_GA1_name <- str_extract(colnames( df_biomass_maz_po_GA1), "^[a-z]+_[a-z]+")
colnames( df_biomass_maz_po_GA1) =   df_biomass_maz_po_GA1_name

df_biomass_maz_po_GA2_name <- str_extract(colnames( df_biomass_maz_po_GA2), "^[a-z]+_[a-z]+")
colnames( df_biomass_maz_po_GA2) =   df_biomass_maz_po_GA2_name

df_biomass_ramian_GA1_name <- str_extract(colnames( df_biomass_ramian_GA1), "^[a-z]+_[a-z]+")
colnames( df_biomass_ramian_GA1) =   df_biomass_ramian_GA1_name


df_biomass_ramian_GA2_name <- str_extract(colnames( df_biomass_ramian_GA2), "^[a-z]+_[a-z]+")
colnames( df_biomass_ramian_GA2) =   df_biomass_ramian_GA2_name

df_biomass_masouleh_EA1_name <- str_extract(colnames( df_biomass_masouleh_EA1), "^[a-z]+_[a-z]+")
colnames( df_biomass_masouleh_EA1) =   df_biomass_masouleh_EA1_name


combined_df_biomass <- bind_rows(df_biomass_kho_GA1 ,
                                       df_biomass_kho_GA2 ,
                                       df_biomass_kho_GA3 ,
                                       df_biomass_kho_GA4 , 
                                       df_biomass_masouleh_EA1 ,
                                       df_biomass_maz_java_GA1,  
                                       df_biomass_masouleh_GA1 ,
                                       df_biomass_masouleh_GA2 ,
                                       df_biomass_maz_java_EA1 ,
                                       df_biomass_maz_java_GA1 ,
                                       df_biomass_maz_java_GA2 ,
                                       df_biomass_maz_po_EA1 ,
                                       df_biomass_maz_po_GA1 ,
                                       df_biomass_maz_po_GA2 ,
                                       df_biomass_ramian_GA1 ,
                                       df_biomass_ramian_GA2 ,
                                       df_biomass_masouleh_EA1)%>% 
  select(sort(tidyselect::peek_vars()))

write.csv(combined_df_biomass, file="combined_df_biomass.csv")
write.csv(combined_df_percent_cover, file="combined_df_percent_cover.csv")

