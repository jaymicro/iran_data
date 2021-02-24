library(tidyverse)
library(janitor)
library(readxl)


# Gillian Masouleh GA1 ----------------------------------------------------


df_percent_cover_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class)) %>% 
  mutate(id_id = rep("masouleh_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_masouleh_GA1)


plant_metadata_gil_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                            sheet = 1,  fillMergedCells = TRUE, 
                                            colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name) %>% 
  slice(1)


# Gillian Masouleh EA2 ----------------------------------------------------



df_percent_cover_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_WORK", times = nrow(.)))

sp_name <- colnames(df_percent_cover_masouleh_EA2)

plant_metadata_gil_EA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                             sheet = 2,  fillMergedCells = TRUE, 
                                             colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)



# Gillian Masouleh GA1 ----------------------------------------------------

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_percent_cover_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_GA1", times = nrow(.)))%>% 
  slice(1)



sp_name <- colnames(df_percent_cover_masouleh_GA1)


plant_metadata_gil_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                             sheet = 3,  fillMergedCells = TRUE, 
                                             colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)

# Gillian Masouleh GA2 ----------------------------------------------------

df_percent_cover_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:range_class))%>% 
  mutate(id_id = rep("masouleh_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_masouleh_GA2)


plant_metadata_gil_GA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-Gillan-Masouleh.xlsx",
                                                 sheet = 4,  fillMergedCells = TRUE, 
                                                 colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)




############################################################################################################
##########################################################################################################


# Ramian GA1------------------------------------------------------------------

df_percent_cover_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("ramian_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_ramian_GA1)

plant_metadata_ramian_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Golestan-Ramian.xlsx",
                                                 sheet = 1,  fillMergedCells = TRUE, 
                                                 colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Ramina GA2 --------------------------------------------------------------



df_percent_cover_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(species_names,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("ramian_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_ramian_GA2)

plant_metadata_ramian_GA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-Golestan-Ramian.xlsx",
                                                    sheet = 2,  fillMergedCells = TRUE, 
                                                    colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)

##########################################################################################################
##########################################################################################################


# Maz Jav EA1 -------------------------------------------------------------

df_percent_cover_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_java_EA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_EA1)

plant_metadata_maz_java_EA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Javaherdeh site.xlsx",
                                                    sheet = 1,  fillMergedCells = TRUE, 
                                                    colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Maz java GA1 ---------------------------------------------------------------

df_percent_cover_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_java_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_GA1)

plant_metadata_maz_java_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Javaherdeh site.xlsx",
                                                      sheet = 2,  fillMergedCells = TRUE, 
                                                      colNames = F) %>% 
  slice(75:77) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  select(1:(length(sp_name)+1)) %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Maz java GA2 ------------------------------------------------------------

df_percent_cover_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_java_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_java_GA2)

plant_metadata_maz_java_GA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Javaherdeh site.xlsx",
                                                      sheet = 3,  fillMergedCells = TRUE, 
                                                      colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)

# Maz Po   GA1 -----------------------------------------------------------------

df_percent_cover_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_GA1)

plant_metadata_maz_po_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Polour.xlsx",
                                                      sheet = 1,  fillMergedCells = TRUE, 
                                                      colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Maz Po GA2 --------------------------------------------------------------

df_percent_cover_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_GA2)

plant_metadata_maz_po_GA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Polour.xlsx",
                                                    sheet = 2,  fillMergedCells = TRUE, 
                                                    colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Maz Po EA1 ------------------------------------------------------------------

df_percent_cover_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g))%>% 
  mutate(id_id = rep("maz_po_EA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_maz_po_EA1)

plant_metadata_maz_po_EA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Polour.xlsx",
                                                    sheet = 3,  fillMergedCells = TRUE, 
                                                    colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols") %>% 
  select(-c(x1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)



# Maz GA1  -----------------------------------------------------------------

df_percent_cover_kho_GA1 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g_m2:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA1", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA1)

plant_metadata_kho_GA1 <- openxlsx::readWorkbook(xlsxFile = "Iran-North Khorasan.xlsx",
                                                    sheet = 1,  fillMergedCells = TRUE, 
                                                    colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  select(1:(length(sp_name)+1)) %>% 
  select(-c(X1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Kho GA2 -----------------------------------------------------------------

df_percent_cover_kho_GA2 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA2", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA2)

plant_metadata_kho_GA2 <- openxlsx::readWorkbook(xlsxFile = "Iran-North Khorasan.xlsx",
                                                 sheet = 2,  fillMergedCells = TRUE, 
                                                 colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  select(1:(length(sp_name)+1)) %>% 
  select(-c(X1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Kho GA3 -----------------------------------------------------------------

df_percent_cover_kho_GA3 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA3", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA3)

plant_metadata_kho_GA3 <- openxlsx::readWorkbook(xlsxFile = "Iran-North Khorasan.xlsx",
                                                 sheet = 3,  fillMergedCells = TRUE, 
                                                 colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  select(1:(length(sp_name)+1)) %>% 
  select(-c(X1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)


# Kho GA4 -----------------------------------------------------------------

df_percent_cover_kho_GA4 <- read_xlsx("Iran-North Khorasan.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(-c(plot_indicator,above_ground_live_dry_biomass_g:litter_dry_biomass_g_m2))%>% 
  mutate(id_id = rep("kho_GA3", times = nrow(.)))


sp_name <- colnames(df_percent_cover_kho_GA4)


plant_metadata_kho_GA4 <- openxlsx::readWorkbook(xlsxFile = "Iran-North Khorasan.xlsx",
                                                 sheet = 4,  fillMergedCells = TRUE, 
                                                 colNames = F) %>% 
  slice(75:79) %>%
  #janitor::row_to_names(1) %>% 
  select(1:(length(sp_name)+1)) %>% 
  select(-c(X1)) %>% 
  select(1:length(sp_name)) %>% 
  rename_at(colnames(.), ~ sp_name)%>% 
  slice(1)

change_names <- function(x){
  nw_name <- str_extract(colnames(x), "^[a-z]+_[a-z]+")
  names(x) <- nw_name
  return(as.data.frame(x))
}

df <- list(plant_metadata_gil_EA2,
           plant_metadata_gil_GA1,
           plant_metadata_gil_GA2,
           plant_metadata_kho_GA1,
           plant_metadata_kho_GA2,
           plant_metadata_kho_GA3,
           plant_metadata_kho_GA4,
           plant_metadata_maz_java_EA1,
           plant_metadata_maz_java_GA1,
           plant_metadata_maz_java_GA2,
           plant_metadata_maz_po_EA1,
           plant_metadata_maz_po_GA1,
           plant_metadata_maz_po_GA2,
           plant_metadata_ramian_GA1,
           plant_metadata_ramian_GA2)
  

nw_df <- lapply(df, change_names)  

combined_df_fn <- 
  bind_rows(nw_df, .id = "column_label") %>% 
  select(sort(tidyselect::peek_vars()))
  
#write.csv(combined_df_fn, file = "functional_group.csv")

fg1=read.csv("functional_group_t.csv", header = T)%>%
  clean_names()%>%
  mutate(life_span=if_else(growth_form=="Annual Forb"|growth_form=="Annual Grass ","Annual","Perennial"),
         life_form=case_when(growth_form=="Annual Forb"|growth_form=="Perennial Forb"~"Forb",
                             growth_form=="Annual Grass "|growth_form=="Perennial Grass"~"Grass",
                             growth_form=="Bushy Tree"|growth_form=="Shrub"~"Woody"))%>%
  mutate(g=str_extract(species, "^\\w+_"))
head(fg1)


floristic_list <- openxlsx::readWorkbook(xlsxFile = "Iran-Mazandaran-Javaherdeh site.xlsx",
                                                      sheet = 4,   
                                                      colNames = T) %>%
  janitor::clean_names()%>%
  select(2:5)%>%
  mutate(spec=str_extract(scientific_name_of_species, "^\\w+[:space:]+\\w+")) %>% 
  mutate(species_lw = tolower(spec)) 
  

names(fl_list)
fl_list<-floristic_list%>% 
  mutate(species = gsub( " +", "_", floristic_list$species_lw))%>%
  mutate(g=str_extract(species, "^\\w+_"))%>%
  select(c(family_name,g ))
  
head(fl_list)

meta_fg=left_join(fg1,fl_list,by="g")%>%
  mutate(legume=if_else(family_name=="Papilionaceae"|
                            family_name=="Caesalpiniaceae"|
                          family_name=="Mimosaceae"|
                          family_name=="Fabaceae"|
                          family_name=="Leguminosae","Y","N"))%>%
  distinct(., species, .keep_all=T)

table(is.na(meta_fg$family_name))


library("xlsx")

xlsx::write.xlsx(meta_fg, file="meta_fg.xlsx")
