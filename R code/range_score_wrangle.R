library(tidyverse)
library(janitor)
library(readxl)


###############################################
###############  Masouleh ####################
##############################################



###Excel sheet number 1##
#########################

##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_masouleh_EA1 <- read_xlsx("Iran-Gillan-Masouleh_update.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(species_names,range_score, range_class)) %>% 
  mutate(id_id = rep("masouleh_EA1", times = nrow(.)))%>%
  rename(plot_indicator=species_names)

names(df_range_score_masouleh_EA1)


###Excel sheet number 2##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_masouleh_EA2 <- read_xlsx("Iran-Gillan-Masouleh_update.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(species_names,range_score, range_class)) %>% 
  mutate(id_id = rep("masouleh_EA2", times = nrow(.)))%>%
  rename(plot_indicator=species_names)



###Excel sheet number 3##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_masouleh_GA1 <- read_xlsx("Iran-Gillan-Masouleh_update.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(x1,range_score, range_class)) %>% 
  mutate(id_id = rep("masouleh_GA1", times = nrow(.)))%>%
  rename(plot_indicator=x1)


names(df_range_score_masouleh_GA1)

###Excel sheet number 4##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_masouleh_GA2 <- read_xlsx("Iran-Gillan-Masouleh_update.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(species_names,range_score, range_class)) %>% 
  mutate(id_id = rep("masouleh_GA2", times = nrow(.)))%>%
  rename(plot_indicator=species_names)




###############################################
###############  Ramian ######################
##############################################

###Excel sheet number 1##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_ramian_GA1 <- read_xlsx("Iran-Golestan-Ramian_update.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(x1,range_score, range_class)) %>% 
  mutate(id_id = rep("ramian_GA1", times = nrow(.)))%>%
  rename(plot_indicator=x1)




###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_ramian_GA2 <- read_xlsx("Iran-Golestan-Ramian_update.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(species_names,range_score, range_class)) %>% 
  mutate(id_id = rep("ramian_GA2", times = nrow(.)))%>%
  rename(plot_indicator=species_names)


############################################################
###############  Mazandaran-Javaherdeh  ####################
############################################################

###Excel sheet number 1##
#########################


##Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_java_EA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site_update.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_java_EA1", times = nrow(.)))

###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_java_GA1 <- read_xlsx("Iran-Mazandaran-Javaherdeh site_update.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_java_GA1", times = nrow(.)))





###Excel sheet number 3##
#########################

#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_java_GA2 <- read_xlsx("Iran-Mazandaran-Javaherdeh site_update.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_java_GA2", times = nrow(.)))




############################################################
###############  Mazandaran-Polour ####################
############################################################


###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_po_GA1 <- read_xlsx("Iran-Mazandaran-Polour_update.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(x1,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_po_GA1", times = nrow(.)))%>%
  rename(plot_indicator=x1)




###Excel sheet number 2##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_po_GA2 <- read_xlsx("Iran-Mazandaran-Polour_update.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(x1,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_po_GA2", times = nrow(.)))%>%
  rename(plot_indicator=x1)





###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_maz_po_EA1 <- read_xlsx("Iran-Mazandaran-Polour_update.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(x1,range_score, range_class)) %>% 
  mutate(id_id = rep("maz_po_EA1", times = nrow(.)))%>%
  rename(plot_indicator=x1)




############################################################
###############  Khorasan ##################################
############################################################



###Excel sheet number 1##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the excel
df_range_score_kho_GA1 <- read_xlsx("Iran-North Khorasan_update.xlsx", sheet = 1, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("kho_GA1", times = nrow(.)))



###Excel sheet number 2##
#########################


#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_range_score_kho_GA2 <- read_xlsx("Iran-North Khorasan_update.xlsx", sheet = 2, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("kho_GA2", times = nrow(.)))



###Excel sheet number 3##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_range_score_kho_GA3 <- read_xlsx("Iran-North Khorasan_update.xlsx", sheet = 3, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("kho_GA3", times = nrow(.)))



###Excel sheet number 4##
#########################



#Importing data one sheet at a time. Each df is one unique sheet in the exce
df_range_score_kho_GA4 <- read_xlsx("Iran-North Khorasan_update.xlsx", sheet = 4, cell_rows(10:74)) %>% 
  janitor::clean_names() %>%  
  select(c(plot_indicator,range_score, range_class)) %>% 
  mutate(id_id = rep("kho_GA4", times = nrow(.)))

change_names <- function(x){
  nw_name <- str_extract(colnames(x), "^[a-z]+_[a-z]+")
  names(x) <- nw_name
  return(as.data.frame(x))
}

df <- rbind(df_range_score_kho_GA1 ,
           df_range_score_kho_GA2 ,
           df_range_score_kho_GA3 ,
           df_range_score_kho_GA4 , 
           df_range_score_masouleh_EA1 ,
           df_range_score_masouleh_EA2,
           df_range_score_masouleh_GA1 ,
           df_range_score_masouleh_GA2 ,
           df_range_score_maz_java_EA1 ,
           df_range_score_maz_java_GA1,  
           df_range_score_maz_java_GA2 ,
           df_range_score_maz_po_EA1 ,
           df_range_score_maz_po_GA1 ,
           df_range_score_maz_po_GA2 ,
           df_range_score_ramian_GA1 ,
           df_range_score_ramian_GA2 
)


write.xlsx(df,"df_range_score.xlsx")
