library(tidyverse)

change_names <- function(x){
  nw_name <- str_extract(colnames(x), "^[a-z]+_[a-z]+")
  names(x) <- nw_name
  return(as.data.frame(x))
}

df <- list(df_percent_cover_kho_GA1 ,
       df_percent_cover_kho_GA2 ,
       df_percent_cover_kho_GA3 ,
       df_percent_cover_kho_GA4 , 
       df_percent_cover_masouleh_EA1 ,
       df_percent_cover_masouleh_EA2,
       df_percent_cover_masouleh_GA1 ,
       df_percent_cover_masouleh_GA2 ,
       df_percent_cover_maz_java_EA1 ,
       df_percent_cover_maz_java_GA1,  
       df_percent_cover_maz_java_GA2 ,
       df_percent_cover_maz_po_EA1 ,
       df_percent_cover_maz_po_GA1 ,
       df_percent_cover_maz_po_GA2 ,
       df_percent_cover_ramian_GA1 ,
       df_percent_cover_ramian_GA2 
       )


nw_df <- lapply(df, change_names)  

combined_df_percent_cover_nw <- 
  bind_rows(nw_df, .id = "column_label") %>% 
  select(sort(tidyselect::peek_vars()))%>%
  select(-column_label)

write.csv(combined_df_percent_cover_nw, file ="percentcover_correct.csv" )

df.bio <- list(df_biomass_kho_GA1 ,
           df_biomass_kho_GA2 ,
           df_biomass_kho_GA3 ,
           df_biomass_kho_GA4 , 
           df_biomass_masouleh_EA1 ,
           df_biomass_masouleh_EA2,
           df_biomass_masouleh_GA1 ,
           df_biomass_masouleh_GA2 ,
           df_biomass_maz_java_EA1 ,
           df_biomass_maz_java_GA1,  
           df_biomass_maz_java_GA2 ,
           df_biomass_maz_po_EA1 ,
           df_biomass_maz_po_GA1 ,
           df_biomass_maz_po_GA2 ,
           df_biomass_ramian_GA1 ,
           df_biomass_ramian_GA2 
)


nw_df_biomass <- lapply(df.bio, change_names)  

combined_df_biomass_nw <- 
  bind_rows(nw_df_biomass, .id = "column_label") %>% 
  select(sort(tidyselect::peek_vars()))%>%
  select(-column_label)

write.csv(combined_df_biomass_nw, file ="biomass_correct.csv" )
