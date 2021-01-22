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
       df_percent_cover_masouleh_EA1)

nw_df <- lapply(df, change_names)  

