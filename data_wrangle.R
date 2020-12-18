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


table(sort(colnames(df_extracted_names)) == sort(colnames(all_data)))
