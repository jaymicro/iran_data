
df_biomass_clean <- read.csv("biomass_correct.csv",  row.names = 1)

df_biomass_clean <- df_biomass_clean %>% 
  replace(is.na(.), 0) 

df_biomass_clean <- df_biomass_clean %>% 
  mutate(bs = .$boissiera_squarrosa + .$bosiera_squarrosa,
         cd = .$cardaria_draba + .$cardariadraba_l,
         dg = .$dactylis_glomerata + .$dactylis_golmerata,
         gv = .$galium_verum + .$gallium_verum,
         lp = .$lolium_perenne + .$lolium_perrenne,
         tm = .$tragopogon_monanus + .$tragopogon_montanus) %>% 
  select(-c("boissiera_squarrosa", "bosiera_squarrosa",
            "cardaria_draba", "cardariadraba_l",
            "dactylis_glomerata", "dactylis_golmerata",
            "galium_verum", "gallium_verum",
            "lolium_perenne", "lolium_perrenne",
            "tragopogon_monanus", "tragopogon_montanus")) %>% 
  write.csv("biomass_March1.csv")



df_pc <- read.csv("percentcover_correct.csv",  row.names = 1)%>% 
  replace(is.na(.), 0) %>% 
  mutate(bs = .$boissiera_squarrosa + .$bosiera_squarrosa,
         cd = .$cardaria_draba + .$cardariadraba_l,
         dg = .$dactylis_glomerata + .$dactylis_golmerata,
         gv = .$galium_verum + .$gallium_verum,
         lp = .$lolium_perenne + .$lolium_perrenne,
         tm = .$tragopogon_monanus + .$tragopogon_montanus) %>% 
  select(-c("boissiera_squarrosa", "bosiera_squarrosa",
            "cardaria_draba", "cardariadraba_l",
            "dactylis_glomerata", "dactylis_golmerata",
            "galium_verum", "gallium_verum",
            "lolium_perenne", "lolium_perrenne",
            "tragopogon_monanus", "tragopogon_montanus")) %>% 
  write.csv("percentcover_March1.csv")
