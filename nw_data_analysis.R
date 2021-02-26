library(tidyverse)
library(vegan)
library(ape)
library(lme4)
library(lmerTest)
library(ape)
#library(dae)

df_biomass_clean <- read.csv("biomass_correct.csv",  row.names = 1)
df_pc<- read.csv("percentcover_correct.csv",  row.names = 1) 
df_rangescore <- readxl::read_xlsx("df_range_score.xlsx") 
# df_pc <- df_percentcover %>% 
#   replace(is.na(.), 0) 
# 
# 
# df_biomass_clean <- df_biomass %>% 
#   replace(is.na(.), 0)
# 
# df_pc <- df_pc %>% mutate(bs = boissiera_squarrosa + bosiera_squarrosa, cd = cardaria_draba +cardariadraba_l, 
#                  dg = dactylis_glomerata + dactylis_glomerata, gv  = galium_verum + galium_verum,
#                  lp = lolium_perrenne+ lolium_perenne,tm = tragopogon_monanus +tragopogon_montanus)%>% 
#   select(-c(boissiera_squarrosa,bosiera_squarrosa, cardaria_draba, cardariadraba_l, dactylis_glomerata, dactylis_glomerata,
#             galium_verum, galium_verum, lolium_perrenne, lolium_perenne,tragopogon_monanus, tragopogon_montanus))
# 
# df_biomass_clean <- df_biomass_clean %>% mutate(bs = boissiera_squarrosa + bosiera_squarrosa, cd = cardaria_draba +cardariadraba_l, 
#                           dg = dactylis_glomerata + dactylis_glomerata, gv  = galium_verum + galium_verum,
#                           lp = lolium_perrenne+ lolium_perenne,tm = tragopogon_monanus +tragopogon_montanus)%>% 
#   select(-c(boissiera_squarrosa,bosiera_squarrosa, cardaria_draba, cardariadraba_l, dactylis_glomerata, dactylis_glomerata,
#             galium_verum, galium_verum, lolium_perrenne, lolium_perenne,tragopogon_monanus, tragopogon_montanus))
# 
# write.csv(df_pc, "percentcover_correct.csv")
# write.csv(df_biomass_clean, "biomass_correct.csv")

metadata <- df_pc %>% 
  select(id_id) %>% 
  mutate(         treatment = ifelse(grepl("GA", .$id_id), "grazing", "exclosure"),
                  site = ifelse(grepl("kho", .$id_id), "kho",
                                ifelse(grepl("maz_po", .$id_id), "maz_po",
                                       ifelse(grepl("masouleh", .$id_id), "masouleh",
                                              ifelse(grepl("maz_java", .$id_id), "maz_java","ramian"))))) %>% 
  mutate(aspect = case_when(id_id == "masouleh_EA1" ~ "NW",
                            id_id == "masouleh_EA2" ~ "N",
                            id_id == "masouleh_GA1" ~ "SE",
                            id_id == "masouleh_GA2" ~ "NE",
                            id_id == "ramian_GA1" ~ "NW",
                            id_id == "ramian_GA2" ~ "NW",
                            id_id == "maz_java_EA1" ~ "E",
                            id_id == "maz_java_GA1" ~ "SW",
                            id_id == "maz_java_GA2" ~ "SW",
                            id_id == "maz_po_GA1" ~ "NW",
                            id_id == "maz_po_GA2" ~ "W",
                            id_id == "maz_po_EA1" ~ "flat",
                            id_id == "kho_GA1" ~ "NW",
                            id_id == "kho_GA2" ~ "SE",
                            id_id == "kho_GA3" ~ "W",
                            id_id == "kho_GA4" ~ "NE")) %>% 
  mutate(elevation = case_when(id_id == "masouleh_EA1" ~ "2216",
                               id_id == "masouleh_EA2" ~ "1950",
                               id_id == "masouleh_GA1" ~ "2216",
                               id_id == "masouleh_GA2" ~ "2188",
                               id_id == "ramian_GA1" ~ "2020",
                               id_id == "ramian_GA2" ~ "2020",
                               id_id == "maz_java_EA1" ~ "1838",
                               id_id == "maz_java_GA1" ~ "2347",
                               id_id == "maz_java_GA2" ~ "2658",
                               id_id == "maz_po_GA1" ~ "2893",
                               id_id == "maz_po_GA2" ~ "2663",
                               id_id == "maz_po_EA1" ~ "2414",
                               id_id == "kho_GA1" ~ "1860",
                               id_id == "kho_GA2" ~ "1020",
                               id_id == "kho_GA3" ~ "2153",
                               id_id == "kho_GA4" ~ "2313")) %>% 
  mutate(slope = case_when(id_id == "masouleh_EA1" ~ "60",
                           id_id == "masouleh_EA2" ~ "27",
                           id_id == "masouleh_GA1" ~ "60",
                           id_id == "masouleh_GA2" ~ "5",
                           id_id == "ramian_GA1" ~ "55.5",
                           id_id == "ramian_GA2" ~ "55.5",
                           id_id == "maz_java_EA1" ~ "10",
                           id_id == "maz_java_GA1" ~ "60",
                           id_id == "maz_java_GA2" ~ "70",
                           id_id == "maz_po_GA1" ~ "29.16",
                           id_id == "maz_po_GA2" ~ "20.6",
                           id_id == "maz_po_EA1" ~ "1",
                           id_id == "kho_GA1" ~ "10",
                           id_id == "kho_GA2" ~ "3",
                           id_id == "kho_GA3" ~ "17",
                           id_id == "kho_GA4" ~ "28")) %>% 
  mutate(avg_temp = case_when(id_id == "masouleh_EA1" ~ "23.7",
                            id_id == "masouleh_EA2" ~ "23.7",
                            id_id == "masouleh_GA1" ~ "16.6",
                            id_id == "masouleh_GA2" ~ "16.6",
                            id_id == "ramian_GA1" ~ "16.5",
                            id_id == "ramian_GA2" ~ "16.5",
                            id_id == "maz_java_EA1" ~ "13",
                            id_id == "maz_java_GA1" ~ "11",
                            id_id == "maz_java_GA2" ~ "11",
                            id_id == "maz_po_GA1" ~ "9",
                            id_id == "maz_po_GA2" ~ "9",
                            id_id == "maz_po_EA1" ~ "9.07",
                            id_id == "kho_GA1" ~ "18.5",
                            id_id == "kho_GA2" ~ "22.41",
                            id_id == "kho_GA3" ~ "16.54",
                            id_id == "kho_GA4" ~ "12")) %>% 
  mutate(avg_ppt = case_when(id_id == "masouleh_EA1" ~ "601",
                               id_id == "masouleh_EA2" ~ "601",
                               id_id == "masouleh_GA1" ~ "502",
                               id_id == "masouleh_GA2" ~ "502",
                               id_id == "ramian_GA1" ~ "450",
                               id_id == "ramian_GA2" ~ "450",
                               id_id == "maz_java_EA1" ~ "675",
                               id_id == "maz_java_GA1" ~ "670",
                               id_id == "maz_java_GA2" ~ "670",
                               id_id == "maz_po_GA1" ~ "535",
                               id_id == "maz_po_GA2" ~ "535",
                               id_id == "maz_po_EA1" ~ "535",
                               id_id == "kho_GA1" ~ "247.11",
                               id_id == "kho_GA2" ~ "75.65",
                               id_id == "kho_GA3" ~ "113.5",
                               id_id == "kho_GA4" ~ "225.9")) %>% 
  mutate(lat = case_when(id_id == "masouleh_EA1" ~ "37.176921",
                           id_id == "masouleh_EA2" ~ "37.175876",
                           id_id == "masouleh_GA1" ~ "37.169036",
                           id_id == "masouleh_GA2" ~ "37.169202",
                           id_id == "ramian_GA1" ~ "36.8395",
                           id_id == "ramian_GA2" ~ "36.8378",
                           id_id == "maz_java_EA1" ~ "36.85629",
                           id_id == "maz_java_GA1" ~ "36.866934",
                           id_id == "maz_java_GA2" ~ "36.850133",
                           id_id == "maz_po_GA1" ~ "35.897483",
                           id_id == "maz_po_GA2" ~ "35.889129",
                           id_id == "maz_po_EA1" ~ "35.861351",
                           id_id == "kho_GA1" ~ "37.4729",
                           id_id == "kho_GA2" ~ "37.8402",
                           id_id == "kho_GA3" ~ "37.7417",
                           id_id == "kho_GA4" ~ "37.295")) %>% 
  mutate(long = case_when(id_id == "masouleh_EA1" ~ "48.94019",
                          id_id == "masouleh_EA2" ~ "48.940206",
                          id_id == "masouleh_GA1" ~ " 48.918775",
                          id_id == "masouleh_GA2" ~ "48.920883",
                          id_id == "ramian_GA1" ~ "55.2557",
                          id_id == "ramian_GA2" ~ "55.2551",
                          id_id == "maz_java_EA1" ~ "50.470124",
                          id_id == "maz_java_GA1" ~ "50.448812",
                          id_id == "maz_java_GA2" ~ "50.436617",
                          id_id == "maz_po_GA1" ~ "52.010294",
                          id_id == "maz_po_GA2" ~ "52.034467",
                          id_id == "maz_po_EA1" ~ "52.060705",
                          id_id == "kho_GA1" ~ "56.2393",
                          id_id == "kho_GA2" ~ "56.7447",
                          id_id == "kho_GA3" ~ "58.0815",
                          id_id == "kho_GA4" ~ "57.0775"))  
           
names(df_biomass_clean)
which(rowSums(df_biomass_clean[, -109]) == 0)
which(rowSums(df_pc[, -109]) == 0)

df_pc <- df_pc %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>% select(-id_id)

df_biomass_clean <- df_biomass_clean %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>% select(-id_id)

metadata <- metadata %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))

df_rangescore <- df_rangescore %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))

which(rowSums(df_biomass_clean[, -109]) == 0)
which(rowSums(df_pc[, -109]) == 0)

which(is.numeric(df_pc))
###########################################################################################
############################### Alpha diversity ###########################################
###########################################################################################
div_metric <- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(as.matrix(df_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))

div_metric$evenness= div_metric$Shannon_index/log(div_metric$species_richness)

kruskal.test(div_metric$species_richness ~ metadata$treatment)
kruskal.test(div_metric$Shannon_index ~ metadata$treatment)
kruskal.test(div_metric$simpson_index ~ metadata$treatment)
kruskal.test(div_metric$evenness ~ metadata$treatment)

mod1 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))
qqnorm(resid(mod2))
qqline(resid(mod2))

mod3 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$id_id/df_rangescore$plot_indicator))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))



anova(mod1, mod2, mod3)
#########################################################################################################
########                        Simpson index
##########################################################################################
mod1 <- lmer((div_metric$simpson_index) ~ metadata$treatment + (1|metadata$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer(log1p(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer(log1p(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$id_id) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))


#-------------berger-parker--------------------
library("diverse")

bg=diversity(t(df_pc), type='berger-parker', category_row=T)%>%
  arrange(rownames(.))

kruskal.test(bg$berger.parker.D ~ metadata$treatment)

mod1 <- lmer((bg$berger.parker.D) ~ metadata$treatment + (1|metadata$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer((bg$berger.parker.D) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer((bg$berger.parker.D) ~ metadata$treatment + (1|metadata$id_id) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))



#########################################################################################
###################   productivity vs diversity #######################################'
######################################################################################

bm <- div_metric %>% 
  mutate(trt = metadata$treatment) %>% 
  filter(biomass < 5000)


bm_nw <- div_metric_new%>% 
  mutate(trt = metadata$treatment) %>% 
  filter(biomass < 5000)

ggplot(bm, aes(x = Shannon_index, y = scale(biomass, center = T), color = trt)) +
  geom_point(aes(x = Shannon_index, y = biomass), alpha = 0.4 ) +
  geom_smooth(method = "gam") +theme_bw()

ggplot(bm, aes(x = Shannon_index, y = biomass, color = trt)) +
  geom_point(aes(x = Shannon_index, y = biomass), alpha = 0.4 ) +
  geom_smooth(method = "gam") +theme_bw()

ggplot(bm , aes(x = simpson_index, y = biomass,  color = trt)) +
  geom_point(aes(x = simpson_index, y = biomass), alpha = 0.4) +
  geom_smooth(method = "gam")

ggplot(bm , aes(x =  species_richness, y = biomass,  color = trt)) +
  geom_point(aes(x =  species_richness, y = biomass), alpha = 0.4) +
  geom_smooth(method = "gam")


##############################################################################################

normalization <- function(x){ 
  p = (x - min(x))/(max(x) - min(x))
  return (p)
}

test_pc <- lapply(df_pc, normalization) %>% 
  as.data.frame()

for (i in 1:ncol(test_pc)){
  q=range(test_pc[,i])
  print(q)
}



div_metric_new <- data.frame(
  species_richness = specnumber(test_pc),
  shannon_index = diversity(as.matrix(test_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(test_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))



boxplot(div_metric_new$species_richness ~ metadata$treatment)
boxplot(div_metric_new$shannon_index ~ metadata$treatment)
boxplot(div_metric_new$simpson_index ~ metadata$treatment)


boxplot(div_metric$species_richness ~ metadata$treatment)
boxplot(div_metric$Shannon_index ~ metadata$treatment)
boxplot(div_metric$simpson_index ~ metadata$treatment)


kruskal.test(div_metric_new$species_richness ~ metadata$treatment)
kruskal.test(div_metric_new$shannon_index ~ metadata$treatment)
kruskal.test(div_metric_new$simpson_index ~ metadata$treatment)



nw_pc <- scale(df_pc, center = T)

div_metric_new_new <- data.frame(
  species_richness = specnumber(nw_pc),
  shannon_index = diversity(as.matrix(nw_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(nw_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))



ggplot(bm_nw, aes(x = shannon_index, y = biomass, color = trt)) +
  geom_point(aes(x = shannon_index, y = biomass), alpha = 0.4 ) +
  geom_smooth(method = "lm") +theme_bw()

ggplot(bm_nw, aes(x = shannon_index, y = biomass, color = trt)) +
  geom_point(aes(x = shannon_index, y = biomass), alpha = 0.4 ) +
  geom_smooth(method = "lm") +theme_bw()

ggplot(bm_nw , aes(x = simpson_index, y = biomass,  color = trt)) +
  geom_point(aes(x = simpson_index, y = biomass), alpha = 0.4) +
  geom_smooth(method = "lm")

ggplot(bm_nw , aes(x =  species_richness, y = biomass,  color = trt)) +
  geom_point(aes(x =  species_richness, y = biomass), alpha = 0.4) +
  geom_smooth(method = "lm")



ggplot(bm, aes(x = biomass, y = trt, color = trt)) +
  geom_point(aes(x = biomass, y = trt), alpha = 0.4 ) +
  geom_smooth(method = "lm") +theme_bw()

ggplot(bm, aes(x = biomass, y = trt, color = trt)) +
  geom_point(aes(x = biomass, y = trt), alpha = 0.4 ) +
  geom_smooth(method = "lm") +theme_bw()

ggplot(bm, aes(x = biomass, y = trt,  color = trt)) +
  geom_point(aes(x = biomass, y = trt), alpha = 0.4) +
  geom_smooth(method = "lm")




bm_nw$biomass <- (bm_nw$biomass - min(bm_nw$biomass))/(max(bm_nw$biomass) - min(bm_nw$biomass))
range(bm_nw$biomass)

table(bm_nw$trt)

###############################################################################################
##################   Beta - diversity #########################################################
###############################################################################################


dist_bray <- vegdist(df_pc)

set.seed(1111)
beta_div <- metaMDS(dist_bray, k =2, trymax = 999)


# biomass vs dominance ----------------------------------------------------

mod_PD <- aov(sqrt(div_metric$biomass) ~ div_metric$simpson_index * metadata$treatment +Error(metadata$id_id))
(mod_PD)
plot(residuals.aovlist(mod_PD))
shapiro.test(resid(mod_PD))
hist(div_metric$biomass)

plot(div_metric$biomass ~ div_metric$simpson_index)

ratio <- div_metric$biomass / div_metric$simpson_index

ggplot(div_metric, aes(y= ratio, x = treatment)) +
  geom_point() +
  geom_smooth(method = "gam" )

which(ratio >10000)


kruskal.test(ratio ~ metadata$treatment)
boxplot(ratio ~ metadata$treatment)

###Lets run Random forest model with plants as predictors and treatment as the response variables....

library(randomForest)

df_rf<-data.frame(response=metadata$treatment, df_pc)

names(df_rf)

smp_size <- floor(0.75 * nrow(df_rf))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_rf)), size = smp_size)
train <- df_rf[train_ind, ]
test <- df_rf[-train_ind, ]
rf_classifier = randomForest(response ~ ., data=train
                             , ntree=500, mtry=2, importance=TRUE)

rf_classifier

varImpPlot(rf_classifier)

# prediction_for_table <- predict(rf_classifier,test)
# 
# table(observed=test[,5],predicted=prediction_for_table)
# 
# names(test)

nw_metric <- data.frame(nw_metric = div_metric$simpson_index/div_metric$biomass, 
                                                 div_metric,
                                                 treatment = metadata$treatment,
                                                 id = metadata$id_id) %>% 
     drop_na()
mod_PD <- aov(log1p(nw_metric) ~ treatment+ Error(id),data = nw_metric)

### let create the metadata information from aspect, slope, MAP, MAT and then overlay that information on
### beta diversity
