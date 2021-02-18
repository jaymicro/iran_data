library(tidyverse)
library(vegan)
library(ape)
library(lme4)
library(lmerTest)
library(ape)


df_percentcover<- read.csv("percentcover_correct.csv",  row.names = 1)
df_biomass<- read.csv("biomass_correct.csv",  row.names = 1)

df_pc <- df_percentcover %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))


df_biomass_clean <- df_biomass %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))

which(rowSums(df_biomass_clean) ==0)
which(rowSums(df_pc) ==0)

metadata <- df_percentcover %>% 
  select(id_id) %>% 
  mutate(         treatment = ifelse(grepl("GA", .$id_id), "grazing", "exclosure"),
                  site = ifelse(grepl("kho", .$id_id), "kho",
                                ifelse(grepl("maz_po", .$id_id), "maz_po",
                                       ifelse(grepl("masouleh", .$id_id), "masouleh",
                                              ifelse(grepl("maz_java", .$id_id), "maz_java","ramian"))))) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))



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

mod1 <- lmer((div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer((div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer((div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$id_id) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))

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

#########################################################################################################
##################   Beta - diversity #########################################################
############################################################################################


dist_bray <- vegdist(df_pc)

set.seed(1111)
beta_div <- metaMDS(dist_bray, k =2, trymax = 999)


# biomass vs dominance ----------------------------------------------------

mod_PD <- lmer(div_metric$biomass ~ div_metric$simpson_index + (1|metadata$id_id))
plot(resid(mod_PD))



