library(tidyverse)
library(vegan)
library(ape)
library(lme4)
library(lmerTest)
library(factoextra)


df_percentcover<- read.csv("combined_df_percent_cover.csv",  row.names = 1)
df_biomass<- read.csv("combined_df_biomass.csv",  row.names = 1)

names(df_biomass)
df_pc <- df_percentcover %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))


df_biomass_clean <- df_biomass %>% 
  replace(is.na(.), 0) %>% 
  select(-c(id_id)) %>% 
  slice(-c(489,505,128, 192, 256, 576, 704,768,832,896,960,1024))

names(df_biomass_clean)

df_biomass_clean$stipa_lessingiana[df_biomass_clean$stipa_lessingiana > 5000] <- 5.88

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



#--------remove sites that do not have exclosure
df_pc_all<-data.frame(df_pc, metadata)

df_pc_pairs=df_pc_all%>%
  filter(site!="ramian")

names(df_pc_pairs)

###########################################################################################
############################### Alpha diversity ###########################################
###########################################################################################
div_metric <- data.frame(
  species_richness = specnumber(df_pc_pairs[,1:210]),
  Shannon_index = diversity(as.matrix(df_pc_pairs[,1:210]), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc_pairs[,1:210]), index = "simpson", MARGIN = 1, base = exp(1)))


kruskal.test(div_metric$species_richness ~ df_pc_pairs$treatment)
kruskal.test(div_metric$Shannon_index ~ df_pc_pairs$treatment)
kruskal.test(div_metric$simpson_index ~ df_pc_pairs$treatment)

mod1 <- lmer((div_metric$Shannon_index) ~ df_pc_pairs$treatment + (1|df_pc_pairs$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer((div_metric$Shannon_index) ~ df_pc_pairs$treatment + (1|df_pc_pairs$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer((div_metric$Shannon_index) ~ df_pc_pairs$treatment + (1|df_pc_pairs$id_id) + (1|df_pc_pairs$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))

#########################################################################################################
########                        Simpson index
##########################################################################################
mod1 <- lmer(div_metric$simpson_index ~ df_pc_pairs$treatment + (1|df_pc_pairs$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer(log1p(div_metric$simpson_index) ~ df_pc_pairs$treatment + (1|df_pc_pairs$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer(log1p(div_metric$simpson_index) ~ df_pc_pairs$treatment + (1|df_pc_pairs$id_id) + (1|df_pc_pairs$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))


#####---------------slope, aspect, elevation-----------------

metadata_new<-metadata%>%
  mutate(aspect=case_when(id_id=="kho_GA1"~"NW",
                          id_id=="kho_GA2"~"SE",
                          id_id=="kho_GA3"~"W",
                          id_id=="kho_GA4"~"NE",
                          id_id=="masouleh_EA1"~"N",
                          id_id=="masouleh_GA1"~"",
                          id_id=="masouleh_GA2"~"",
                          id_id=="maz_java_EA1"~"",
                          id_id=="maz_java_GA1"~"",
                          id_id=="maz_java_GA2"~"",
                          id_id=="maz_po_EA1"~"",
                          id_id=="maz_po_GA1"~""))

levels(metadata$id_id)
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


pca_values=prcomp(t(df_pc), scale = FALSE)

PC1=pca_values$rotation[,1]
PC2=pca_values$rotation[,2]

PC_1_2=data.frame(PC1,PC2,trt=metadata$treatment,site=metadata$site)


ggplot(PC_1_2, aes(x = PC1, y = PC2, color = trt, shape=site)) +
  geom_point(alpha = 0.4 ) +
  theme_bw()+
  stat_ellipse()




pca_values=prcomp(t(df_pc_pairs[,1:210]), scale = FALSE)

PC1=pca_values$rotation[,1]
PC2=pca_values$rotation[,2]

PC_1_2=data.frame(PC1,PC2,df_pc_pairs)


ggplot(PC_1_2, aes(x = PC1, y = PC2, color = treatment, shape=site)) +
  geom_point(alpha = 0.4 ) +
  theme_bw()+
  stat_ellipse()


library("factoextra")
get_eigenvalue(pca_values)


dist_euc <- vegdist(df_pc_pairs[,1:210], method = "euclidean")

set.seed(124)

per_mod <-adonis(dist_euc ~ df_pc_pairs$treatment )

print(per_mod)

set.seed(123)
plot(anosim(dist_euc, df_pc_pairs$treatment))



  
kruskal.test()
  

