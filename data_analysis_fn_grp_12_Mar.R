#Functional Group analysis
# Import packages ---------------------------------------------------------

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)



# Import data -------------------------------------------------------------

df_biomass_clean <- read.csv("biomass_12_Mar.csv",  row.names = 1) 


df_pc<- read.csv("pc_12_Mar.csv",  row.names = 1) 

df_rangescore <- read.csv("df_range_score.csv", row.names = 1)

metadata<- read.csv("metadata.csv") 

fn_grp <- readxl::read_xlsx("meta_fg.xlsx")


# Data analysis -----------------------------------------------------------


# Native non-native data wrangle -------------------------------------------------------

native <- which((fn_grp$exotic == "N"))
exotic <- which((fn_grp$exotic == "Y"))

##### native #####
native_cover <- df_pc %>% 
  select(all_of(native))

native_biomass <-  df_biomass_clean %>% 
  select(all_of(native))

##### exotic #####

exotic_cover <- df_pc %>% 
  select(all_of(exotic))

exotic_biomass <-  df_biomass_clean %>% 
  select(all_of(exotic))



# native not native data analysis -----------------------------------------
bm <- rowSums(df_biomass_clean)
pc <- rowSums(df_pc)

mod.native.biomass <- lmer(sqrt(rowSums(native_biomass)/bm) ~ metadata$treatment * metadata$site + (1|metadata$grid))
hist(resid(mod.native.biomass))
shapiro.test(resid(mod.native.biomass))
plot(resid(mod.native.biomass))
anova(mod.native.biomass )


mod.native.cover <- lmer(sqrt(rowSums(native_cover)/pc) ~ metadata$treatment * metadata$site +  (1|metadata$grid))
hist(resid(mod.native.cover))
shapiro.test(resid(mod.native.cover))
plot(resid(mod.native.cover))
anova(mod.native.cover)


# exotic ------------------------------------------------------------------
mod.exotic.biomass <- lmer(sqrt(rowSums(exotic_biomass)/bm) ~ metadata$treatment * metadata$site + (1|metadata$grid))
hist(resid(mod.exotic.biomass))
shapiro.test(resid(mod.exotic.biomass))
plot(resid(mod.exotic.biomass))
anova(mod.exotic.biomass )


mod.exotic.cover <- lmer(sqrt(rowSums(exotic_cover)) ~ metadata$treatment * metadata$site + (1|metadata$grid))
hist(resid(mod.exotic.cover))
shapiro.test(resid(mod.exotic.cover))
plot(resid(mod.exotic.cover))
anova(mod.exotic.cover)


# ratio exotic/native -----------------------------------------------------



exo_nativ <- lmer(sqrt(rowSums(exotic_biomass)/rowSums(native_biomass)) ~  
                    metadata$treatment * metadata$site + (1|metadata$grid))

hist(resid(exo_nativ))
plot(resid(exo_nativ))
range(rowSums(exotic_biomass)/rowSums(native_biomass))
anova(exo_nativ)

multcomp::cld(emmeans::emmeans(exo_nativ,~treatment* site))
plot(multcomp::cld(emmeans::emmeans(exo_nativ, ~ treatment)))
multcomp::cld(emmeans::emmeans(exo_nativ, "treatment", by = c("site")))


exo_nativ_cover <- lmer(log1p(rowSums(exotic_cover)/rowSums(native_cover)) ~  
                    metadata$treatment * metadata$site + (1|metadata$grid))

hist(resid(exo_nativ_cover))
plot(resid(exo_nativ_cover))
range(rowSums(exotic_cover)/rowSums(native_cover))
anova(exo_nativ_cover)

kruskal.test(rowSums(exotic_cover)/rowSums(native_cover) ~ metadata$treatment)
boxplot(rowSums(exotic_cover)/rowSums(native_cover) ~ metadata$treatment)

# Forbs and grasses -------------------------------------------------------


forb_inc_legume <- which((fn_grp$life_form == "Forb"))

forb_no_legume <- which((fn_grp$life_form == "Forb" & fn_grp$legume=="N"))


forb_inc_legume_pc <- df_pc %>% 
  select(all_of(forb_inc_legume))

forb_inc_legume_biomass <- df_biomass_clean %>% 
  select(all_of(forb_inc_legume))

forb_no_legume_pc <- df_pc %>% 
  select(all_of(forb_no_legume))

forb_no_legume_biomass <- df_biomass_clean %>% 
select(all_of(forb_no_legume))


Grass <- which((fn_grp$life_form == "Grass"))

Grass_pc <- df_pc %>% 
  select(all_of(Grass))

Grass_biomass <- df_biomass_clean %>% 
  select(all_of(Grass))

legume_inc_shrub <- which(fn_grp$legume=="Y")
legume_no_shrub <- which(fn_grp$legume=="Y" & fn_grp$life_form=="Forb")


legume_inc_shrub_pc <- df_pc %>% 
  select(all_of(legume_inc_shrub ))

legume_inc_shrub_biomass <- df_biomass_clean %>% 
  select(all_of(legume_inc_shrub ))


legume_no_shrub_pc <- df_pc %>% 
  select(all_of(legume_no_shrub ))

legume_no_shrub_biomass <- df_biomass_clean %>% 
  select(all_of(legume_no_shrub ))


# diversity of forbs and grasses ------------------------------------------

forb_inc_legume_a_div <- data.frame(
  forb_shan = diversity(forb_inc_legume_pc, index = "shannon"),
  forb_simp = diversity(forb_inc_legume_pc, index = "simpson"),
  forb_rich = specnumber(forb_inc_legume_pc))

forb_no_legume_a_div <- data.frame(
  forb_shan = diversity(forb_no_legume_pc, index = "shannon"),
  forb_simp = diversity(forb_no_legume_pc, index = "simpson"),
  forb_rich = specnumber(forb_no_legume_pc))


Grass_a_div <- data.frame(
  Grass_shan = diversity(Grass_pc, index = "shannon"),
  Grass_simp = diversity(Grass_pc, index = "simpson"),
  Grass_rich = specnumber(Grass_pc))


legume_inc_shrub_a_div <- data.frame(
  legume_shan = diversity(legume_inc_shrub_pc, index = "shannon"),
  legume_simp = diversity(legume_inc_shrub_pc, index = "simpson"),
  legume_rich = specnumber(legume_inc_shrub_pc))

legume_no_shrub_a_div <- data.frame(
  legume_shan = diversity(legume_no_shrub_pc, index = "shannon"),
  legume_simp = diversity(legume_no_shrub_pc, index = "simpson"),
  legume_rich = specnumber(legume_no_shrub_pc))

# Analyzing forbs ---------------------------------------------------------

#------forb including legumes

#shannon
forb_inc_legume_mod1  <- lmer(exp(forb_inc_legume_a_div$forb_shan) ~  metadata$treatment *(metadata$site)+ (1|metadata$grid) + (1|metadata$site))
plot(forb_inc_legume_mod1)
plot(resid(forb_inc_legume_mod1))
lattice::qqmath(forb_inc_legume_mod1)
shapiro.test(resid(forb_inc_legume_mod1))
anova(forb_inc_legume_mod1)

boxplot(forb_inc_legume_a_div$forb_shan ~ metadata$treatment*metadata$site)
kruskal.test(forb_inc_legume_a_div$forb_shan ~ metadata$treatment)


forb_inc_legume_mod2  <- lmer(exp(forb_inc_legume_a_div$forb_shan) ~  df_rangescore$range_score *(metadata$site)+ (1|metadata$grid) + (1|metadata$site))
plot(forb_inc_legume_mod2)
plot(resid(forb_inc_legume_mod2))
lattice::qqmath(forb_inc_legume_mod2)
shapiro.test(resid(forb_inc_legume_mod2))
anova(forb_inc_legume_mod2)

boxplot(forb_inc_legume_a_div$forb_shan ~ metadata$treatment*metadata$site)
kruskal.test(forb_inc_legume_a_div$forb_shan ~ metadata$treatment)
#richness
forb_inc_legume_mod2  <- glmer((forb_inc_legume_a_div$forb_rich) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) , family ="poisson")
Anova(forb_inc_legume_mod2)


boxplot(forb_inc_legume_a_div$forb_rich ~ metadata$treatment*metadata$site)

#----forb no legume

#shannon
forb_no_legume_mod1  <- lmer(exp(forb_no_legume_a_div$forb_shan) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(forb_no_legume_mod1)
plot(resid(forb_no_legume_mod1))
lattice::qqmath(forb_no_legume_mod1)
shapiro.test(resid(forb_no_legume_mod1))
anova(forb_no_legume_mod1)

boxplot(forb_no_legume_a_div$forb_shan ~ metadata$treatment*metadata$site)


kruskal.test(forb_no_legume_a_div$forb_shan ~ metadata$treatment)

#richness
forb_no_legume_mod2  <- glmer((forb_no_legume_a_div$forb_rich) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) , family ="poisson")
Anova(forb_no_legume_mod2)


boxplot(forb_no_legume_a_div$forb_rich ~ metadata$treatment*metadata$site)


#  grass ---------------------------------------------------------

#shannon
Grass_mod1  <- lmer(exp(Grass_a_div$Grass_shan) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(Grass_mod1)
plot(resid(Grass_mod1))

lattice::qqmath(Grass_mod1)
shapiro.test(resid(Grass_mod1))
anova(Grass_mod1)
hist(Grass_a_div$Grass_shan)

boxplot(Grass_a_div$Grass_shan ~ metadata$treatment*metadata$site)
kruskal.test(Grass_a_div$Grass_shan ~ metadata$treatment)

#richness
Grass_mod2  <- glmer((Grass_a_div$Grass_rich) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) , family ="poisson")
Anova(Grass_mod2)


boxplot(Grass_a_div$Grass_rich ~ metadata$treatment*metadata$site)


# legume ------------------------------------------------------------------

#no shrubs
#shannon 
legume_no_shrub_mod1  <- lmer(exp(legume_no_shrub_a_div$legume_shan) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(legume_no_shrub_mod1)
plot(resid(legume_no_shrub_mod1))
lattice::qqmath(legume_no_shrub_mod1)
shapiro.test(resid(legume_no_shrub_mod1))
anova(legume_no_shrub_mod1)

boxplot(legume_no_shrub_a_div$legume_shan ~ metadata$treatment*metadata$site)
kruskal.test(legume_no_shrub_a_div$legume_shan ~ metadata$treatment)

#richness
legume_no_shrub_mod2  <- glmer((legume_no_shrub_a_div$legume_rich) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), family ="poisson")
summary(legume_no_shrub_mod2)


boxplot(legume_no_shrub_a_div$legume_rich ~ metadata$treatment)

#legume with shrubs
legume_inc_shrub_mod1  <- lmer(exp(legume_inc_shrub_a_div$legume_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(legume_inc_shrub_mod1)
plot(resid(legume_inc_shrub_mod1))
lattice::qqmath(legume_inc_shrub_mod1)
shapiro.test(resid(legume_inc_shrub_mod1))
anova(legume_inc_shrub_mod1)

boxplot(legume_inc_shrub_a_div$legume_shan ~ metadata$treatment)
kruskal.test(legume_inc_shrub_a_div$legume_shan ~ metadata$treatment)


#richness
legume_inc_shrub_mod2  <- glmer((legume_inc_shrub_a_div$legume_rich) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), family ="poisson")
summary(legume_inc_shrub_mod2)


boxplot(legume_inc_shrub_a_div$legume_rich ~ metadata$treatment)



# Exotic vs native analysis -----------------------------------------------

######Exotic############
Exotic <- which((fn_grp$exotic == "Y"))

Exotic_pc <- df_pc %>% 
  select(all_of(Exotic))

Exotic_biomass <- df_biomass_clean %>% 
  select(all_of(Exotic))


Exotic_a_div <- data.frame(
  Exotic_shan_prop = diversity(Exotic_pc, index = "shannon"),
  Exotic_simp_prop = diversity(Exotic_pc, index = "simpson"),
  Exotic_rich_prop = specnumber(Exotic_pc))

###poor residuals for all alpha diversity metric
Exotic_mod1  <- lmer(log1p(Exotic_a_div$Exotic_simp) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(Exotic_mod1)
plot(resid(Exotic_mod1))

lattice::qqmath(Exotic_mod1)
shapiro.test(resid(Exotic_mod1))
anova(Exotic_mod1)
hist(Exotic_a_div$Exotic_shan)

boxplot(Exotic_a_div$Exotic_shan ~ metadata$treatment*metadata$site)
kruskal.test(Exotic_a_div$Exotic_shan ~ metadata$treatment)

#Proportion of exotic biomass

prop_exotic_biomass <- rowSums(Exotic_biomass)/rowSums(df_biomass_clean)
quantile(prop_exotic_biomass)


mod_exo_biomass <- lmer(exp(prop_exotic_biomass) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(mod_exo_biomass)
plot(resid(mod_exo_biomass))

lattice::qqmath(mod_exo_biomass)
shapiro.test(resid(mod_exo_biomass))
anova(mod_exo_biomass)
hist(resid(mod_exo_biomass))

boxplot(prop_exotic_biomass ~ metadata$treatment*metadata$site)
kruskal.test(Exotic_a_div$Exotic_shan ~ metadata$treatment)

####native

Native <- which((fn_grp$exotic == "N"))

Native_pc <- df_pc %>% 
  select(all_of(Native))

Native_biomass <- df_biomass_clean %>% 
  select(all_of(Native))


Native_a_div <- data.frame(
  Native_shan_prop = diversity(Native_pc, index = "shannon"),
  Native_simp_prop = diversity(Native_pc, index = "simpson"),
  Native_rich_prop = specnumber(Native_pc))

###poor residuals for all alpha diversity metric
Native_mod1  <- glmer((Native_a_div$Native_rich_prop) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid), family = "poisson" )
plot(Native_mod1)
plot(resid(Native_mod1))

lattice::qqmath(Native_mod1)

shapiro.test(resid(Native_mod1))
car::Anova(Native_mod1, test = "F")
hist(Native_a_div$Native_shan)

boxplot(Native_a_div$Native_rich_prop ~ metadata$treatment*metadata$site)


kruskal.test(Native_a_div$Native_rich_prop ~ metadata$treatment)

##native vs treatment & exotic

native_exo_tret  <- lmer(Native_a_div$Native_rich_prop ~  Exotic_a_div$Exotic_rich* metadata$treatment  + (1|metadata$grid) + (1|metadata$site))
plot(native_exo_tret)
plot(resid(native_exo_tret))
car::vif(native_exo_tret)
lattice::qqmath(native_exo_tret)

shapiro.test(resid(native_exo_tret))
car::Anova(native_exo_tret,type=3)
summary(native_exo_tret)
hist(Native_a_div$Native_shan)

boxplot(Native_a_div$Native_rich_prop ~ metadata$treatment)
kruskal.test(Native_a_div$Native_rich_prop ~ metadata$treatment)


plt_df <- data.frame(nat_ric = Native_a_div$Native_rich_prop,
                     exo_ric = Exotic_a_div$Exotic_rich,
                     treat = metadata$treatment)
ggplot(plt_df, aes(x = treat, y = nat_ric, color = treat))+
  geom_boxplot() +
  ggpubr::stat_compare_means(method = "kruskal")

ggplot(plt_df, aes(x = treat, y = exo_ric, color = treat))+
  geom_boxplot() +
  ggpubr::stat_compare_means(method = "kruskal")



# Palatibilty -------------------------------------------------------------

palatable_i <- which((fn_grp$class == "I"))

palatable_i_pc <- df_pc %>% 
  select(all_of(palatable_i))

palatable_i_biomass <- df_biomass_clean %>% 
  select(all_of(palatable_i))


palatable_i_a_div <- data.frame(
  palatable_i_shan = diversity(palatable_i_pc, index = "shannon"),
  palatable_i_simp = diversity(palatable_i_pc, index = "simpson"),
  palatable_i_rich = specnumber(palatable_i_pc))


palatable_mod  <- lmer(palatable_i_biomass ~  metadata$treatment  + (1|metadata$grid) + (1|metadata$site))
plot(palatable_mod)
plot(resid(palatable_mod))
car::vif(palatable_mod)
lattice::qqmath(palatable_mod)

#percent cover

#Proportion of exotic cover

prop_exotic_cover <- rowSums(Exotic_pc)/rowSums(df_pc)

mod_exo_cover <- lmer(sqrt(prop_exotic_cover) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(mod_exo_cover)
plot(resid(mod_exo_cover))


#Proportion of Native cover

prop_Native_cover <- rowSums(Native_pc)/rowSums(df_pc)

mod_nat_cover <- lmer(exp(prop_Native_cover) ~  metadata$treatment * (metadata$site)+ (1|metadata$grid) )
plot(mod_nat_cover)
plot(resid(mod_nat_cover))
anova(mod_nat_cover)



