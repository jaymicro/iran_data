
# Import packages ---------------------------------------------------------

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)



# Import data -------------------------------------------------------------

df_biomass_clean <- read.csv("biomass_12_Mar.csv",  row.names = 1) 


df_pc<- read.csv("pc_12_Mar.csv",  row.names = 1) 

df_rangescore <- read.csv("df_range_score_12_Mar.csv", row.names = 1)

metadata<- read.csv("meta_12_Mar.csv")

fn_grp <- readxl::read_xlsx("meta_fg.xlsx")


# Data analysis -----------------------------------------------------------


# Shannon index -----------------------------------------------------------


div_metric <- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(as.matrix(df_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))

mod4 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$grid) + (1|metadata$site))

anova(mod4)
plot(resid(mod4))
shapiro.test(resid(mod4))
lattice::qqmath(mod4)
boxplot(div_metric$Shannon_index ~ metadata$treatment)


# Simpson index -----------------------------------------------------------

mod3 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$grid) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
hist(resid(mod3))

shapiro.test(resid(mod3))

kruskal.test(div_metric$simpson_index ~ metadata$treatment)
boxplot(div_metric$simpson_index ~ metadata$treatment)


# Productivity ------------------------------------------------------------
pd_mod1 <- lmer(sqrt(bm) ~ treatment + (1|site) + (1|grid), metadata, REML = F)
plot(pd_mod1)
plot(resid(pd_mod1))
lattice::qqmath(pd_mod1)

shapiro.test(resid(pd_mod1))
anova(pd_mod1)

boxplot(sqrt(metadata$bm) ~ metadata$treatment)

# functional groups -------------------------------------------------------

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
  forb_rich = specnumber(forb_inc_legume_pc),
  div_metric)

forb_no_legume_a_div <- data.frame(
  forb_shan = diversity(forb_no_legume_pc, index = "shannon"),
  forb_simp = diversity(forb_no_legume_pc, index = "simpson"),
  forb_rich = specnumber(forb_no_legume_pc),
  div_metric)


Grass_a_div <- data.frame(
  Grass_shan = diversity(Grass_pc, index = "shannon"),
  Grass_simp = diversity(Grass_pc, index = "simpson"),
  Grass_rich = specnumber(Grass_pc),
  div_metric)


legume_inc_shrub_a_div <- data.frame(
  legume_shan = diversity(legume_inc_shrub_pc, index = "shannon"),
  legume_simp = diversity(legume_inc_shrub_pc, index = "simpson"),
  legume_rich = specnumber(legume_inc_shrub_pc),
  div_metric)

legume_no_shrub_a_div <- data.frame(
  legume_shan = diversity(legume_no_shrub_pc, index = "shannon"),
  legume_simp = diversity(legume_no_shrub_pc, index = "simpson"),
  legume_rich = specnumber(legume_no_shrub_pc),
  div_metric)

# Analyzing forbs ---------------------------------------------------------

#------forb including legumes

#shannon
forb_inc_legume_mod1  <- lmer(exp(forb_inc_legume_a_div$forb_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(forb_inc_legume_mod1)
plot(resid(forb_inc_legume_mod1))
lattice::qqmath(forb_inc_legume_mod1)
shapiro.test(resid(forb_inc_legume_mod1))
anova(forb_inc_legume_mod1)

boxplot(forb_inc_legume_a_div$forb_shan ~ metadata$treatment)
kruskal.test(forb_inc_legume_a_div$forb_shan ~ metadata$treatment)

#richness
forb_inc_legume_mod2  <- glmer((forb_inc_legume_a_div$forb_rich) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), family ="poisson")
summary(forb_inc_legume_mod2)


boxplot(forb_inc_legume_a_div$forb_rich ~ metadata$treatment)

#----forb no legume

#shannon
forb_no_legume_mod1  <- lmer(exp(forb_no_legume_a_div$forb_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(forb_no_legume_mod1)
plot(resid(forb_no_legume_mod1))
lattice::qqmath(forb_no_legume_mod1)
shapiro.test(resid(forb_no_legume_mod1))
anova(forb_no_legume_mod1)

boxplot(forb_no_legume_a_div$forb_shan ~ metadata$treatment)
kruskal.test(forb_no_legume_a_div$forb_shan ~ metadata$treatment)

#richness
forb_no_legume_mod2  <- glmer((forb_no_legume_a_div$forb_rich) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), family ="poisson")
summary(forb_no_legume_mod2)


boxplot(forb_no_legume_a_div$forb_rich ~ metadata$treatment)


#  grass ---------------------------------------------------------

#shannon
Grass_mod1  <- lmer(exp(Grass_a_div$Grass_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(Grass_mod1)
plot(resid(Grass_mod1))

lattice::qqmath(Grass_mod1)
shapiro.test(resid(Grass_mod1))
anova(Grass_mod1)
hist(Grass_a_div$Grass_shan)

boxplot(Grass_a_div$Grass_shan ~ metadata$treatment)
kruskal.test(Grass_a_div$Grass_shan ~ metadata$treatment)

#richness
Grass_mod2  <- glmer((Grass_a_div$Grass_rich) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), family ="poisson")
summary(Grass_mod2)


boxplot(Grass_a_div$Grass_rich ~ metadata$treatment)


# legume ------------------------------------------------------------------

#no shrubs
#shannon 
legume_no_shrub_mod1  <- lmer(exp(legume_no_shrub_a_div$legume_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(legume_no_shrub_mod1)
plot(resid(legume_no_shrub_mod1))
lattice::qqmath(legume_no_shrub_mod1)
shapiro.test(resid(legume_no_shrub_mod1))
anova(legume_no_shrub_mod1)

boxplot(legume_no_shrub_a_div$legume_shan ~ metadata$treatment)
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



##rangescore
range_mod <- lmer(df_rangescore$range_score ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(range_mod)
plot(resid(range_mod))
lattice::qqmath(range_mod)
anova(range_mod)
boxplot(df_rangescore$range_class ~ metadata$treatment)


# Biomass by functional group ---------------------------------------------
p_forb_biomass$bio_prop <- rowSums(p_forb_biomass)/metadata$bm
p_grass_biomass$bio_prop <- rowSums(p_grass_biomass)/metadata$bm
a_forb_biomass$bio_prop <- rowSums(a_forb_biomass)/metadata$bm

###Perennial forb biomass
p_forb_bio_mod <- lmer(expm1(p_forb_biomass$bio_prop) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(p_forb_bio_mod)
plot(resid(p_forb_bio_mod))
lattice::qqmath(p_forb_bio_mod)
car::Anova(p_forb_bio_mod)
boxplot(p_forb_biomass$bio ~ metadata$treatment)


##Annual forb biomass
a_forb_bio_mod <- lmer(log1p(a_forb_biomass$bio_prop) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(a_forb_bio_mod)
plot(resid(a_forb_bio_mod ))
lattice::qqmath(a_forb_bio_mod )
car::Anova(a_forb_bio_mod)
boxplot(a_forb_biomass$bio~ metadata$treatment)
hist(a_forb_biomass$bio_prop)



# Biomass vs productivity -------------------------------------------------

mod_bm <- lmer(sqrt(metadata$bm) ~ forb_a_div$forb_shan * metadata$treatment+
             Grass_a_div$Grass_shan * metadata$treatment+
             #legumeforb_div$simpson_index *metadata$treatment  +
             (1|metadata$site) + (1|metadata$grid))
summary(mod_bm)
anova(mod_bm) %>% broom::tidy() %>% View()
hist(resid(mod_bm))
shapiro.test(resid(mod_bm))

plot(sqrt(metadata$bm) ~ legumeforb_div$simpson_index)
plot(sqrt(metadata$bm) ~ Grass_a_div$Grass_simp)
plot(sqrt(metadata$bm) ~ forb_a_div$forb_simp)
boxplot(sqrt(metadata$bm) ~ metadata$treatment)



plt_df <- data.frame(grass_shan = Grass_a_div$Grass_shan,
                     forb_shan = forb_a_div$forb_shan,
                     biomass = metadata$bm,
                     treatment = metadata$treatment) %>% 
  pivot_longer(-c(treatment, biomass))



ggplot(plt_df, aes(x = value, y = sqrt(biomass)))+
  geom_point(aes(x = value, y = sqrt(biomass), color = treatment ),size =4, alpha = 0.3) +
  facet_grid(name ~ treatment ) +
  stat_smooth(method = "lm")

car::vif(mod_bm)
# Multivariate analysis ---------------------------------------------------


dist_bray <- vegdist(df_pc, method = "euclidean")

set.seed(1234)
veg_permanova <- adonis(dist_bray ~ metadata$treatment + metadata$elevation + metadata$avg_temp +
         metadata$avg_ppt + metadata$aspect,parallel = getOption("mc.cores"))

set.seed(121)
veg_anosim1 <- anosim(dist_bray, metadata$treatment, parallel = getOption("mc.cores"))

set.seed(123456)
veg_anosim2 <- anosim(dist_bray, metadata$elevation, parallel = getOption("mc.cores"))

spe.hel <- decostand(df_pc, "hellinger")
bc<-vegdist(spe.hel, method="bray", binary=FALSE) 

pc <- ape::pcoa(dist_bray)
biplot(pc)

pc_hel <- ape::pcoa(bc)
biplot(pc_hel)
pc_hel$values


set.seed(124)
adonis(bc ~ metadata$treatment + metadata$elevation + metadata$avg_temp + metadata$avg_ppt) 

simpleRDA <- rda(spe.hel ~  metadata$treatment +
                   metadata$elevation +
                   metadata$avg_temp +
                   metadata$avg_ppt +
                   metadata$slope+
                   metadata$bm)


marginal_terms <-  anova.cca(simpleRDA, by ="margin", parallel = getOption("mc.cores"))
rda_axis <- anova.cca(simpleRDA, by ="axis", parallel = getOption("mc.cores"))
by_terms <- anova.cca(simpleRDA, by ="terms",parallel = getOption("mc.cores"))

step.res <- ordiR2step(, , perm.max = 200)

plot(simpleRDA)

vif.cca(simpleRDA)
