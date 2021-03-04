
# Import packages ---------------------------------------------------------

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(glmmTMB)



# Import data -------------------------------------------------------------

df_biomass_clean <- read.csv("biomass_March1.csv",  row.names = 1) 

df_pc<- read.csv("percentcover_March1.csv",  row.names = 1) 

df_rangescore <- readxl::read_xlsx("df_range_score.xlsx") %>% 
  separate(plot_indicator,into = c('col', 'row'), sep = 1)

metadata<- read.csv("metadata.csv")


# Data wrangle ------------------------------------------------------------



metadata$elevation <- as.numeric(metadata$elevation)
metadata$slope <- as.numeric(metadata$slope)
metadata$avg_temp <- as.numeric(metadata$avg_temp)
metadata$avg_ppt <- as.numeric(metadata$avg_ppt)

str(metadata)

names(df_biomass_clean)
which(rowSums(df_biomass_clean[, -106]) == 0)
which(rowSums(df_pc[, -106]) == 0)

df_pc <- df_pc %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>% select(-id_id)

df_biomass_clean <- df_biomass_clean %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024)) %>% select(-id_id)

metadata <- metadata %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))

df_rangescore <- df_rangescore %>% 
  slice(-c(128,  192,  256, 489, 505, 576,  704,  768,  832,  896,  960, 1024))

which(rowSums(df_biomass_clean) == 0)
which(rowSums(df_pc) == 0)

metadata$grid <- paste(df_rangescore$col, df_rangescore$row)
metadata$bm <- rowSums(df_biomass_clean)

which(is.numeric(df_pc))


# Data analysis -----------------------------------------------------------


# Shannon index -----------------------------------------------------------


div_metric <- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(as.matrix(df_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))

mod4 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site) + 
               (1|metadata$grid) )
anova(mod4)
plot(resid(mod4))
shapiro.test(resid(mod4))
lattice::qqmath(mod4)


# Simpson index -----------------------------------------------------------

mod2 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$grid) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))

kruskal.test(div_metric$simpson_index ~ metadata$treatment)
boxplot(div_metric$simpson_index ~ metadata$treatment)


# Productivity ------------------------------------------------------------
metadata$grid <- paste(df_rangescore$col, df_rangescore$row)
metadata$bm <- rowSums(df_biomass_clean)
range(metadata$bm)

pd_mod1 <- lmer(sqrt(bm) ~ treatment + (1|site) + (1|grid)  , metadata, REML = F)
plot(pd_mod1)
plot(resid(pd_mod1))
lattice::qqmath(pd_mod1)

shapiro.test(resid(pd_mod1))
anova(pd_mod1)


# functional groups -------------------------------------------------------

fn_grp <- readxl::read_xlsx("meta_fg.xlsx")

table(is.na(match(fn_grp$species, fn_grp$species)))
table(is.na(match(colnames(df_pc), fn_grp$species)))
sp <- which(is.na(match(colnames(df_pc), fn_grp$species))) %>% as.vector()

p <- df_biomass_clean %>% 
  select(all_of(sp)) %>% 
  colnames()

match(p, fn_grp$species)

df_biomass_clean <- df_biomass_clean %>% 
  select(sort(tidyselect::peek_vars()))
df_pc <- df_pc  %>% 
  select(sort(tidyselect::peek_vars()))

fn_grp <- fn_grp %>% 
  mutate(span_form = paste(life_span,life_form, sep = " ")) 

table(is.na(match(fn_grp$species, colnames(df_biomass_clean))))
table(is.na(match(fn_grp$species, colnames(df_pc))))


table(fn_grp$life_form)

# Analyzing forbs ---------------------------------------------------------



p_forb <- which((fn_grp$span_form == "Perennial Forb"))


P_forb_pc <- df_pc %>% 
  select(all_of(p_forb))

p_forb_biomass <- df_biomass_clean %>% 
  select(all_of(p_forb))

P_forb_meta <- fn_grp %>% 
  filter(span_form == "Perennial Forb")


# alpha diversity of perennial forbs ------------------------------------

p_forb_a_div <- data.frame(
  p_forb_shan = diversity(P_forb_pc, index = "shannon"),
  p_forb_simp = diversity(P_forb_pc, index = "simpson"),
  p_forb_rich = specnumber(P_forb_pc),
  div_metric)
)


p_forb_mod1  <- lmer(exp(p_forb_a_div$p_forb_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site))
plot(p_forb_mod1)
plot(resid(p_forb_mod1))
lattice::qqmath(p_forb_mod1)
shapiro.test(resid(p_forb_mod1))
anova(p_forb_mod1)

hist(p_forb_a_div$p_forb_shan)
boxplot(p_forb_a_div$p_forb_shan ~ metadata$treatment)
kruskal.test(p_forb_a_div$p_forb_shan ~ metadata$treatment)

kruskal.test(p_forb_a_div$p_forb_shan ~  metadata$treatment)

# Annual Forbs ------------------------------------------------------------

a_forb <- which((fn_grp$span_form == "Annual Forb"))


a_forb_pc <- df_pc %>% 
  select(all_of(a_forb))

a_forb_biomass <- df_biomass_clean %>% 
  select(all_of(a_forb))

a_forb_meta <- fn_grp %>% 
  filter(span_form == "Annual Forb")


a_forb_a_div <- data.frame(
  a_forb_shan = diversity(a_forb_pc, index = "shannon"),
  a_forb_simp = diversity(a_forb_pc, index = "simpson"),
  a_forb_rich = specnumber(a_forb_pc),
  div_metric)
)


a_forb_mod <- lmer(sqrt(a_forb_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), a_forb_a_div)
plot(resid(a_forb_mod))
lattice::qqmath(a_forb_mod)
shapiro.test(resid(a_forb_mod))
anova(a_forb_mod)

hist(a_forb_a_div$a_forb_shan)
kruskal.test(a_forb_a_div$a_forb_shan ~ metadata$treatment)
boxplot(a_forb_a_div$a_forb_shan ~ metadata$treatment)


# Perennial grass ---------------------------------------------------------

p_grass <- which((fn_grp$span_form == "Perennial Grass"))


p_grass_pc <- df_pc %>% 
  select(all_of(p_grass))


p_grass_biomass <- df_biomass_clean %>% 
  select(all_of(p_grass))

p_grass_meta <- fn_grp %>% 
  filter(span_form == "Perennial Grass")


p_grass_a_div <- data.frame(
  p_grass_shan = diversity(a_forb_pc, index = "shannon"),
  p_grass_simp = diversity(a_forb_pc, index = "simpson"),
  p_grass_rich = specnumber(a_forb_pc),
  div_metric)
)

p_grass_mod <- lmer(exp(p_grass_shan) ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), p_grass_a_div)
plot(p_grass_mod)
plot(resid(p_grass_mod))
lattice::qqmath(p_grass_mod)
str(p_grass_a_div)
anova(p_grass_mod)

kruskal.test(p_grass_shan ~  metadata$treatment, p_grass_a_div) 
t.test(p_grass_shan ~  metadata$treatment, p_grass_a_div)
friedman.test(p_grass_a_div$p_grass_shan, metadata$treatment,metadata$grid, data = p_grass_a_div)


# legume ------------------------------------------------------------------



# Multivariate analysis ---------------------------------------------------


dist_bray <- vegdist(df_pc)

set.seed(1234)
adonis(dist_bray ~ metadata$treatment + metadata$elevation + metadata$avg_temp +
         metadata$avg_ppt + metadata$aspect)

set.seed(121)
anosim(dist_bray, metadata$treatment)

set.seed(123456)
anosim(dist_bray, metadata$elevation)

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
