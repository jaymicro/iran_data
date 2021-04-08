
# Import packages ---------------------------------------------------------

library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(ggsci)



# Import data -------------------------------------------------------------

df_biomass_clean <- read.csv("biomass_12_Mar.csv",  row.names = 1) 


df_pc<- read.csv("pc_12_Mar.csv",  row.names = 1) 

df_rangescore <- read.csv("df_range_score.csv", row.names = 1)

metadata<- read.csv("metadata.csv")

fn_grp <- readxl::read_xlsx("meta_fg.xlsx")


# Data analysis -----------------------------------------------------------


# Shannon index -----------------------------------------------------------


div_metric <- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(as.matrix(df_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean),
  metadata)

div_metric$site <- factor(div_metric$site, levels = c("maz_po","masouleh","maz_java"  ))

mod.shan1 <- lmer(exp(Shannon_index) ~ treatment*site + (1|metadata$grid), data = div_metric)

anova(mod.shan1)
plot(resid(mod.shan1))
shapiro.test(resid(mod.shan1))
lattice::qqmath(mod.shan1)
boxplot(div_metric$Shannon_index ~ metadata$treatment* metadata$site)
shannon.emmeans<-cld(emmeans(mod.shan1,~treatment*site))

# posthoc test ------------------------------------------------------------

post_hoc <- emmeans(mod.shan1, list(pairwise ~ treatment*site), adjust = "tukey")
multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters)
plot(multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters))

shann_tst <- multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters) %>% 
  as.data.frame()



ph_shann_report <- shann_tst %>% 
  mutate(treatments = paste(treatment, site)) %>% 
  mutate(group = str_trim(.group, side = "both"))
ph_shann_report$site <- factor(ph_shann_report$site, levels = c("masouleh","maz_java", "maz_po" ))


# Plot shannon ------------------------------------------------------------

site = c(
  maz_po = "Polour",
  masouleh = "Masouleh",
  maz_java = "Javaherdeh"
)

ggplot(div_metric, aes(x = treatment, y = exp(Shannon_index), fill = treatment)) +
  geom_violin(aes(x = treatment, y = exp(Shannon_index)), alpha = 0.6) +
  facet_grid(. ~ site, labeller = labeller(site = site)) +
  geom_boxplot(aes(x = treatment, y = exp(Shannon_index)), 
               alpha = 0.75, width = 0.25, lwd = 1, show.legend = F)  +
  geom_text(data= shann_tst, aes(label = .group, y = 0 + 13),
            position = position_dodge(0.9), vjust = -1) +
   scale_fill_aaas(name = "Treatment",
                   label = c("Exclosure", "Grazing")) +
labs(x = NULL, y = expression(e^"Shannon diversity"), size = 16)+
    theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
ggsave(filename = "shann_index.jpg", width = 8, height = 6, dpi = 1200, units = "in")

# Simpson index -----------------------------------------------------------

mod3 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment * metadata$site + (1|metadata$grid) )
anova(mod3)
plot(resid(mod3))
hist(resid(mod3))

shapiro.test(resid(mod3))

kruskal.test(div_metric$simpson_index ~ metadata$treatment)
boxplot(div_metric$simpson_index ~ metadata$treatment * metadata$site)

# Richness ----------------------------------------------------------------

mod_rich <- glmer(species_richness ~ treatment * site + (1|grid), family="poisson", data = div_metric)

car::Anova(mod_rich)

boxplot(div_metric$species_richness ~ metadata$treatment * metadata$site)

# posthoc test ------------------------------------------------------------

post_hoc <- emmeans(mod_rich, list(pairwise ~ treatment*site), adjust = "tukey")
multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters)
plot(multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters))

rich_tst <- multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters) %>% 
  as.data.frame()



ph_rich_report <- shann_tst %>% 
  dplyr::select(c(site, .group, treatment)) %>% 
 arrange(row.names(.))


# plot richness -----------------------------------------------------------

site = c(
  maz_po = "Polour",
  masouleh = "Masouleh",
  maz_java = "Javaherdeh"
)

ggplot(div_metric, aes(x = treatment, y = species_richness, fill = treatment)) +
  geom_violin(aes(x = treatment, y = species_richness), alpha = 0.6) +
  facet_grid(. ~ site, labeller = labeller(site = site)) +
  geom_boxplot(aes(x = treatment, y = species_richness), 
               alpha = 0.75, width = 0.25, lwd = 1, show.legend = F)  +
  geom_text(data = ph_rich_report, aes(x = treatment,label = .group, y = 0 + 22.5),
             vjust = -1, inherit.aes = F) +
  scale_fill_aaas(name = "Treatment",
                  label = c("Exclosure", "Grazing")) +
  labs(x = NULL, y = "Richness", size = 14)+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
ggsave(filename = "richness_index.jpg", width = 8, height = 6, dpi = 1200, units = "in")


# Productivity ------------------------------------------------------------
pd_mod1 <- lmer(sqrt(bm) ~ treatment * site + (1|grid), metadata, REML = F)
plot(pd_mod1)
plot(resid(pd_mod1))
lattice::qqmath(pd_mod1)

shapiro.test(resid(pd_mod1))
anova(pd_mod1)

boxplot(sqrt(metadata$bm) ~ metadata$treatment * metadata$site)

#posthoc test -------------------------------------------------------

post_hoc <- emmeans(pd_mod1, list(pairwise ~ treatment*site), adjust = "tukey")
multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters)
plot(multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters))

pd_tst <- multcomp::cld(post_hoc$`emmeans of treatment, site`, Letters = letters) %>% 
  as.data.frame()



prodd_tst <-pd_tst %>% 
  dplyr::select(c(site, .group, treatment)) %>% 
  arrange(row.names(.))


# Plot productivity -------------------------------------------------------

ggplot(div_metric, aes(x = treatment, y = sqrt(bm), fill = treatment)) +
  geom_violin(aes(x = treatment, y = sqrt(bm)), alpha = 0.6) +
  facet_grid(. ~ site, labeller = labeller(site = site)) +
  geom_boxplot(aes(x = treatment, y = sqrt(bm)), 
               alpha = 0.75, width = 0.25, lwd = 1, show.legend = F)  +
  geom_text(data = prodd_tst, aes(x = treatment,label = .group, y = 0 + 35.5),
            vjust = -1, inherit.aes = F) +
  scale_fill_aaas(name = "Treatment",
                  label = c("Exclosure", "Grazing")) +
  labs(x = NULL, y = expression(sqrt("ANPP")), size = 14)+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
ggsave(filename = "ANPP.jpg", width = 8, height = 6, dpi = 1200, units = "in")

# functional groups - see data_analysis_fn_grp_12_Mar.R -------------------------------------------------------
# all groups are having similar responses with fg, also did exotic and native but not meeting normality

##rangescore
df_rangescore$range_score[df_rangescore$range_score > 100] <- 100  

mod.site.score <- lmer((df_rangescore$range_score) ~ metadata$treatment *metadata$site  + (1|metadata$grid))
anova(mod.site.score)
plot(resid(mod.site.score))
shapiro.test(resid(mod.site.score))
lattice::qqmath(mod.site.score)

boxplot(df_rangescore$range_score ~ metadata$treatment*metadata$site)

mod.site.score <- glmer(metadata$treatment~  (df_rangescore$range_score) *metadata$site  + (1|metadata$grid), family = "binomial")
Anova(mod.site.score)
plot(resid(mod.site.score))
shapiro.test(resid(mod.site.score))
lattice::qqmath(mod.site.score)

boxplot(df_rangescore$range_score ~ metadata$treatment*metadata$site)



#create new variable to adjust for range score


# which(metadata$score==0)
# 
# df_pc%>%
#   slice(194, 196, 198, 200)%>%
#   rowSums()
#   View()

df_rangescore$range_score <- replace(df_rangescore$range_score, which(df_rangescore$range_score==0), c(68, 74, 72, 67.5))

metadata=metadata%>%
  mutate(shan=div_metric$Shannon_index,
         score=df_rangescore$range_score,
         shan.score=shan/(score/100+1))

mod.shan.score <- lmer((metadata$shan.score) ~ metadata$treatment *metadata$site  + (1|metadata$grid))
anova(mod.shan.score)
plot(resid(mod.shan.score))
shapiro.test(resid(mod.shan.score))
lattice::qqmath(mod.shan.score)

boxplot(metadata$shan.score ~ metadata$treatment*metadata$site)

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



# litter vs site and treatment --------------------------------------------
lit <- decostand(df_rangescore$litter_dry, method = "hellinger", MARGIN = 2)

mod.litter <- lmer(df_rangescore$litter_dry ~ metadata$treatment * metadata$site + (1|metadata$grid))
hist(resid(mod.litter))
shapiro.test(resid(mod.litter))
anova(mod.litter)
cld(emmeans(mod.litter,~treatment*site))

df_rangescore$treatment <- metadata$treatment
df_rangescore$site <- metadata$site

boxplot(df_rangescore$litter_dry ~ metadata$treatment * metadata$site)

ggplot(df_rangescore, aes(x = treatment, y = litter_dry)) +
  geom_boxplot()+
  facet_grid( ~ site, scales = "free_y")+
  ggpubr::stat_compare_means(method = "t.test")

# Multivariate analysis ---------------------------------------------------


dist_bray <- vegdist(df_pc, method = "euclidean")

set.seed(1234)
veg_permanova <- adonis(dist_bray ~ metadata$treatment + metadata$elevation + metadata$avg_temp +
         metadata$avg_ppt + metadata$aspect,parallel = getOption("mc.cores"))


set.seed(121)
veg_anosim1 <- anosim(dist_bray, metadata$treatment, parallel = getOption("mc.cores"))
veg_anosim1

set.seed(123456)
veg_anosim2 <- anosim(dist_bray, metadata$elevation, parallel = getOption("mc.cores"))
veg_anosim2

spe.hel <- decostand(df_pc, "hellinger")
bc<-vegdist(spe.hel, method="bray", binary=FALSE) 

pc <- ape::pcoa(dist_bray)
biplot(pc)

pc_hel <- ape::pcoa(bc)
biplot(pc_hel)
pc_hel$values[1:2,]


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
