library(tidyverse)
library(vegan)
library(ape)
library(lme4)
library(lmerTest)
library(ape)
library(nlme)
library(glmmTMB)
#library(dae)


df_biomass_clean <- read.csv("biomass_March1.csv",  row.names = 1) 
df_pc<- read.csv("percentcover_March1.csv",  row.names = 1) 
df_rangescore <- readxl::read_xlsx("df_range_score.xlsx") %>% 
  separate(plot_indicator,into = c('col', 'row'), sep = 1)

as.numeric(unlist(df_pc))

df_rangescore$col[df_rangescore$col == "A"] <- 1
df_rangescore$col[df_rangescore$col == "B"] <- 2
df_rangescore$col[df_rangescore$col == "C"] <- 3
df_rangescore$col[df_rangescore$col == "D"] <- 4
df_rangescore$col[df_rangescore$col == "E"] <- 5
df_rangescore$col[df_rangescore$col == "F"] <- 6
df_rangescore$col[df_rangescore$col == "G"] <- 7
df_rangescore$col[df_rangescore$col == "H"] <- 8


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
                          id_id == "kho_GA4" ~ "57.0775")) %>% 
  mutate(cat_ele = case_when(
    elevation<1500                  ~ "a",
    elevation<2000 & elevation>1500 ~ "b",
    elevation<2500 & elevation>2000 ~ "c",
    TRUE                            ~ "d"
  ))

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


mod1 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment  + (1|metadata$site))
anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))

mod2 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))
qqnorm(resid(mod2))
qqline(resid(mod2))

mod3 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site)+(1|metadata$id_id), REML = F)
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))

mod4 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site) + 
               (1|metadata$grid) )
anova(mod4)
plot(resid(mod4))
shapiro.test(resid(mod4))
lattice::qqmath(mod4)


mod4a <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site) + 
               (1|metadata$grid) + (1|metadata$aspect))
anova(mod4a)
plot(resid(mod4a))
shapiro.test(resid(mod4a))
lattice::qqmath(mod4a)

mod4b <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site) + 
                (1|metadata$grid) + (1|metadata$aspect) + (1|metadata$slope))
anova(mod4b)
plot(resid(mod4b))
shapiro.test(resid(mod4b))
lattice::qqmath(mod4b)

mod4c <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + (1|metadata$site) + 
                (1|metadata$grid) + (1|metadata$aspect)+ (1|metadata$slope) + (1|metadata$elevation))
anova(mod4c)
plot(resid(mod4c))
shapiro.test(resid(mod4c))
lattice::qqmath(mod4c)


AIC(mod4,mod4a, mod4b, mod4c)

mod7 <- lmer(exp(div_metric$Shannon_index) ~ metadata$treatment + metadata$elevation + (1|metadata$grid))
anova(mod7)
plot(resid(mod7))
plot(mod7)
shapiro.test(resid(mod7))
lattice::qqmath(mod7)

lmtest::lrtest( mod4, mod7)
AIC(mod4, mod5, mod6, mod7)


plot(div_metric$Shannon_index ~ metadata$elevation)

#mod 4 is the best model, mod 3 has lower AIC but does not pass normality assumption


dfdivmeta <- cbind(div_metric, metadata, df_rangescore)


dfdivmeta$col <- as.numeric(dfdivmeta$col)
dfdivmeta$row <- as.numeric(dfdivmeta$row)




#########################################################################################################
########                        Simpson index
##########################################################################################

#obtain lambda

EnvStats::boxcox(div_metric$simpson_index+.1)

plot(fitted(rich_mod), resid(rich_mod), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

shapiro.test(resid(rich_mod))


#different method
library("forecast")
lda <- BoxCox.lambda(div_metric$simpson_index, method=c("guerrero"))

trans.rain <- BoxCox(div_metric$simpson_index,lda)
hist(trans.rain)


#lmer models with exp transformation, not normal
mod2 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$site))

anova(mod1)
plot(resid(mod1))
shapiro.test(resid(mod1))
qqnorm(resid(mod1))
qqline(resid(mod1))


mod2 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$id_id))
anova(mod2)
plot(resid(mod2))
shapiro.test(resid(mod2))

mod3 <- lmer(exp(div_metric$simpson_index) ~ metadata$treatment + (1|metadata$id_id) + (1|metadata$site))
anova(mod3)
plot(resid(mod3))
shapiro.test(resid(mod3))

kruskal.test(div_metric$simpson_index ~ metadata$treatment)
boxplot(div_metric$simpson_index ~ metadata$treatment)

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








# Productivity ------------------------------------------------------------
metadata$grid <- paste(df_rangescore$col, df_rangescore$row)
metadata$bm <- rowSums(df_biomass_clean)
range(metadata$bm)

pd_mod1 <- lmer(sqrt(bm) ~ treatment + (1|site) + (1|grid) + (1|aspect) , metadata, REML = F)
plot(pd_mod1)
plot(resid(pd_mod1))
lattice::qqmath(pd_mod1)

shapiro.test(resid(pd_mod1))
anova(pd_mod1)


# functional groups -------------------------------------------------------

fn_grp <- readxl::read_xlsx("meta_fg.xlsx")
fn_grp_nw <- readxl::read_xlsx("meta_fg_mohammad.xlsx")

match(fn_grp$species, fn_grp_nw$species)

table(is.na(match(fn_grp$species, fn_grp_nw$species)))
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


p_forb_mod1  <- glmer(p_forb_a_div$p_forb_shan ~  metadata$treatment + (1|metadata$grid), 
                      family = "poisson")
plot(p_forb_mod1)
plot(resid(p_forb_mod1))
lattice::qqmath(p_forb_mod1)
shapiro.test(resid(p_forb_mod1))
anova(p_forb_mod1)

hist(p_forb_a_div$p_forb_shan)
boxplot(p_forb_a_div$p_forb_shan ~ metadata$treatment)

p_forb_z <- glmmTMB(p_forb_a_div$p_forb_shan ~  metadata$treatment +
          (1|metadata$grid), ziformula = ~1)

plot(residuals(p_forb_z))
car::Anova(p_forb_z)

dim(p_forb_a_div)
dim(metadata)
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


a_forb_mod <- aov(log1p(a_forb_shan) ~  metadata$treatment + Error(metadata$grid), a_forb_a_div)
plot(dae::residuals.aovlist( a_forb_mod))
lattice::qqmath(a_forb_mod)
qqnorm(dae::residuals.aovlist(a_forb_mod))
qqline(dae::residuals.aovlist(a_forb_mod))
shapiro.test(dae::residuals.aovlist(a_forb_mod))

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



p_grass_mod <- lmer(p_grass_shan ~  metadata$treatment + (1|metadata$grid) + (1|metadata$site), p_grass_a_div)
plot(a_grass_mod)
plot(resid(a_grass_mod))
lattice::qqmath(a_grass_mod)
str(p_grass_a_div)

kruskal.test(p_grass_shan ~  metadata$treatment, p_grass_a_div) 
t.test(p_grass_shan ~  metadata$treatment, p_grass_a_div)
friedman.test(p_grass_a_div$p_grass_shan, metadata$treatment,metadata$grid, data = p_grass_a_div)

###############################################################################################
##################   Beta - diversity #########################################################
###############################################################################################


dist_bray <- vegdist(labdsv::hellinger(df_pc))

set.seed(1234)
adonis(dist_bray ~ metadata$treatment + metadata$elevation + metadata$avg_temp + metadata$avg_ppt)

set.seed(1111)
beta_div <- metaMDS(dist_bray, k =2, trymax = 999)

pc <- ape::pcoa(dist_bray)
plot.pcoa(pc)

rd <- cca(labdsv::hellinger(df_pc) ~ metadata$treatment + metadata$elevation + metadata$avg_temp + metadata$avg_ppt )
plot(rd)
anova.cca(rd, by = "margin")


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
