library("tidyverse")
library("vegan")
library("reshape2")

df_biomass <- read.csv("biomass_12_Mar.csv", row.names = 1)
df_pc <- read.csv("pc_12_Mar.csv", row.names = 1)
metadata <- read.csv("metadata.csv")

biomass <- rowSums(df_biomass)
shannon_change  <- vegdist(div_metric$Shannon_index)


bray_pc <- vegdist(df_pc, method = "bray")
bray_biomass <- vegdist(df_biomass, method = "bray")

long_pc <- melt(as.matrix(bray_pc), varnames = c("row", "col"))
long_biomass <- melt(as.matrix(bray_biomass), varnames = c("row", "col"))
long_bm <- melt(as.matrix(bm), varnames = c("row", "col"))
long_shannon <- melt(as.matrix(shannon_change), varnames = c("row", "col"))

long_pc$row <- factor(long_pc$row, levels = metadata$X, labels = metadata$treatment)
long_pc$col <- factor(long_pc$col, levels = metadata$X, labels = metadata$treatment)


long_biomass$row <- factor(long_biomass$row, levels = metadata$X, labels = metadata$treatment)
long_biomass$col <- factor(long_biomass$col, levels = metadata$X, labels = metadata$treatment)

long_bm$row <- factor(long_bm$row, levels = metadata$X, labels = metadata$treatment)
long_bm$col <- factor(long_bm$col, levels = metadata$X, labels = metadata$treatment)


long_pc <- long_pc %>% 
  mutate(treatment = case_when(
    row == "exclosure" & col == "exclosure"    ~ "exclosure",
    row == "exclosure" & col == "grazing"      ~ "between",
    row == "grazing" & col == "exclosure"    ~ "between",
    TRUE    ~ "grazing",
  ))

long_biomass <- long_biomass %>% 
  mutate(treatment = case_when(
    row == "exclosure" & col == "exclosure"    ~ "exclosure",
    row == "exclosure" & col == "grazing"      ~ "between",
    row == "grazing" & col == "exclosure"    ~ "between",
    TRUE    ~ "grazing",
  ))

long_bm <- long_bm %>% 
  mutate(treatment = case_when(
    row == "exclosure" & col == "exclosure"    ~ "exclosure",
    row == "exclosure" & col == "grazing"      ~ "between",
    row == "grazing" & col == "exclosure"    ~ "between",
    TRUE    ~ "grazing",
  ))

combined_df <- data.frame(biomass = long_biomass$value,
                          beta_div = long_pc$value,
                          bm = long_bm$value,
                          shann = long_shannon$value
                          treatment = long_biomass$treatment)


ggplot(combined_df, aes(x = beta_div, y = biomass, color = treatment ))+
  geom_point(pch = 21, alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_grid(. ~ treatment)

ggplot(combined_df, aes(x = beta_div, y = bm, color = treatment ))+
  geom_point(pch = 21, alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_grid(. ~ treatment)

ggplot(combined_df, aes(y = beta_div, x = treatment, color = treatment )) +
  geom_violin() +
  #geom_boxplot()+
  ggpubr::stat_compare_means()

ggplot(combined_df, aes(y = biomass, x = treatment, color = treatment )) +
  geom_violin() +
  #geom_boxplot()+
  ggpubr::stat_compare_means()


summary(lm(long_biomass$value ~ long_shannon$value ))
summary(lm(long_biomass$value ~ long_pc$value + long_shannon$value ))

plot(long_biomass$value ~ long_shannon$value)
boxplot(long_pc$value ~ long_pc$treatment)
