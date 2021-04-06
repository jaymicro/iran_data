library(tidyverse)


# Import data  ------------------------------------------------------------


df_biomass_clean <- read.csv("biomass_12_Mar.csv",  row.names = 1) 


df_pc<- read.csv("pc_12_Mar.csv",  row.names = 1) 

df_rangescore <- read.csv("df_range_score_12_Mar.csv", row.names = 1)

metadata<- read.csv("metadata.csv")

div_metric <- data.frame(
  species_richness = specnumber(df_pc),
  Shannon_index = diversity(as.matrix(df_pc), index = "shannon", MARGIN = 1, base = exp(1)),
  simpson_index = diversity(as.matrix(df_pc), index = "simpson", MARGIN = 1, base = exp(1)),
  biomass = rowSums(df_biomass_clean))



# Diversity figures -------------------------------------------------------

div_fig <- data.frame(
  div_metric,
  treatment = metadata$treatment,
  site = metadata$site
)


ggplot(div_fig, aes(x = treatment, y = exp(Shannon_index), fill = treatment)) +
  geom_violin(aes(x = treatment, y = exp(Shannon_index))) +
  geom_boxplot(aes(x = treatment, y = exp(Shannon_index)), width = 0.25, lwd = 1) +
  facet_grid(. ~ site ) +
  theme_bw() +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = 15)
  
ggplot(div_fig, aes(x = treatment, y = (species_richness), fill = treatment)) +
  geom_violin(aes(x = treatment, y = (species_richness))) +
  geom_boxplot(aes(x = treatment, y = (species_richness)), width = 0.25, lwd = 1) +
  facet_grid(. ~ site ) +
  theme_bw() +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = 25)


ggplot(div_fig, aes(x = treatment, y = exp(simpson_index), fill = treatment)) +
  geom_violin(aes(x = treatment, y = exp(simpson_index), fill = treatment)) +
  geom_boxplot(aes(x = treatment, y = exp(simpson_index), fill = treatment), width = 0.25, lwd = 1) +
  facet_grid(. ~ site ) +
  theme_bw() +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = 2.5)

ggplot(div_fig, aes(x = treatment, y = sqrt(biomass), fill = treatment)) +
  geom_violin(aes(x = treatment, y = sqrt(biomass), fill = treatment)) +
  geom_boxplot(aes(x = treatment, y = sqrt(biomass)), width = 0.25, lwd = 1) +
  facet_grid(. ~ site ) +
  theme_bw() +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = 40) +
  ylim(c(0,45))


ggplot(div_fig, aes(x = treatment, y = df_rangescore$range_score, fill = treatment)) +
  geom_violin(aes(x = treatment, y = df_rangescore$range_score, fill = treatment)) +
  geom_boxplot(aes(x = treatment, y = df_rangescore$range_score), width = 0.25, lwd=1) +
  facet_grid(. ~ site ) +
  theme_bw() +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = 110) 

