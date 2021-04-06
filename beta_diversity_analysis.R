
# Import packages ---------------------------------------------------------


library(tidyverse)
library(vegan)
library(ape)
library(ggsci)



# Import data -------------------------------------------------------------

df_biomass <- read.csv("biomass_12_Mar.csv", row.names = 1)
df_pc <- read.csv("pc_12_Mar.csv", row.names = 1)
metadata <- read.csv("metadata.csv")


# Data transformation -----------------------------------------------------


hel_pc <- decostand(df_pc, method = "hellinger")
dist_bray <- vegdist(hel_pc , method = "bray")
PC <- pcoa(dist_bray, correction="none", rn=NULL)

mds <- metaMDS(dist_bray, parallel = getOption("mc.core"), trymax =9999)

plt_beta <- data.frame(PC1 = PC$vectors[,1],
                       PC2 = PC$vectors[,2],
                       site = metadata$site,
                       metadata[,3:15],
                       NMDS1 = mds)


ggplot(plt_beta, aes(PC1, PC2,shape= site,  color  = treatment))+
  geom_point(size =4) +
  stat_ellipse(geom = "polygon",level = 0.95, alpha = 0.3,  show.legend = F) +
  scale_color_npg()
