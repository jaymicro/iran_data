library("tidyverse")
library("vegan")
library("reshape2")

df_biomass <- read.csv("biomass_12_Mar.csv", row.names = 1)
df_pc <- read.csv("pc_12_Mar.csv", row.names = 1)
metadata <- read.csv("metadata.csv")



bray_pc <- vegdist(df_pc, method = "bray")
eucc_biomass <- vegdist(df_biomass, method = "bray")

long_pc <- melt(as.matrix(bray_pc), varnames = c("row", "col"))
long_biomass <- melt(as.matrix(eucc_biomass), varnames = c("row", "col"))


long_pc$row <- factor(long_pc$row, levels = metadata$X.1, labels = metadata$treatment)
long_pc$col <- factor(long_pc$col, levels = metadata$X.1, labels = metadata$treatment)


long_biomass$row <- factor(long_biomass$row, levels = metadata$X.1, labels = metadata$treatment)
long_biomass$col <- factor(long_biomass$col, levels = metadata$X.1, labels = metadata$treatment)



summary(lm(long_biomass$value ~ long_pc$value ))

plot(long_biomass$value ~ long_pc$value)
boxplot(long_pc$value ~ long_pc$treatment)
