
# Import packages ---------------------------------------------------------


library(tidyverse)
library(vegan)
library(ape)
library(ggsci)



# Import data -------------------------------------------------------------

df_biomass <- read.csv("biomass_12_Mar.csv", row.names = 1)
df_pc <- read.csv("pc_12_Mar.csv", row.names = 1)
metadata <- read.csv("metadata.csv")
range_scor <- read.csv("df_range_score.csv")


# Data transformation -----------------------------------------------------


hel_pc <- decostand(df_pc, method = "hellinger")
dist_bray <- vegdist(hel_pc , method = "bray")
PC <- pcoa(dist_bray, correction="none", rn=NULL)

set.seed(111)
mod.adonis <- adonis2(dist_bray ~  metadata$treatment * metadata$site, parallel = getOption("mc.core"))
broom::tidy(mod.adonis)


plt_beta <- data.frame(PC1 = PC$vectors[,1],
                       PC2 = PC$vectors[,2],
                       site = metadata$site,
                       metadata[,3:15])




plt <- ggplot(plt_beta, aes(PC1, PC2 , color  = treatment,  shape = site))+
  geom_point(size = 4, alpha = 0.7) +
  stat_ellipse(geom = "polygon",level = 0.95, alpha = 0.3,  show.legend = F) +
    theme_bw() +
  theme(panel.grid = element_blank())+
  labs(x = "PCo axis 1 (5.5 %)",
       y = "PCo axis 1 (3.9 %)") +
  ggsci::scale_color_aaas(labels = c("Exclosure", "Grazing")) +
  ggeasy::easy_all_text_color("black") +
  ggeasy::easy_all_text_size(size = 14) +
  annotate(geom = "text",
           x = c(-0.38,-0.38,-0.38),
           y = c(-0.34,-0.38,-0.42),
           size = 4.5,
           label = c(expression(bold(paste(Treatment~~~~~~~~~~~~~~ R^2 ~"=" ~"0.07"~";" ~ "p <0.001;"))),
                      expression(bold(paste(Site~~~~~~~~~~~~~~~~~~~~~~~~~~ R^2 ~"=" ~"0.29"~";" ~ "p <0.001;"))),
                      expression(bold(paste(Site:Treatment~~~~ R^2 ~"="~"0.17"~";" ~ "p <0.001;")))))+
  guides(col=guide_legend("Treatment"),
         shape=guide_legend("Site")) +
  scale_shape_manual(values = c(2,6,7),
                     labels = c("Masouleh", "Javaherdeh", "Polour" ))

plt_withoutbold <- ggplot(plt_beta, aes(PC1, PC2 , color  = treatment,  shape = site))+
  geom_point(size = 4, alpha = 0.7,   show.legend = F) +
  stat_ellipse(geom = "polygon",level = 0.95, alpha = 0.3,  show.legend = F) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(x = "PCo axis 1 (5.5 %)",
       y = "PCo axis 1 (3.9 %)") +
  ggsci::scale_color_aaas(labels = c("Exclosure", "Grazing")) +
  ggeasy::easy_all_text_color("black") +
  ggeasy::easy_all_text_size(size = 14) +
  annotate(geom = "text",
           x = c(-0.38,-0.38,-0.38),
           y = c(-0.34,-0.38,-0.42),
           size = 4.5,
           label = c(expression(paste(Treatment~~~~~~~~~~~~~~ R^2 ~"=" ~"0.07"~";" ~ "p <0.001,")),
                     expression(paste(Site~~~~~~~~~~~~~~~~~~~~~~~~ R^2 ~"=" ~"0.29"~";" ~ "p <0.001,")),
                     expression(paste(Site:Treatment~~~~ R^2 ~"="~"0.17"~";" ~ "p <0.001,"))))+
  guides(col=guide_legend("Treatment"),
         shape=guide_legend("Site")) +
  scale_shape_manual(values = c(2,6,7),
                     labels = c("Masouleh", "Javaherdeh", "Polour" ))
             
plt_withoutbold
ggsave(plt_withoutbold, filename = "beta_diversity.jpg", width = 12, height = 8, units = "in", dpi = 1200)

grass_bm <- rowSums(Grass_biomass)/plt_beta$bm
legume_bm <- rowSums(legume_inc_shrub_biomass)/plt_beta$bm
forb_bm <- rowSums(forb_no_legume_biomass)/plt_beta$bm


set.seed(123)
rda.mod <- rda(hel_pc ~  elevation + slope + avg_temp + avg_ppt + bm +
                 range_scor$range_score + grass_bm + legume_bm + forb_bm +  range_scor$litter_dry +
                 rowSums(exotic_cover) + rowSums(native_cover),
               data = metadata)

rda_score <- as.data.frame(summary(rda.mod)$biplot[,1:2])

library(ggord)
ggord::ggord(rda.mod, grp_in = metadata$site) 

names_factor <- c("Elevation ", "Slope", "MAT", "MAP", "Total Biomass", "Range scroe", "Grass biomass",
                  "Legume Biomass", "Forb Biomass", "Litter Biomass", "Exotic cover", "Native cover")

rownames(rda_score) <- names_factor



rda.plot <- ggplot(plt_beta, aes(PC1, PC2 , color  = treatment,  shape = site))+
  geom_point(size = 4, alpha = 0.7) +
  stat_ellipse(geom = "polygon",level = 0.95, alpha = 0.3,  show.legend = F) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(x = "RDA 1 (28.2 %)",
       y = "RDA 1 (21.9 %)") +
  ggsci::scale_color_aaas(labels = c("Exclosure", "Grazing")) +
  ggeasy::easy_all_text_color("black") +
  ggeasy::easy_all_text_size(size = 14) +
  geom_segment(data = rda_score, aes(x=0, xend=RDA1, y=0, yend=RDA2),
                   arrow=arrow(length=unit(0.01,"npc")), inherit.aes = FALSE) +
  geom_label(data = rda_score,aes(x=RDA1, y = RDA2), label = row.names(rda_score), check_overlap = TRUE, inherit.aes = F) +
  guides(col=guide_legend("Treatment"),
         shape=guide_legend("Site")) +
  scale_shape_manual(values = c(2,6,7),
                     labels = c("Masouleh", "Javaherdeh", "Polour" )) +
  expand_limits(x = c(-0.8, 06))
 
  
rda.plot
ggsave(rda.plot, filename = "rda.plt.jpg", width = 12, height = 8, units = "in", dpi = 1200)

str(grass_bm)
set.seed(123)
rda.mod <- rda(hel_pc ~  elevation + slope + avg_temp + avg_ppt + bm +
                 df_rangescore$range_score + grass_bm + legume_bm + forb_bm + df_rangescore$litter_dry +
                 rowSums(exotic_cover) + rowSums(native_cover),
               data = metadata)
plot(rda.mod)
summary(rda.mod)
anova.cca(rda.mod)
anova.cca(rda.mod, by = "axis")
anova.cca(rda.mod, by = "margin")
vif.cca(rda.mod)






View(fortify(rda.mod))
