# R_code_EBR.r

## Habitat diversity and biogeographical regionalisation

# Working directory
setwd("C:/R/")
install.packages("rgdal")
install.packages("sf")
install.packages("dplyr")
install.packages("modEvA")
install.packages("lme4")
install.packages("ggplot2")
install.packages("tidyverse")

# Libraries
library(rgdal)
library(sf)
library(dplyr)
library(modEvA)
library(lme4)
library(ggplot2)
library(tidyverse)

# Import 'Biogeographical Regions' .shp
biogeo <- readOGR('BiogeoRegions2016.shp')

# Import .shp about Europe habitat based on a grid of
# 49600 x 10 Km x 10 Km cells that covers the European territory 
habitat <- readOGR("europe_habitats.shp")
habitat_df_full <- as.data.frame(habitat)
habitat_df_completo

habitat <- st_read("europe_habitats.shp")

# Habitat richness calculated by summing the 
# number of unique habitat type per cell

hr_bin <- habitat %>%
  as.data.frame() %>%
  select(-geometry, -EofOrigin, -NofOrigin) %>%
  group_by(CellCode) %>%
  summarise(across(.fns = sum)) %>%
  remove_rownames() %>%
  column_to_rownames("CellCode") %>%
  {ifelse(. > 0, 1, 0)} %>%
  rowSums()



# Calculate distance from the nearest Biogeographical border

# Convert into sf objects
sf_region <- st_as_sf(biogeo_region)
sf_habitat <- st_as_sf(habitat)

# transform the map of Biogeographical regions from
# a polygon shape (MULTIPOLYGON) to a line (MULTILINESTRING)
border <- st_cast(sf_region, "MULTILINESTRING")
class(border)

# Calculate distance from Biogeographical border and 
# the grid cells
dist <- st_distance(sf_habitat, border)
str(dist)

# Distance dataframe
dist.df <- as.data.frame(dist)
dim(dist.df)

# Minimum distance of each grid cell from the nearest border
min_dist <- apply(dist.df, 1, FUN=min)
str(min_dist)

head(min_dist)
# [1]    0.000    0.000    0.000    0.000    0.000 3617.966



# We have created a .shp file containing information about 
# cell code, habitat richness, and distance from the border 
# using QGis software
hr_bioreg_dist <- readOGR("hr_dist.shp")
summary(hr_bioreg_dist)
str(hr_bioreg_dist)

hr_bioreg_dist.df <- as.data.frame(hr_bioreg_dist)
summary(hr_bioreg_dist.df)
str(hr_bioreg_dist.df)

range(hr_bioreg_dist.df$hr_bin)
# [1]  1 61

range(hr_bioreg_dist.df$min_dst)
# [1]   0.0000 329.8992

# From metres to Km
hr_bioreg_dist.df$min_dst <- hr_bioreg_dist.df$min_dst/1000



# Bivariate map to map to visualise patterns of 
# habitat distribution across the biogeographical regions

install.packages("biscale") # Bivariate map
install.packages("raster")
install.packages("cowplot")

library(biscale)
library(raster)
library(cowplot)

hr_bioreg_dist <- shapefile("hr_dist.shp")
hr_bioreg_dist <- st_as_sf(hr_bioreg_dist)    # Biscale works with 'sf' objects
hr_bioreg_dist <- transform(hr_bioreg_dist, hr_bin =as.numeric(hr_bin))

bioreg_fort <- st_as_sf(bioreg) %>% fortify()    # Biscale works with 'sf' objects

data <- bi_class(hr, x= min_dst, y = hr_bin, dim = 3, style ="quantile")   # create classes
# dim = 3 to produce a three-by-three map
# style = 'quantile' for calculating breaks

biv_map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "transparent", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_sf(data=bioreg_fort, fill = "transparent", lwd=0.2) +
  coord_sf() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  bi_theme()                                                                       # create bivariate map                                            

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Distance (Km)",
                    ylab = "Habitat Richness",
                    size = 8)                                                      # create bivariate legend

finalPlot <- ggdraw() +
  draw_plot(biv_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)

finalPlot

ggsave("bivmap.png", finalPlot, width = 7, height = 7, dpi = 300)

##  Density plot

# Richness
install.packages("ggridges")
library(ggridges)

den <- ggplot(hr_bioreg_dist.df, aes(x = hr_bin, y = bioreg)) +
  geom_density_ridges2(panel_scaling = FALSE)  +
  xlab("Habitat Richness") +
  ylab("Biogeographical Regions")
den

ggsave("den.richness.png", den, width = 5, height = 7, dpi = 300)

# Distance
den.dist <- ggplot(hr_bioreg_dist.df, aes(x = min_dst, y = bioreg)) +
  geom_density_ridges2(panel_scaling = FALSE) +
  xlab("Distance (Km)") +
  ylab("Biogeographical Regions")
den.dist

ggsave("density.distance.png", den.dist, width = 7, height = 7, dpi = 300)


## Statistical analysis

# LM: habitat richness ~ distanza minima total dataset
lm.total <- lm(hr_bin ~ (min_dst), data = hr_bioreg_dist.df)
summary(lm.total)

# GLM
poisson.total <- glm(hr_bin ~ min_dst, family = poisson(), data = hr_bioreg_dist.df)
summary(poisson.total)

d2_Total <- Dsquared(poisson.total)

## Alpine

alpine <- filter(hr_bioreg_dist.df, bioreg == 'Alpine')
alpine$min_dst <- alpine$min_dst/1000

# LM
lm_alpine <- lm(hbtt_rc ~ min_dst, data = alpine)
summary(lm_alpine)

# GLM Poisson
poisson.alpine <- glm(hbtt_rc ~ min_dst, family = poisson(), data = alpine)
summary(poisson.alpine)

Dsquared(poisson.alpine)

## Atlantic

atlantic <- filter(hr_bioreg_dist.df, bioreg == 'Atlantic')
atlantic$min_dst <- atlantic$min_dst/1000

# LM
lm_atlantic <- lm(hbtt_rc ~ min_dst, data = atlantic)
summary(lm_atlantic)

# GLM Poisson
poisson.atlantic <- glm(hbtt_rc ~ min_dst, family = poisson(), data = atlantic)
summary(poisson.atlantic)

Dsquared(poisson.atlantic)

## Black Sea

black_sea <- filter(hr_bioreg_dist.df, bioreg == 'Black Sea')
black_sea$min_dst <- black_sea$min_dst/1000

# LM
lm_black_sea <- lm(hbtt_rc ~ min_dst, data = black_sea)
summary(lm_black_sea)

# GLM Poisson
poisson.black_sea <- glm(hbtt_rc ~ min_dst, family = poisson(), data = black_sea)
summary(poisson.black_sea)

Dsquared(poisson.black_sea)

## Boreal
boreal <- filter(hr_bioreg_dist.df, bioreg == 'Boreal')
boreal$min_dst <- boreal$min_dst/1000

# LM
lm_boreal <- lm(hbtt_rc ~ min_dst, data = boreal)
summary(lm_boreal)

# GLM Poisson
poisson.boreal <- glm(hbtt_rc ~ min_dst, family = poisson(), data = boreal)
summary(poisson.boreal)

Dsquared(poisson.boreal)

## Continental
continental <- filter(hr_bioreg_dist.df, bioreg == 'Continental')
continental$min_dst <- continental$min_dst/1000

# LM
lm_continental <- lm(hbtt_rc ~ min_dst, data = continental)
summary(lm_continental)

# GLM Poisson
poisson.continental <- glm(hbtt_rc ~ min_dst, family = poisson(), data = continental)
summary(poisson.continental)

Dsquared(poisson.continental)

## Macaronesian
macaronesian <- filter(hr_bioreg_dist.df, bioreg == 'Macaronesian')
macaronesian$min_dst <- macaronesian$min_dst/1000

# LM
lm_macaronesian <- lm(hbtt_rc ~ min_dst, data = macaronesian)
summary(lm_macaronesian)

# GLM Poisson
poisson.macaronesian <- glm(hbtt_rc ~ min_dst, family = poisson(), data = macaronesian)
summary(poisson.macaronesian)

Dsquared(poisson.macaronesian)

##  Mediterranean
mediterranean <- filter(hr_bioreg_dist.df, bioreg == 'Mediterranean')
mediterranean$min_dst <- mediterranean$min_dst/1000

# LM
lm_mediterranean <- lm(hbtt_rc ~ min_dst, data = mediterranean)
summary(lm_mediterranean)

# GLM Poisson
poisson.mediterranean <- glm(hbtt_rc ~ min_dst, family = poisson(), data = mediterranean)
summary(poisson.mediterranean)

Dsquared(poisson.mediterranean)

## Pannonian
pannonian <- filter(hr_bioreg_dist.df, bioreg == 'Pannonian')
pannonian$min_dst <- pannonian$min_dst/1000

# LM
lm_pannonian <- lm(hbtt_rc ~ min_dst, data = pannonian)
summary(lm_pannonian)

# GLM Poisson
poisson.pannonian <- glm(hbtt_rc ~ min_dst, family = poisson(), data = pannonian)
summary(poisson.pannonian)

Dsquared(poisson.pannonian)

## Steppic
steppic <- filter(hr_bioreg_dist.df, bioreg == 'Steppic')
steppic$min_dst <- steppic$min_dst/1000

# LM
lm_steppic <- lm(hbtt_rc ~ min_dst, data = steppic)
summary(lm_steppic)

# GLM Poisson
poisson.steppic <- glm(hbtt_rc ~ min_dst, family = poisson(), data = steppic)
summary(poisson.steppic)

Dsquared(poisson.steppic)


## Visualization of fitted Generalised Linear Model line


install.packages("ggpubr")
library(ggpubr) # creating and customizing 'ggplot2'

## Plot GLM Total
fitglm = glm(hr_bin ~ min_dst, family = poisson(), data=hr_bioreg_dist.df)

hr_bioreg_dist.df$predglm = predict(fitglm, type="response")

glm.tot <- ggplot(hr_bioreg_dist.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.021") +
    theme_classic() +
  labs(title="Total") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())


## Alpine
fitglm.al = glm(hr_bin ~ min_dst, family = poisson(), data=alpine.df)

alpine.df$predglm = predict(fitglm.al, type ="response")

glm.al <- ggplot(alpine.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.002") +
  theme_classic() +
  labs(title="Alpine") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Atlantic
fitglm.at = glm(hr_bin ~ min_dst, family = poisson(), data=atlantic.df)

atlantic.df$predglm = predict(fitglm.at, type ="response")

glm.at <- ggplot(atlantic.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.117") +
  theme_classic() +
  labs(title="Atlantic") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Black Sea
fitglm.b = glm(hr_bin ~ min_dst, family = poisson(), data=black_sea.df)

black_sea.df$predglm = predict(fitglm.b, type ="response")

glm.b <- ggplot(black_sea.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.081") +
  theme_classic() +
  labs(title="Black Sea") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Boreal
fitglm.bo = glm(hr_bin ~ min_dst, family = poisson(), data=boreal.df)

boreal.df$predglm = predict(fitglm.bo, type = "response")

glm.bo <- ggplot(boreal.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.125") +
  theme_classic() +
  labs(title="Boreal") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Continental
fitglm.c = glm(hr_bin ~ min_dst, family = poisson(),  data=continental.df)

continental.df$predglm = predict(fitglm.c, type = "response")

glm.c <- ggplot(continental.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.01") +
  theme_classic() +
  labs(title="Continental") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Macaronesian
fitglm.ma = glm(hr_bin ~ min_dst, family = poisson(), data=macaronesian.df)

macaronesian.df$predglm = predict(fitglm.ma, type ="response")

glm.ma <- ggplot(macaronesian.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.002") +
  theme_classic() +
  labs(title="Macaronesian") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Mediterranean
fitglm.me = glm(hr_bin ~ min_dst, family = poisson(), data=mediterranean.df)

mediterranean.df$predglm = predict(fitglm.me, type ="response")

glm.me <- ggplot(mediterranean.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.017") +
  theme_classic() +
  labs(title="Mediterranean") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Pannonian
fitglm.p = glm(hr_bin ~ min_dst, family = poisson(), data=pannonian.df)

pannonian.df$predglm = predict(fitglm.p, type ="response")

glm.p <- ggplot(pannonian.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.021") +
  theme_classic() +
  labs(title="Pannonian") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Steppic
fitglm.s = glm(hr_bin ~ min_dst, family = poisson(), data=steppic.df)

steppic.df$predglm = predict(fitglm.s, type ="response")

glm.s <- ggplot(hr_bioreg_dist.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkblue") +
  geom_line(aes(y=predglm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "D²: 0.088") +
  theme_classic() +
  labs(title="Steppic") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

# Final plot
map1 <- ggarrange(glm.tot, glm.al, glm.at, glm.b, glm.bo,
                 glm.c, glm.ma, glm.me, glm.p, glm.s,
                 ncol = 3, nrow= 4)
Finalmap <- annotate_figure(map1, 
                bottom = text_grob("Distance (Km)"),
                left = text_grob("Habitat Richness", rot = 90))
Finalmap

ggsave("glm.png", Finalmap, width = 8, height = 8, dpi = 300)


## Visualization of fitted Linear regression Model line

# Plotting LM Total
fitlm = lm(hr_bin ~ min_dst, data=hr_bioreg_dist.df)

hr_bioreg_dist.df$predlm = predict(fitlm)

plot.tot <- ggplot(hr_bioreg_dist.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.021") +
  theme_classic() +
  labs(title="Total") + 
  theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Alpine

fitlm.al = lm(hr_bin ~ min_dst, data = alpine.df)

alpine.df$predlm = predict(fitlm.al)

plot.al <- ggplot(alpine.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.026") +
  theme_classic() +
  labs(title="Alpine") + theme(plot.title=element_text( hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Atlantic

fitlm.at = lm(hr_bin ~ min_dst, data = atlantic.df)

atlantic.df$predlm = predict(fitlm.at)

plot.at <- ggplot(atlantic.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.114") +
  theme_classic() +
  labs(title="Atlantic") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Black Sea
fitlm.b = lm(hr_bin ~ min_dst, data = black_sea.df)

black_sea.df$predlm = predict(fitlm.b)

plot.b <- ggplot(black_sea.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.11") +
  theme_classic() +
  labs(title="Black Sea") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Boreal
fitlm.bo = lm(hr_bin ~ min_dst, data = boreal.df)

boreal.df$predlm = predict(fitlm.bo)

plot.bo <- ggplot(boreal.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.122") +
  theme_classic() +
  labs(title="Boreal") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Continental
fitlm.c = lm(hr_bin ~ min_dst, data = continental.df)

continental.df$predlm = predict(fitlm.c)

plot.c <- ggplot(continental.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.01") +
  theme_classic() +
  labs(title="Continental") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Macaronesian
fitlm.ma = lm(hr_bin ~ min_dst, data = macaronesian.df)

macaronesian.df$predlm = predict(fitlm.ma)

plot.ma <- ggplot(macaronesian.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.002") +
  theme_classic() +
  labs(title="Macaronesian") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Mediterranean
fitlm.me = lm(hr_bin ~ min_dst, data = mediterranean.df)

mediterranean.df$predlm = predict(fitlm.me)

plot.me <- ggplot(mediterranean.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.018") +
  theme_classic() +
  labs(title="Mediterranean") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Pannonnian
fitlm.p = lm(hr_bin ~ min_dst, data = pannonian.df)

pannonian.df$predlm = predict(fitlm.p)

plot.p <- ggplot(pannonian.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.022") +
  theme_classic() +
  labs(title="Pannonian") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

## Steppic
fitlm.s = lm(hr_bin ~ min_dst, data = steppic.df)

steppic.df$predlm = predict(fitlm.s)

plot.s <- ggplot(steppic.df, aes(x=min_dst, y=hr_bin)) +
  geom_point(alpha=0.3, col="darkgreen") +
  geom_line(aes(y=predlm), size=1, col="red") +
  xlim(0,350) + ylim(0,70) +
  annotate("text", x = 290, y = 65, label = "R²: 0.075") +
  theme_classic() +
  labs(title="Steppic") + theme(plot.title=element_text(hjust = 0.5)) +
  theme(axis.title.x  = element_blank(),
        axis.title.y = element_blank())

map <- ggarrange(plot.tot, plot.al, plot.at, plot.b, plot.bo,
          plot.c, plot.ma, plot.me, plot.p, plot.s,
          ncol = 3, nrow= 4)
final.lm <- annotate_figure(map, 
                bottom = text_grob("Distance (Km)"),
                left = text_grob("Habitat Richness", rot = 90))               
final.lm

ggsave("lm.png", final.lm, width = 8, height = 8, dpi = 300)

