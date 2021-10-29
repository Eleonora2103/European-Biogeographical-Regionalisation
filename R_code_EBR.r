# R_code_EBR.r

## Habitat diversity and biogeographical regionalisation

# Working directory
setwd("C:/R/")

install.packages("rgdal")
install.packages("vegan")
install.packages("sf")
install.packages("readxl")
install.packages("dplyr")
install.packages("stats")
install.packages("modEvA")
install.packages("mgcv")
install.packages("glmmsr")
install.packages("lme4")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("stargazer")

# Libraries
library(rgdal)
library(vegan)
library(sf)
library(readxl)
library(dplyr)
library(stats)
library(modEvA)
library(mgcv)
library(glmmsr)
library(lme4)
library(ggplot2)
library(gridExtra)
library(stargazer)

## Importo immagine Europe_habitat
habitat <- readOGR("europe_habitats.shp")

## Import 'Regioni Biogeografiche'
biogeo_region <- readOGR('BiogeoRegions2016.shp')

# Importo lo shapefile filtrato
# Ho creato un nuovo .shp file tramite QGis selezionando alcuni quadrati
biogeo_habitat <- readOGR("Selezione.shp")
biogeo_habitat
summary(biogeo_habitat)
str(biogeo_habitat)

# Converto lo .shp file in dataframe
habitat_df <- as.data.frame(biogeo_habitat)
head(habitat_df)
names(habitat_df)
str(habitat_df)

# Sostituisco valori della tabella in 'presence/absence'
presence_absence <- decostand(habitat_df[,4:ncol(habitat_df)], "pa")

# Habitat richness
habitat_richness <- rowSums(presence_absence)


#####################################################################

# Calcolo la distanza dal margine più vicino

# Utilizzo lo Shapefile filtrato (Selezione.shp) e quello delle regioni Biogeografiche (BiogeoRegions2016.shp)
sf_region <- st_as_sf(biogeo_region)
sf_habitat <- st_as_sf(biogeo_habitat)
sf_habitat

# trasformo il layer del confine biogeografico da poligono a linea
border <- st_cast(sf_region, "MULTILINESTRING")
class(border)

# calcolo la distanza tra il confine ed i punti della griglia selezionata
dist <- st_distance(sf_habitat, border)
str(dist)

# creo il dataframe della distanza
dist.df <- as.data.frame(dist)
dim(dist.df)

# calcolare la distanza minima dal confine per ogni cella
min_dist <- apply(dist.df, 1, FUN=min)
str(min_dist)

head(min_dist)
# [1]    0.000    0.000    0.000    0.000    0.000 3617.966

# Aggiungo la colonna 'habitat_richness' al dataframe 
habitat_df <- cbind(habitat_df, habitat_richness)
names(habitat_df)

# aggiungo la distanza dal confine più vicino al dataframe 'habitat_df'
habitat_df <- cbind(habitat_df, min_dist)
names(habitat_df)
head(habitat_df)

# Grafico distanza/habitat richness
library(mgcv)
plot(habitat_df$min_dist, habitat_df$habitat_richness)

#################################################################

# Importo lo .shp file del dataset contenente distanze minime, regioni biogeografiche e habitat richness
hr_bioreg_dist <- readOGR("hr_bioreg_dist.shp")
summary(hr_bioreg_dist)
str(hr_bioreg_dist)
summary(hr_bioreg_dist$bioreg)

# Converto a dataframe
hr_bioreg_dist.df <- as.data.frame(hr_bioreg_dist)
summary(hr_bioreg_dist.df)
str(hr_bioreg_dist.df)

#Conversione da metri a kilometri 
hr_bioreg_dist.df$min_dst <- hr_bioreg_dist.df$min_dst/1000

saveRDS(hr_bioreg_dist.df, file = "data.Rds")
load("data.Rda")

##########################################
for(i in 1:49600){
  hr_bioreg_dist.df$min_dst[i] <- (hr_bioreg_dist.df$min_dst[i]/1000) 
}
##########################################

plot(hr_bioreg_dist.df$min_dst, hr_bioreg_dist.df$hbtt_rc, xlab = "minimum distance", ylab = "habitat richness",
     main = "Minimum distance/Habitat richness")
abline(lm,lwd = 3,  col= "red")

## LM: habitat richness ~ distanza minima
lm.total <- lm(hbtt_rc ~ (min_dst), data = hr_bioreg_dist.df)
summary(lm.total)

range(lm.total$min_dst)

## Polynomial regression
polynomial <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = hr_bioreg_dist.df)
summary(polynomial)

plot(hr_bioreg_dist.df$min_dst, hr_bioreg_dist.df$hbtt_rc)

#GLM
poisson.total <- glm(hbtt_rc ~ min_dst, family = poisson(), data = hr_bioreg_dist.df)
summary(poisson.total)
total.dataframe <- as.data.frame(poisson.total)

Dsquared(poisson.total)

#
(plot.glm.total <- ggplot(hr_bioreg_dist.df, aes(x = min_dst, y = hbtt_rc, color = "red")) + 
  geom_point() +
    geom_smooth(method="glm") +
  geom_line(data= cbind(hr_bioreg_dist.df, pred = predict(poisson.total)), aes(y=pred), size = 1, color="black"))

#
range(poisson.total$coefficents$, na.rm = TRUE)
xweight <- seq(-Inf, Inf)
yweight <- predict(poisson.total, list(min_dst = xweight), type = "response")
plot(poisson.total$min_dst, poisson.total$hbtt_rc, xlab = "Minimun distance", ylab= "Habitat Richness")
lines(xweight, yweight)

#
ggplot2::ggplot(data = poisson.total, aes(x = min_dst, y = hbtt_rc)) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))

ggplot2::ggplot(data = poisson.total, aes(x = min_dst, y = hbtt_rc)) + 
  geom_point()

# GAM
gam.total <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = hr_bioreg_dist.df, family = poisson())
summary(gam.total)
plot(gam.total)

summary(mgcv::gam(hr_bioreg_dist.df$hbtt_rc ~ s(hr_bioreg_dist.df$min_dst)))

#GLMM
# habitat_richness ~ distanza_dal_margine + (1|bioreg)
glmm.total <- lme4::glmer(hbtt_rc ~ min_dst + (1|bioreg), data = hr_bioreg_dist.df)

# LMM
lmm.total <- lme4::lmer(hbtt_rc ~ min_dst + (1|bioreg), data = hr_bioreg_dist.df)

########### 'Alpine'

alpine <- filter(hr_bioreg_dist.df, bioreg == 'Alpine')
alpine$min_dst <- alpine$min_dst/1000
alpine.df <- as.data.frame(alpine)

# LM
lm_alpine <- lm(hbtt_rc ~ min_dst, data = alpine)
summary(lm_alpine)

# Polynomial regression
polynomial.alpine <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = alpine)
summary(polynomial.alpine)

# Calcolo GLM Poisson
poisson.alpine <- glm(hbtt_rc ~ min_dst, family = poisson(), data = alpine)
summary(poisson.alpine)

Dsquared(poisson.alpine)

(plot.glm.total <- ggplot(alpine, aes(x = min_dst, y = hbtt_rc, color = "green")) + 
    geom_point(alpha = 0.5) + 
    xlim(0,300) +
    geom_line(data= cbind(alpine, pred = predict(poisson.alpine)), aes(y=pred), size = 1, color="black") +
    labs(x = "Minimum Distance (Km)", y = "Habitat Richness",
         title = "Alpine"))

# GAM
gam.alpine <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = alpine, family = poisson())
summary(gam.alpine)
plot(gam.alpine)

# GLMM
glmm_alpine <- glmm(hbtt_rc ~ min_dst + (1|bioreg), data = alpine, family = binomial, method = "Laplace")

########### 'Atlantic'

atlantic <- filter(hr_bioreg_dist.df, bioreg == 'Atlantic')
atlantic$min_dst <- atlantic$min_dst/1000

# LM
lm_atlantic <- lm(hbtt_rc ~ min_dst, data = atlantic)
summary(lm_atlantic)

plot(atlantic$hbtt_rc ~ atlantic$min_dst)

# Polynomial regression
polynomial.atlantic <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = atlantic)
summary(polynomial.atlantic)

# Calcolo GLM Poisson
poisson.atlantic <- glm(hbtt_rc ~ min_dst, family = poisson(), data = atlantic)
summary(poisson.atlantic)
plot(poisson.atlantic)

Dsquared(poisson.atlantic)

# GAM
gam.atlantic <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = atlantic, family = poisson())
summary(gam.atlantic)

plot(gam.atlantic)

########### 'Black Sea'

black_sea <- filter(hr_bioreg_dist.df, bioreg == 'Black Sea')
black_sea$min_dst <- black_sea$min_dst/1000
black_sea.df <- as.data.frame(black_sea)

# LM
lm_black_sea <- lm(hbtt_rc ~ min_dst, data = black_sea)
summary(lm_black_sea)

plot(black_sea$hbtt_rc ~ black_sea$min_dst)

# Polynomial regression
polynomial.black_sea <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = black_sea)
summary(polynomial.black_sea)

# Calcolo GLM Poisson
poisson.black_sea <- glm(hbtt_rc ~ min_dst, family = poisson(), data = black_sea)
summary(poisson.black_sea)
plot(poisson.black_sea)

Dsquared(poisson.black_sea)

(plot.glm.blacksea <- ggplot(black_sea, aes(x = min_dst, y = hbtt_rc, color = "yellow")) + 
    geom_point(alpha = 0.5) + 
    xlim(0,350) +
    geom_line(data= cbind(black_sea, pred = predict(poisson.black_sea)), aes(y=pred), size = 1, color="black") +
    labs(x = "Minimum Distance (Km)", y = "Habitat Richness",
         title = "Black Sea"))

# GAM
gam.black_sea <- gam(hbtt_rc ~ s(min_dst, k = 3),  data = black_sea, family = poisson())
summary(gam.black_sea)
plot(gam.black_sea)

########## 'Boreal'
boreal <- filter(hr_bioreg_dist.df, bioreg == 'Boreal')
boreal$min_dst <- boreal$min_dst/1000

# LM
lm_boreal <- lm(hbtt_rc ~ min_dst, data = boreal)
summary(lm_boreal)

plot(boreal$hbtt_rc ~ boreal$min_dst)

# Polynomial regression
polynomial.boreal <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = boreal)
summary(polynomial.boreal)

# Calcolo GLM Poisson
poisson.boreal <- glm(hbtt_rc ~ min_dst, family = poisson(), data = boreal)
summary(poisson.boreal)
plot(poisson.boreal)

Dsquared(poisson.boreal)

# GAM
gam.boreal <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = boreal, family = poisson())
summary(gam.boreal)
plot(gam.boreal)

##########  'Continental'
continental <- filter(hr_bioreg_dist.df, bioreg == 'Continental')
continental$min_dst <- continental$min_dst/1000

# LM
lm_continental <- lm(hbtt_rc ~ min_dst, data = continental)
summary(lm_continental)

plot(continental$hbtt_rc ~ continental$min_dst)

# Polynomial regression
polynomial.continental <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = continental)
summary(polynomial.continental)

# Calcolo GLM Poisson
poisson.continental <- glm(hbtt_rc ~ min_dst, family = poisson(), data = continental)
summary(poisson.continental)
plot(poisson.continental)

Dsquared(poisson.continental)

# GAM
gam.continental <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = continental, family = poisson())
summary(gam.continental)
plot(gam.continental)

########## Importo 'Macaronesian'
macaronesian <- filter(hr_bioreg_dist.df, bioreg == 'Macaronesian')
macaronesian$min_dst <- macaronesian$min_dst/1000

# LM
lm_macaronesian <- lm(hbtt_rc ~ min_dst, data = macaronesian)
summary(lm_macaronesian)

plot(macaronesian$hbtt_rc ~ macaronesian$min_dst)

# Polynomial regression
polynomial.macaronesian <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = macaronesian)
summary(polynomial.macaronesian)

# Calcolo GLM Poisson
poisson.macaronesian <- glm(hbtt_rc ~ min_dst, family = poisson(), data = macaronesian)
summary(poisson.macaronesian)
plot(poisson.macaronesian)

Dsquared(poisson.macaronesian)

# GAM
gam.macaronesian <- gam(hbtt_rc ~ s(min_dst, k = -1),  data = macaronesian, family = poisson())
summary(gam.macaronesian)
plot(gam.macaronesian)

 ########## Importo 'Mediterranean'
mediterranean <- filter(hr_bioreg_dist.df, bioreg == 'Mediterranean')
mediterranean$min_dst <- mediterranean$min_dst/1000

# LM
lm_mediterranean <- lm(hbtt_rc ~ min_dst, data = mediterranean)
summary(lm_mediterranean)

plot(mediterranean$hbtt_rc ~ mediterranean$min_dst)

# Polynomial regression
polynomial.mediterranean <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = mediterranean)
summary(polynomial.mediterranean)

# Calcolo GLM Poisson
poisson.mediterranean <- glm(hbtt_rc ~ min_dst, family = poisson(), data = mediterranean)
summary(poisson.mediterranean)

Dsquared(poisson.mediterranean)

# GAM
gam.mediterranean <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = mediterranean, family = poisson())
summary(gam.mediterranean)
plot(gam.mediterranean)

########### 'Pannonian'
pannonian <- filter(hr_bioreg_dist.df, bioreg == 'Pannonian')
pannonian$min_dst <- pannonian$min_dst/1000
plot(pannonian$hbtt_rc ~ pannonian$min_dst)

# LM
lm_pannonian <- lm(hbtt_rc ~ min_dst, data = pannonian)
summary(lm_pannonian)

# Polynomial regression
polynomial.pannonian <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = pannonia)
summary(polynomial.pannonian)

# Calcolo GLM Poisson
poisson.pannonian <- glm(hbtt_rc ~ min_dst, family = poisson(), data = pannonian)
summary(poisson.pannonian)

Dsquared(poisson.pannonian)

# GAM
gam.pannonian <- gam(hbtt_rc ~ s(min_dst, k = 3),  data = pannonian, family = poisson())
summary(gam.pannonian)
plot(gam.pannonian)

########### Steppic
steppic <- filter(hr_bioreg_dist.df, bioreg == 'Steppic')
steppic$min_dst <- steppic$min_dst/1000

# LM
lm_steppic <- lm(hbtt_rc ~ min_dst, data = steppic)
summary(lm_steppic)
plot(lm_steppic)

plot(steppic$hbtt_rc, steppic$min_dst)

# Polynomial regression
polynomial.steppic <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = steppic)
summary(polynomial.steppic)

# Calcolo GLM Poisson
poisson.steppic <- glm(hbtt_rc ~ min_dst, family = poisson(), data = steppic)
summary(poisson.steppic)

Dsquared(poisson.steppic)

# GAM
gam.steppic <- gam(hbtt_rc ~ s(min_dst, k = 4),  data = steppic, family = poisson())
summary(gam.steppic)
plot(gam.pannonian)
gam.check(gam.pannonian)

###############################################################

# Regione Biogeografica come variabile qualitativa

# LM
lm_bioreg <- lm(hbtt_rc ~ factor(bioreg) * min_dst, data = hr_bioreg_dist.df)
summary(lm_bioreg)

lm_bioreg$coefficients

# GLM
glm_bioreg <- glm(hbtt_rc ~ factor(bioreg) + min_dst, data = hr_bioreg_dist.df, family = poisson())
summary(glm_bioreg)

Dsquared(glm_bioreg)

glm_bioreg$coefficients

# GAM
gam_bioreg <- gam(hbtt_rc ~ s(factor(bioreg) + min_dst), data = hr_bioreg_dist.df, family = poisson ())
summary(gam_bioreg)
gam_bioreg$coefficients


##################################
# Plot total dataset vs Alpine


(plot.glm.total <- ggplot(hr_bioreg_dist.df, aes(x = min_dst, y = hbtt_rc)) + 
    geom_point(col="red") +
    geom_line(data= cbind(hr_bioreg_dist.df, pred = predict(poisson.total)), aes(y=pred), size = 1, col="black") +
    xlab("Minimum Distance") + ylab("Habitat Richness")
    labs(title = "Total"))


(plot.glm.blacksea <- ggplot(black_sea.df, aes(x = min_dst, y = hbtt_rc)) + 
    geom_point(col="green") +
    xlab("Minimum Distance") + ylab("Habitat Richness")
    geom_line(data= cbind(black_sea.df, pred.blacksea = predict(poisson.black_sea)), aes(y=pred.blacksea), size = 1, col="black") +
    labs(title = "Black Sea"))

grid.arrange(plot.glm.total, plot.glm.blacksea)
dev.off()

(plot.glm.alpine <- ggplot(alpine.df, aes(x = min_dst, y = hbtt_rc)) + 
    geom_point(col="yellow") + xlim(0,350) +
    geom_line(data= cbind(alpine.df, pred.alpine = predict(poisson.alpine)), aes(y=pred.alpine), size = 1, col="black") +
    geom_smooth(method = glm) +
    labs(x = "Minimum Distance (Km)", y = "Habitat Richness",
         title = "Alpine"))

#######################################################
(plot.glm <- ggplot(hr_bioreg_dist.df, aes(x=min_dst, y=hbtt_rc, col = hbtt_rc)) +
    geom_point() +
    xlab("Minimum Distance") + ylab("Habitat Richness") +
    geom_smooth(method = glm, col= "red" , fill = "red", alpha = 0.5) +
    theme_classic() +
    labs(title = "Total"))

(plot.blacksea <- ggplot(black_sea.df, aes(x=min_dst, y=hbtt_rc, col = hbtt_rc)) +
    geom_point() +
    xlab("Minimum Distance") + ylab("Habitat Richness") +
    geom_smooth(method = glm, col= "red" , fill = "red", alpha = 0.5) +
    theme_classic() +
    labs(title = "Black Sea"))

grid.arrange(plot.glm, plot.blacksea)

############
# Table

# Total
stargazer(poisson.total, type = "text",
          digits = 3,
          digit.separator = "")
