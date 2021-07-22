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

# Libraries
library(rgdal)
library(vegan)
library(sf)
library(readxl)
library(dplyr)
library(stats)
library(modEvA)

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
summary(mgcv::gam(habitat_df$habitat_richness ~ s(habitati_df$min_dist)))

#################################################################

# Importo lo .shp file del dataset contenente distanze minime, regioni biogeografiche e habitat richness
hr_bioreg_dist <- readOGR("hr_bioreg_dist.shp")
summary(hr_bioreg_dist)
str(hr_bioreg_dist)

# Converto a dataframe
hr_bioreg_dist.df <- as.data.frame(hr_bioreg_dist)
summary(hr_bioreg_dist.df)
str(hr_bioreg_dist.df)

plot(hr_bioreg_dist.df$min_dst, hr_bioreg_dist.df$hbtt_rc, xlab = "minimum distance", ylab = "habitat richness",
     main = "Minimum distance/Habitat richness")
abline(lm,lwd = 3,  col= "red")


## LM: habitat richness ~ distanza minima
lm <- lm(hbtt_rc ~ min_dst, data = hr_bioreg_dist.df)
summary(lm)

#Call:
  lm(formula = hbtt_rc ~ min_dst, data = hr_bioreg_dist.df)

#Residuals:
  Min      1Q  Median      3Q     Max 
#-26.068  -8.741  -2.011   5.932 108.932 

#Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
 (Intercept)  2.707e+01  7.810e-02  346.61   <2e-16 ***
  min_dst     -5.717e-05  9.574e-07  -59.71   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 13.27 on 49598 degrees of freedom
#Multiple R-squared:  0.06706,	Adjusted R-squared:  0.06704 
#F-statistic:  3565 on 1 and 49598 DF,  p-value: < 2.2e-16
  
abline(lm,lwd = 3,  col= "red")

## Polynomial regression
polynomial <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = hr_bioreg_dist.df)
summary(polynomial)

#Call:
  lm(formula = hbtt_rc ~ min_dst + I(min_dst^2), data = hr_bioreg_dist.df)

#Residuals:
  Min      1Q  Median      3Q     Max 
-28.446  -8.446  -1.494   6.232 106.554 

#Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.945e+01  8.946e-02  329.13   <2e-16 ***
  min_dst      -1.820e-04  2.636e-06  -69.04   <2e-16 ***
  I(min_dst^2)  6.319e-10  1.248e-11   50.63   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 12.94 on 49597 degrees of freedom
#Multiple R-squared:  0.1129,	Adjusted R-squared:  0.1129 
#F-statistic:  3157 on 2 and 49597 DF,  p-value: < 2.2e-16

lines(smooth.spline(min_dst, polynomial), data = hr_bioreg_dist, col="blue", lwd=3)

# using partial F-test
anova(lm, polynomial)

#Analysis of Variance Table

#Model 1: hbtt_rc ~ min_dst
#Model 2: hbtt_rc ~ min_dst + I(min_dst^2)
#Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
#1  49598 8732564                                  
#2  49597 8303359  1    429206 2563.7 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# fit a model that includes min_dst^3 as well
model3 <- lm(hbtt_rc ~ min_dst + I(min_dst^2) + I(min_dst^3), data = hr_bioreg_dist.df)
summary(model3)

attributes(lm)

plot(hr_bioreg_dist.df$min_dst, hr_bioreg_dist.df$hbtt_rc)

poisson <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = hr_bioreg_dist.df)
summary(poisson)

#Call:
  glm(formula = hbtt_rc ~ min_dst, family = poisson(link = "log"), 
      data = hr_bioreg_dist.df)

#Deviance Residuals: 
  Min      1Q  Median      3Q     Max  
-6.790  -1.903  -0.406   1.195  14.794  

#Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.309e+00  1.169e-03    2831   <2e-16 ***
  min_dst     -2.688e-06  1.670e-08    -161   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 360060  on 49599  degrees of freedom
#Residual deviance: 331790  on 49598  degrees of freedom
#AIC: 572923

#Number of Fisher Scoring iterations: 4
  
Dsquared(poisson)

# [1] 0.07851531

summary(gam(hr_bioreg_dist.df$hbtt_rc ~ s(hr_bioreg_dist.df$min_dst)))

# Family: gaussian 
# Link function: identity 

# Formula:
hr_bioreg_dist.df$hbtt_rc ~ s(hr_bioreg_dist.df$min_dst)

# Parametric coefficients:
Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 24.05363    0.05577   431.3   <2e-16 ***

  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
edf Ref.df    F p-value    
# s(hr_bioreg_dist.df$min_dst) 8.975      9 1232  <2e-16 ***
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.183   Deviance explained = 18.3%
# GCV = 154.28  Scale est. = 154.25    n = 49600


########### 'Alpine'

alpine <- filter(hr_bioreg_dist.df, bioreg == 'Alpine')

lm_alpine <- lm(hbtt_rc ~ min_dst, data = alpine)
summary(lm_alpine)

plot(alpine$hbtt_rc ~ alpine$min_dst)

# Polynomial regression
polynomial.alpine <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = alpine)
summary(polynomial.alpine)

# Calcolo GLM Poisson
poisson.alpine <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = alpine)
summary(poisson.alpine)
plot(poisson.alpine)

########### 'Atlantic'

atlantic <- filter(hr_bioreg_dist.df, bioreg == 'Atlantic')

lm_atlantic <- lm(hbtt_rc ~ min_dst, data = atlantic)
summary(lm_atlantic)

plot(atlantic$hbtt_rc ~ atlantic$min_dst)

# Polynomial regression
polynomial.atlantic <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = atlantic)
summary(polynomial.atlantic)

# Calcolo GLM Poisson
poisson.atlantic <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = atlantic)
summary(poisson.atlantic)
plot(poisson.atlantic)

########### 'Black Sea'

black_sea <- filter(hr_bioreg_dist.df, bioreg == 'Black Sea')

lm_black_sea <- lm(hbtt_rc ~ min_dst, data = black_sea)
summary(lm_black_sea)

plot(black_sea$hbtt_rc ~ black_sea$min_dst)

# Polynomial regression
polynomial.black_sea <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = black_sea)
summary(polynomial.black_sea)

# Calcolo GLM Poisson
poisson.black_sea <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = black_sea)
summary(poisson.black_sea)
plot(poisson.black_sea)

########## 'Boreal'

boreal <- filter(hr_bioreg_dist.df, bioreg == 'Boreal')

lm_boreal <- lm(hbtt_rc ~ min_dst, data = boreal)
summary(lm_boreal)

plot(boreal$hbtt_rc ~ boreal$min_dst)

# Polynomial regression
polynomial.boreal <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = boreal)
summary(polynomial.boreal)

# Calcolo GLM Poisson
poisson.boreal <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = boreal)
summary(poisson.boreal)
plot(poisson.boreal)

##########  'Continental'

continental <- filter(hr_bioreg_dist.df, bioreg == 'Continental')

lm_continental <- lm(hbtt_rc ~ min_dst, data = continental)
summary(lm_continental)

plot(continental$hbtt_rc ~ continental$min_dst)

# Polynomial regression
polynomial.continental <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = continental)
summary(polynomial.continental)

# Calcolo GLM Poisson
poisson.continental <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = continental)
summary(poisson.continental )
plot(poisson.continental)

########## Importo 'Macaronesian'

macaronesian <- filter(hr_bioreg_dist.df, bioreg == 'Macaronesian')

lm_macaronesian <- lm(hbtt_rc ~ min_dst, data = macaronesian)
summary(lm_macaronesian)

plot(macaronesian$hbtt_rc ~ macaronesian$min_dst)

# Polynomial regression
polynomial.macaronesian <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = macaronesian)
summary(polynomial.macaronesian)

# Calcolo GLM Poisson
poisson.macaronesian <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = macaronesian)
summary(poisson.macaronesian)
plot(poisson.macaronesian)

########## Importo 'Mediterranean'

mediterranean <- filter(hr_bioreg_dist.df, bioreg == 'Mediterranean')

lm_mediterranean <- lm(hbtt_rc ~ min_dst, data = mediterranean)
summary(lm_mediterranean)

plot(mediterranean$hbtt_rc ~ mediterranean$min_dst)

# Polynomial regression
polynomial.mediterranean <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = mediterranean)
summary(polynomial.mediterranean)

# Calcolo GLM Poisson
poisson.mediterranean <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = mediterranean)
summary(poisson.mediterranean)
plot(poisson.mediterranean)

########### 'Pannonian'

pannonian <- filter(hr_bioreg_dist.df, bioreg == 'Pannonian')

lm_pannonian <- lm(hbtt_rc ~ min_dst, data = pannonian)
summary(lm_pannonian)
plot(pannonian$hbtt_rc ~ pannonian$min_dst)

# Polynomial regression
polynomial.pannonian <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = pannonia)
summary(polynomial.pannonian)

# Calcolo GLM Poisson
poisson.pannonian <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = pannonian)
summary(poisson.pannonian)
plot(poisson.pannonian)

########### Steppic

steppic <- filter(hr_bioreg_dist.df, bioreg == 'Steppic')

lm_steppic <- lm(hbtt_rc ~ min_dst, data = steppic)
summary(lm_steppic)
plot(lm_steppic)

plot(steppic$hbtt_rc, steppic$min_dst)

mean(steppic$hbtt_rc)
# [1] 6.803828

mean(steppic$min_dst)
# [1] 25185.55

var(steppic$hbtt_rc)
#  24.13888

var(steppic$min_dst)
# [1] 467094878

# Polynomial regression
polynomial.steppic <- lm(hbtt_rc ~ min_dst + I(min_dst^2), data = steppic)
summary(polynomial.steppic)

# Calcolo GLM Poisson
poisson.steppic <- glm(hbtt_rc ~ min_dst, family = poisson(link = "log"), data = steppic)
summary(poisson.steppic)
plot(poisson.steppic)


