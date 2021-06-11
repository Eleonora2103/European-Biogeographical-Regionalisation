# R_code_EBR.r

## Habitat diversity and biogeographical regionalisation

# Working directory
setwd("C:/R/")

install.packages("rgdal")
install.packages("vegan")
install.packages("sf")

# Libraries
library(rgdal)
library(vegan)
library(sf)

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

