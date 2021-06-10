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
df <- as.data.frame(biogeo_habitat)
head(df)
names(df)
str(df)

# Sostituisco valori della tabella in 'presence/absence'
presence_absence <- decostand(df[,4:ncol(df)], "pa")
is.na(biogeo_habitat)

# Habitat richness
habitat_richness <- rowSums(presence_absence)

# Aggiungo la colonna 'habitat_richness' al dataframe 
df <- cbind(df, habitat_richness)
names(df)

#####################################################################

# Calcolo la distanza dal margine più vicino

# Utilizzo lo Shapefile filtrato (Selezione.shp) e quello delle regioni Biogeografiche (BiogeoRegions2016.shp)
sf <- st_as_sf(biogeo_region)
sf_habitat <- st_as_sf(biogeo_habitat)
sf_habitat

# trasformo il layer del confine biogeografico da poligono a linea
border <- st_cast(sf, "MULTILINESTRING")
class(border)

# calcolo la distanza tra il confine ed i punti della griglia selezionata
dist_1 <- st_distance(sf_habitat, border)
str(dist_1)

# creo il dataframe della distanza
dist.df <- as.data.frame(dist_1)
dim(dist.df)

# calcolare la distanza minima dal confine per ogni cella
min_dist <- apply(dist.df, 1, FUN=min)
str(min_dist)

head(min_dist)
# [1]    0.000    0.000    0.000    0.000    0.000 3617.966

# aggiungo la distanza dal confine più vicino al dataframe 'df'
df <- cbind(df, min_dist)
names(df)
head(df)
# Grafico distanza-habitat richness

plot(df$min_dist, df$habitat_richness)
summary(lm(df$habitat_richness ~ df$min_dist))
