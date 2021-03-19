# R_code_EBR.r

## Habitat diversity and biogeographical regionalisation

# Working directory
setwd("C:/R/")

# Libraries
library(rgdal)
library(vegan)
library(sf)

## Importo immagine Europe_habitat
habitat <- readOGR("europe_habitats.shp")

## Import 'Regioni Biogeografiche'
EBR <- readOGR('BiogeoRegions2016.shp')

# Importo lo shapefile filtrato
# Ho creato un nuovo .shp file tramite QGis selezionando alcuni quadrati
biogeo_habitat <- readOGR("Selezione.shp")
biogeo_habitat
summary(biogeo_habitat)
str(biogeo_habitat)

# Converto lo .shp file in dataframe
df <- as.data.frame(biogeo_habitat)
names(df)

# Sostituisco valori della tabella in 'presence/absence'
presence_absence <- decostand(df[,4:ncol(df)], "pa")
is.na(biogeo_habitat)

# Habitat richness
habitat_richness <- rowSums(presence_absence)

# Aggiungo la colonna 'habitat_richness' al dataframe 
add <- cbind(df, habitat_richness)
names(add)
