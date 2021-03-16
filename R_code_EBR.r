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
# Ho creato un nuovo .shp file tramite QGis selezionando alcuni quadranti ed ho filtrato il layer 'Alpine' dal file 'Regioni Biogeografiche'
biogeo_habitat <- readOGR("BiogeoRegions_selezione.shp")

# Converto lo .shp file in dataframe
biogeo_habitat.df <- as(biogeo_habitat, "data.frame")

# Sostituisco valori della tabella in 'pa'
presence_absence <- decostand(biogeo_habitat.df[,9:ncol(biogeo_habitat.df)], "pa")
# Sono quasi tutti con valore 0 (probabilmente perchè ho selezionato una regione piccola?) 

# Habitat richness
sum <- rowSums(presence_absence)

# Aggiungo la colonna 'habitat_richness' al dataframe 
df <- cbind(biogeo_habitat.df, habitat_richness)
str(df)
head(df)

