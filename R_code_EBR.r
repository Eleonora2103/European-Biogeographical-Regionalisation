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
biogeo_region <- readOGR('BiogeoRegions2016.shp')

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

#####################################################################

# Calcolo la distanza dal margine più vicino
# Utilizzo lo Shapefile filtrato (Selezione.shp) e quello delle regioni Biogeografiche (BiogeoRegions2016.shp)

# transformo file 'sf' 
sf <- st_as_sf(biogeo_region)
sf_habitat <- st_as_sf(biogeo_habitat)
sf_habitat

# estraggo solo i punti all'interno del limite della regione selezionata
grid <- st_intersection(sf, sf_habitat)   
plot(grid, max.plot = 1)

# trasformo il layer habitat da poligono a linea
border <- st_cast(sf, "MULTILINESTRING")
class(border)

# calcolo la distanza tra il confine ed i punti della griglia selezionata
dist <- st_distance(border, grid)

# distanza calcolata in metri
head(dist)
# Units: [m]
# [1]   75991.24 1787175.29 2654651.43  514593.71 1454667.45 1351966.73

# creo un dataframe con la distanza e le coordinate dei punti
df <- data.frame(dist = as.vector(dist)/1000,
                 st_coordinates(border))
# Error in data.frame(dist = as.vector(dist)/1000, st_coordinates(border)) : 
# arguments imply differing number of rows: 7428, 3021658

# color
col_dist <- brewer.pal(11, "RdGy")

ggplot(df, aes(x=dist, y=nrow(dist), fill = dist))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (km)")+ #legend name
  theme_void()+ #map theme
  theme(legend.position = "bottom") #legend position
