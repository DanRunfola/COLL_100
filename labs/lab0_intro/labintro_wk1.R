#install ggmap and other packages

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
#install.packages(x, dependencies=TRUE) # warning: this may take a number of minutes 
lapply(x, library, character.only = TRUE) # load the required packages




#use leaflet::leaflet to make a quick interactive map

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

#CHALLENGE: create a map of your hometown



#use ggmap::qmap to make a few quick & pretty maps

library(ggmap)

gt <- 'Georgia Tech'
qmap(gt, zoom = 14)
qmap(gt, zoom = 14, source = "osm")
qmap(gt, zoom = 14, source = "stamen", maptype = "watercolor", crop = FALSE)
qmap(gt, zoom = 14, source = "stamen", maptype = "toner", crop = FALSE)

#CHALLENGE: create a map of your favorite school or University



#use ggmap::get_map to make a map of a city somewhere in the world

santos <- get_map(location = "Santos", zoom = 12)

ggmap(santos) + labs(x = 'Longitude', y = 'Latitude') + ggtitle('The Home of Pele')

#CHALLENGE: create a map of a city or place somewhere in the world you would like to visit one day



#import a shapefile, create a spatial object and plot some descriptive statistics

install.packages("rgdal", dependencies=TRUE)

library(rgdal)

setwd("/Users/tyfrazier/workspace/work_life/WM/Teaching/COLL_100/labs/lab1_basics")

acr <- readOGR(dsn = "data", layer = "accra")

head(acr@data, n = 20)

mean(acr$AREA)

mean(acr$N_HH_POP)

mean(acr$SLUM_INDEX)

# calculate some statistics and plot them

acr@data[acr$SLUM_INDEX > 3.45, ]

plot(acr, col = "lightgrey")sel <- acr$SLUM_INDEX > 2.75plot(acr[ sel, ], col = "turquoise", add = TRUE)


#CHALLENGE: import a shapefile of an interesting place with at least 3 continuous variables and calculate some basic descriptive statistics (for example the mean or the density) and create a plot where one of the variables has been discretized into at least 2 intervals  


# create some spatial maps using tmap
install.packages("rgeos", dependencies=TRUE)


install.packages("tmap", dependencies=TRUE)

library(tmap)

qtm(shp = acr, fill = "SLUM_INDEX", fill.palette = "-Blues")

qtm(shp = acr, fill = c("SLUM_INDEX", "N_HH_POP"), fill.palette = c("Blues"), ncol = 2)




# create some spatial maps using ggmap


p <- ggplot(acr@data, aes(SLUM_INDEX, N_HH_POP))

p + geom_point(colour = "blue", size=1)

p + geom_point(aes(colour=SLUM_INDEX, size=N_HH_POP))

p + geom_point(aes(colour = SLUM_INDEX, size = N_HH_POP)) + geom_text(size = 1.25, aes(label = EA))




accra <- qtm(shp = acr, fill = "SLUM_INDEX", fill.palette = "-Blues")


png('accra.png')
accra
dev.off()
