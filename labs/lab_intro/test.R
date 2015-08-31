install.packages("ggmap", dependencies=TRUE)
library(ggmap)























1 + 1


gt = 'Georgia Tech'
qmap(gt, zoom = 14)

qmap(gt, zoom = 13, source = "osm")
qmap(gt, zoom = 15, source = "stamen", maptype = 
       "watercolor", crop = FALSE)
qmap(gt, zoom = 16, source = "stamen", maptype = 
       "toner", crop = FALSE)

santos = get_map(location = "Santos", zoom = 12)
ggmap(santos)

ggmap(santos) + labs(x = 'Longitude', y = 'Latitude') + ggtitle('The Home of Pele')
