#revised using ggmap

#install.packages("ggmap") but make sure all dependencies are installed, better to use window

library(ggmap)

qmap(location = "georgia tech")  
qmap(location = "georgia tech", zoom = 14)  
qmap(location = "georgia tech", zoom = 14, source = "osm") 

# Get a Census API Key: http://api.census.gov/data/key_signup.html


setwd("/Users/tyfrazier/workspace/work_life/WM/Teaching/COLL_100/labs/lab1_basics")


# Some basic mapping using the Choropleth Package

library(choroplethr)
data(df_pop_state)

?df_pop_state
head(df_pop_state)

df_pop_state[df_pop_state$region == "florida", ]

state_choropleth(df_pop_state)
state_choropleth(df_pop_state, num_colors = 2)
state_choropleth(df_pop_state, num_colors = 1)

data(df_state_demographics)
?df_state_demographics

colnames(df_state_demographics)

df_state_demographics$value <- df_state_demographics$percent_hispanic

state_choropleth(df_state_demographics)

state_choropleth(df_state_demographics, num_colors=1)

#Map some counties

data(df_pop_county)
head(df_pop_county)

county_choropleth(df_pop_county)

county_choropleth(df_pop_county, state_zoom="north carolina")

#county demographics

data(df_county_demographics)
colnames(df_county_demographics)

df_county_demographics$value <- df_county_demographics$percent_black

county_choropleth(df_county_demographics, num_colors=1, state_zoom=c("virginia","north carolina", "south carolina","florida"))

df_county_demographics$value <- df_county_demographics$percent_hispanic




# Install Package USCensus2010 with dependencies

library(UScensus2010)

# use the helper functions in UScensus2010 to install county, tract, blkgrp and blk datasets

install.county("osx")
install.tract("osx")
install.blkgrp("osx")
#install.cdp("osx")
#install.blk("osx")

# move source packages

# install local packages from source

# load package
library(UScensus2010county)
library(UScensus2010tract)
library(UScensus2010blkgrp)
#library(UScensus2010cdp)
#library(UScensus2010blk)

# load all counties for one state
data(virginia.county10)
data(virginia.tract10)
data(virginia.blkgrp10)
#data(virginia.cdp10)
#data(virginia.blk10)

#map virginia


choropleth(virginia.county10, border = "transparent")
choropleth(virginia.tract10, border = "transparent")
choropleth(virginia.blkgrp10, border = "transparent")
#choropleth(virginia.cdp10, border = "transparent")
#choropleth(virginia.blk10, border = "transparent")


#look at the variables

names(virginia.blkgrp10)
help(virginia.tract10)

#calculate densities using the areaPoly function

png('virginia.png')

par(mfrow=c(3,1),mar=c(3,1.5,1.5,3))

den_co <- virginia.county10$P0010001 / areaPoly(virginia.county10)
virginia.county10$den_co <- den_co
choropleth(virginia.county10,"den_co",color=list(fun="heat.colors", attr = list(4)), main="County Densities", border = "transparent", legend = FALSE, sub = NA)

den_tr <- virginia.tract10$P0010001 / areaPoly(virginia.tract10)
virginia.tract10$den_tr <- den_tr
choropleth(virginia.tract10,"den_tr",color=list(fun="heat.colors", attr = list(4)), main="Tract Densities", border = "transparent", legend = FALSE, sub = NA)

den_bgrp <- virginia.blkgrp10$P0010001 / areaPoly(virginia.blkgrp10)
virginia.blkgrp10$den_bgrp <- den_bgrp
choropleth(virginia.blkgrp10,"den_bgrp",color=list(fun="heat.colors", attr = list(4)), main="Block Group Densities", border = "transparent", legend = FALSE, sub = NA)

dev.off()


#what about 8 bins, how will they look side by side, why is this? what does this have to do with density? the number of areal units?

# par(mfrow=c(3,2))


#calculate summary statistics for a variable

par(mfrow=c(2,2))

choropleth(virginia.county10,"P0030002", color=list(fun="heat.colors", attr = list(8)), main="White Alone", border = "transparent", legend = FALSE)

choropleth(virginia.county10,"P0030003",color=list(fun="heat.colors", attr = list(8)), main="Black Alone", border = "transparent", legend = FALSE)

choropleth(virginia.county10,"P0060002",color=list(fun="heat.colors", attr = list(8)), main="White AIC", border = "transparent", legend = FALSE)

choropleth(virginia.county10,"P0060003",color=list(fun="heat.colors", attr = list(8)), main="Black AIC", border = "transparent", legend = FALSE)

help(virginia.county10)

demographics(dem = c("P0030002","P0060002", "P0030003", "P0060003"), state = "va", level = "county") 


#what about at higher disaggregation

nomansland <- county(name = c("alleghany","covington","bath","highland"), state = "va", level = "tract")

par(mfrow=c(2,2))

choropleth(nomansland,"P0030002", color=list(fun="heat.colors", attr = list(8)), main="White Alone", border = "transparent", legend = FALSE)
choropleth(nomansland,"P0030003",color=list(fun="heat.colors", attr = list(8)), main="Black Alone", border = "transparent", legend = FALSE)
choropleth(nomansland,"P0060002", color=list(fun="heat.colors", attr = list(8)), main="White AIC", border = "transparent", legend = FALSE)
choropleth(nomansland,"P0060003",color=list(fun="heat.colors", attr = list(8)), main="Black AIC", border = "transparent", legend = FALSE)


#USCensus package has data but functions are sometimes lacking

nml_race_tracts <- as.data.frame(demographics(dem = "P0030002", state = "va", level = "tract"))
nml_race_tracts$P0060002 <- as.data.frame(demographics(dem = "P0060002", state = "va", level = "tract"))[,1]
nml_race_tracts$P0030003 <- as.data.frame(demographics(dem = "P0030003", state = "va", level = "tract"))[,1]
nml_race_tracts$P0060003 <- as.data.frame(demographics(dem = "P0060003", state = "va", level = "tract"))[,1]

nml_race_tracts$match <- substr(rownames(nml_race_tracts),1,5)

nml_race_tracts[which(nml_race_tracts$match == "51005" | nml_race_tracts$match == "51580" | nml_race_tracts$match == "51017" | nml_race_tracts$match == "51091"),]




#what about even higher disaggregation, lets zoom in some

james_inlet <- county(name = c("suffolk", "portsmouth", "chesapeake", "norfolk", "virginia beach"), state = "va", level = "blkgrp")

par(mfrow=c(2,2))

choropleth(james_inlet, "P0030002",color=list(fun="heat.colors", attr = list(8)), main="White Alone", border = "transparent", legend = FALSE)
choropleth(james_inlet, "P0030003",color=list(fun="heat.colors", attr = list(8)), main="Black Alone", border = "transparent", legend = FALSE)
choropleth(james_inlet, "P0060002",color=list(fun="heat.colors", attr = list(8)), main="White AIC", border = "transparent", legend = FALSE)
choropleth(james_inlet, "P0060003",color=list(fun="heat.colors", attr = list(8)), main="Black AIC", border = "transparent", legend = FALSE)

ji_race_blkgrp <- as.data.frame(demographics(dem = "P0010001", state = "va", level = "blkgrp"))
ji_race_blkgrp$P0030002 <- as.data.frame(demographics(dem = "P0030002", state = "va", level = "blkgrp"))[,1]
ji_race_blkgrp$P0060002 <- as.data.frame(demographics(dem = "P0060002", state = "va", level = "blkgrp"))[,1]
ji_race_blkgrp$P0030003 <- as.data.frame(demographics(dem = "P0030003", state = "va", level = "blkgrp"))[,1]
ji_race_blkgrp$P0060003 <- as.data.frame(demographics(dem = "P0060003", state = "va", level = "blkgrp"))[,1]

ji_race_blkgrp$match <- substr(rownames(ji_race_blkgrp),1,5)

ji_race_blkgrp[which(ji_race_blkgrp$match == "51800" | ji_race_blkgrp$match == "51740" | ji_race_blkgrp$match == "51550" | ji_race_blkgrp$match == "51710" | ji_race_blkgrp$match == "51810"),]



# ACS Package

library(acs)

