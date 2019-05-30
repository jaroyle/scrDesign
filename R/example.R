#example of scrDesign

library(sf)
library(oSCR)
library(rgdal)
library(lwgeom)
library(raster)
library(mapview)
library(scrDesign)

#hypothestical study area (White mountatin national forest)
wmnf <- st_read("data/wmnf_boundary.shp")

#make a statespagrid
wmnf.ss <- st_sample(wmnf, type = "regular", size = 0.25 * (st_area(wmnf)/1000^2))
wmnf.traps <- st_sample(wmnf, type = "regular", size = (st_area(wmnf)/1000^2))
mapview(wmnf) + 
  mapview(wmnf.traps, cex=0.1, color=NA) +
  mapview(wmnf.ss, cex=0.5)
  

#get the optimal design:
design50 <- SCRdesign(statespace = st_coordinates(wmnf.ss)/1000, 
                      all.traps = st_coordinates(wmnf.traps)/1000, 
                      ntraps = 50, nn = 30, ndesigns = 50, 
                      sigma = 0.75, beta0 = 0.05, crit = 6)

