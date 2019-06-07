#example of scrDesign

library(sf)
library(oSCR)
library(rgdal)
library(lwgeom)
library(raster)
library(mapview)
###library(scrDesign)

#hypothestical study area (White mountatin national forest)
wmnf <- st_read("data/wmnf_boundary.shp")

#make a statespagrid
wmnf.ss <- st_sample(wmnf, type = "regular", size = 0.1 * (st_area(wmnf)/1000^2))
wmnf.traps <- st_sample(wmnf, type = "regular", size = 0.5*(st_area(wmnf)/1000^2))
mapview(wmnf) +
  mapview(wmnf.traps, cex=0.1, color=NA) +
  mapview(wmnf.ss, cex=0.5)


# parameters
ntraps <- 50


#get the optimal design:
design50 <- SCRdesign(statespace = st_coordinates(wmnf.ss)/1000,
                      all.traps = st_coordinates(wmnf.traps)/1000,
                      ntraps = ntraps, nn = 30, ndesigns = 3,
                      sigma = 0.75, beta0 = 0.05, crit = 6)

opt.traps1 <- st_as_sf(data.frame(design50$Xlst[[1]]*1000,
                       coords = c("X","Y"), crs = crs(wmnf)))
opt.traps2 <- st_as_sf(data.frame(design50$Xlst[[2]]*1000,
                       coords = c("X","Y"), crs = crs(wmnf)))
opt.traps3 <- st_as_sf(data.frame(design50$Xlst[[3]]*1000),
                       coords = c("X","Y"), crs = crs(wmnf))

mapview(wmnf) +
  mapview(wmnf.traps, cex=0.1, color=NA) +
  mapview(wmnf.ss, cex=0.5) + 
  mapview(opt.traps1, col.region="red") +
  mapview(opt.traps2, col.region="blue") +
  mapview(opt.traps3, col.region="green")
