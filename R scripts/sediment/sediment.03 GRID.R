
library(tidyverse)
library(raster)
library(sf)

data <- read.csv("./Data/Ghana_MOHID_bathymetry.csv")

data2 <- data[, c(3,2,1)] %>% 
  rasterFromXYZ(digits = 3, crs = st_crs(4326))

crs(data2) <- 4326

plot(data)

vars <- terrain(data2,  unit = "degrees", opt = c("slope", "TPI", "TRI", "roughness"))
