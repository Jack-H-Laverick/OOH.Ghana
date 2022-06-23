
library(tidyverse)
library(raster)

data <- read.csv("./Data/Matt/meanChlorophyll.csv") 

data2 <- data[, c(2,1,3)] %>% 
  rasterFromXYZ(digits = 3, crs = sf::st_crs(4326)) %>% 
  plot()

data <- read.csv("./Data/Matt/MeanMaxWaveOrbitalVelocity.csv") 

data2 <- data[, c(2,1,4)] %>% 
  rasterFromXYZ(digits = 3, crs = sf::st_crs(4326)) %>% 
  plot()

data <- read.csv("./Data/Matt/meanSalinity.csv") 

data2 <- data[, c(2,1,3)] %>% 
  rasterFromXYZ(digits = 2, crs = sf::st_crs(4326)) %>% 
  plot()

data <- read.csv("./Data/Matt/MOHID_varables.csv") 

data2 <- data[, c(1,2,9)] %>% 
  rasterFromXYZ(digits = 3, crs = sf::st_crs(4326)) %>% 
  plot()

data <- read.csv("./Data/Matt/ShearStress95centile_MOHID.csv") 

data2 <- data[, c(2,1,3)] %>% 
  rasterFromXYZ(digits = 3, crs = sf::st_crs(4326)) %>% 
  plot()
