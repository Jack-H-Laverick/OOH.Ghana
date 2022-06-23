
# Create a random forest model to predict NGU sediment classes from bathymetry and shear stress

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "raster", "h2o")                   # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages

source("./R scripts/@_Region file.R")

#### Points to predict for ####
 
bath <- read.csv("./Data/Ghana_MOHID_bathymetry.csv") %>% 
  mutate(latitude = round(latitude, 3), longitude = round(longitude, 3))

bath2 <- bath[, c(3,2,1)] %>% 
  rasterFromXYZ(digits = 3, crs = st_crs(4326))

crs(bath2) <- CRS('+init=epsg:4326')

plot(bath2)

vars <- terrain(bath2,  unit = "degrees", opt = c("slope", "TPI", "TRI", "roughness")) %>% 
  as.data.frame(xy = TRUE) %>% 
  mutate(x = round(x, 3), y = round(y, 3)) %>% 
  left_join(rename(bath, x = longitude, y = latitude))

stress <- read.csv("./Data/Matt/ShearStress95centile_MOHID.csv") 

stress2 <- stress[, c(2,1,3)] %>% 
  rasterFromXYZ(digits = 3, crs = sf::st_crs(4326))

plot(stress2)

stress2 <- as.data.frame(stress2, xy = TRUE) %>% 
  mutate(x = round(x, 3), y = round(y, 3)) %>% 
  left_join(vars)

predict <- dplyr::select(stress2, tri, tpi, roughness, slope, Depth = waterdepth, Stress95 = shearstress95centile, x, y) %>% 
  drop_na(-c(x, y))

#### Load model ####

h2o.init(max_mem_size = "30G")                                   # Start running h2o in the background

saved_model <- h2o.loadModel("Objects/Sediment modelh2o/DRF_model_R_1655906108017_1")

#### Predict at unknown locations ####

tic()
prediction <- h2o.predict(saved_model, as.h2o(predict %>% dplyr::select(-c(x,y)))) %>% # Predict in h2o
  as.data.frame() %>%                                             # Copy results into R
  dplyr::select(Sed_class = predict) %>% 
  bind_cols(dplyr::select(predict, x,y))                          # Add coordinates
toc()

saveRDS(prediction, "./Objects/MiMeMo sediments.rds")             # Save to convert NGU classess to 8 StrathE2E habitat types

h2o.shutdown(prompt = FALSE)                                      # Close h2o, otherwise you'll hit memory limits later

#### Import NGU category translations ####

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%   # Import quantitative values for classes
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  rowwise() %>% 
  mutate(D50 = 10^weighted.mean(c(log10(11.31371), log10(0.3535534), log10(0.007826238)), 
                                c(Gravel,Sand,Silt))) %>%         # Weighted mean of the central value from grain size classes
  ungroup()

#### Transform categorical to continuous ####

Sed_quant <- left_join(prediction, Translate) %>% 
  mutate(Sed_class = as.factor(Sed_class))  

ggplot() + 
  geom_raster(data = Sed_quant, aes(x = x, y = y, fill = D50)) + 
  viridis::scale_fill_viridis(name = 'mean grain size (NGU)', trans = "log") +
  theme_minimal() +
  NULL

saveRDS(Sed_quant, "./Objects/Full sediment.rds")






