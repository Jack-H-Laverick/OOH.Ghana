
# Create a random forest model to predict NGU sediment classes from bathymetry and shear stress

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "h2o", "tictoc")                # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages

h2o.init(max_mem_size = "30G")                                   # Start running h2o in the background

Data <- readRDS("../MiMeMo/Sediment/Objects/RF_sediment_observations.rds") %>% # Read in data
   drop_na() %>%                                                 # Drop point without all estimates
   mutate(Sed_class = as.factor(Sed_class)) 

#ggplot(Data) +                                                  # check data by plotting
#  geom_sf(aes(colour = Stress95), size = 0.1)

#### Split into training and validation ####

Training <- Data %>% 
  st_drop_geometry %>% 
  group_by(Sed_class) %>% 
  sample_frac(0.7) %>% 
  ungroup %>% 
  dplyr::select(-c(Region))

Validation <- Data %>% 
  st_drop_geometry %>% 
  dplyr::select(-c(Region)) %>%  
  anti_join(Training)

rm(Data)

#### Build model ####

tic()
RF <- h2o.randomForest(y = "Sed_class",                          # Predict Sediments
                       training_frame = as.h2o(Training),                  
                       validation_frame = as.h2o(Validation),
                       ntrees = 500,
                       seed = 5678)                              # Set seed to ensure reproducibility
toc()

h2o.saveModel(RF, "./Objects/Sediment modelh2o")

h2o.shutdown(prompt = FALSE)                                     # Close h2o, otherwise you'll hit memory limits later
