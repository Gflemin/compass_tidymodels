### Libraries

# load all required libraries with pacman 
pacman::p_load(renv, drake, tidyverse, tidymodels, lubridate, xgboost, ranger, rpart, iml, janitor, GGally, paradox)

# # initialize renv to copy packages into renv cache 
# renv::init()

# # take a renv snapshot
# renv::snapshot()

# restore our library configuration
renv::restore()
