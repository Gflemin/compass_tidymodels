
### Make sure the here and pacman libraries are installed before running this script. Once they are, run this script and everything should
### be produced for you! If you want to change which observation is used in the local interpretability methods, just go to the drake_plan.R
### script and change the observation = <number> argument in line 20 (local plots = imler_local...)

# here
library(here)

# pacman
library(pacman)

# source scripts
source(here::here("libraries.R"))
source(here::here("functions.R"))

# load up our drake plan
source(here::here("drake_plan.R"))

# run our drake plan and build each of our individual targets 
make(plan)

# visualize our drake plan as a network graph to show how the plan targets (functions from functions.R) relate to each other 
# vis_drake_graph(config)

# use drake::loadd to load the calculated objects into our environment and view them
loadd(permutation_plots) # rerun after changing the drake plan
# loadd(global_plots) # rerun after changing the drake plan
# loadd(local_plots) # rerun after changing the drake plan
permutation_plots
# global_plots
# local_plots

