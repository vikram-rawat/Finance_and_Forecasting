# load libraries: ----------------------------------
source("dependencies.R")

# start: ----------------------------------

layer <- nn_linear(10, 2)

opt <- optim_sgd(layer$parameters, lr = 1e-1)
