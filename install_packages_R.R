# Script: Supporting script to install packages for Deep Learning ------------------------------------------------------------
# Author: Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2021-09-14
# R version 4.1.0 (2021-05-18)
#

# clean environment
rm(list = ls()); gc()

# pacman library is used to load or install the rest of packages
## if you dont have this, please use the install command, otherwise just load
# install.packages("pacman")
library(pacman) 

# now we load or install all the rest of packages automatically
p_load(raster,
       rgdal,
       rgeos,
       gdalUtils,
       sp,
       leaflet,
       mapview,
       magick,
       sf,
       parallel, doParallel, foreach,
       caret
)

# libraries needed for deep learning to run in the local PC
## this is ONLY if you have a GPU and want to run DL in your own computer
## Guide to install the GPU/CUDA etc here: https://doi.org/10.5281/zenodo.3929709
# always put reticulate and use_python as the first packages to load, or you will not be able to choose the conda env/python
p_load(reticulate)
p_load(keras, tfdatasets, tidyverse, rsample, magick)
