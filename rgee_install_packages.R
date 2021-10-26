# Script: Script with installation procedures to use the RGEE ------------------------------------------------------------
# Author: Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2021-10-26
# R version 4.1.1 (2021-08-10)
#

# clean environment
rm(list = ls()); gc()

# general libraries
install.packages("pacman")
library(pacman)



# GEE account -------------------------------------------------------------

## you need a GEE account
## log in the https://code.earthengine.google.com/ and register for one



# installing conda environment --------------------------------------------------------------------

## the conda environment is where the GEE Python API will be located. The RGEE package uses it.
## first you need to install the Miniconda OUTSIDE of R
## install Miniconda3 at https://docs.conda.io/en/latest/miniconda.html
## open 'anaconda' in the command prompt (window button --> anaconda, you will see anaconda prompt)
## then type in the commands below one-by-one (without the #) to install the rgee_py environment and packages:
# conda create -n rgee_py python=3.9
# activate rgee_py
# pip install google-api-python-client
# pip install earthengine-api
# pip install numpy

## ok conda should now be installed, now lets get the path to the environment, type inside anaconda:
# conda env list

## copy the path to the rgee_py environment, you will need it set in the variable below inside R:
## note the use of double backslashes \\ 
## this below is where is located in MY computer, you have to use the 'conda env list' command to find where it is located on yours
rgee_environment_dir = "C:\\ProgramData\\Miniconda3\\envs\\rgee_py\\"



# pre-requirements for R --------------------------------------------------

## R: version at least 3.6 (this is the version that I tested so far and works)
# Link: https://cran.r-project.org/bin/windows/base/

## RStudio: a recent version is recommended.
## Older versions do not show the GEE images in Viewer correctly.
# Link: https://www.rstudio.com/products/rstudio/download/

## RTools: needed to build some packages in R from source
# Link: https://cran.r-project.org/bin/windows/Rtools/
## after installing the Rtools, make sure to run this command line below inside RStudio:
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")



# R packages -------------------------------------------------------------

## if you installed everything above, you can now install the packages inside R

# install general packages used in the scripts
p_load(raster,
       rgdal,
       rgeos,
       gdalUtils,
       sp,
       sf,
       leaflet,
       mapview,
       caret)

# restart and run this and it worked
p_load(rgee, geojsonio, remotes, reticulate, devtools)
install_github("r-spatial/rgee")

## sometimes at this point you are required to restart R or the computer before proceeding
## try restarting if the installation do not finish properly and run the installation again after restar

# set python
reticulate::use_python(rgee_environment_dir, required=T)
rgee::ee_install_set_pyenv(
  py_path = rgee_environment_dir, # Change it for your own Python PATH
  py_env = "rgee_py" # Change it for your own Python ENV
)
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)

# Initialize the Python Environment
# to clean credentials: ee_clean_credentials()
rgee::ee_Initialize(drive = T)

## It worked if some text about google drive credentials appeared, and asked you to log in your GEE account.
## Congrats.

