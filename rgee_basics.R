# Script: Basics of RGEE ------------------------------------------------------------
# Author: Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2021-10-26
# R version 4.1.1 (2021-08-10)
#

# clean environment
rm(list = ls()); gc()

# general libraries
library(pacman) # install.packages("pacman")
p_load(raster,
    rgdal,
    rgeos,
    gdalUtils,
    sp,
    plyr, tidyr
)

# set directory for your conda environment
## Change this to your environment directory as it was installed in the rgee_install_packages 
## if you forgot environment name, open anaconda prompt in windows, type in: conda env list
rgee_environment_dir = "C:\\ProgramData\\Miniconda3\\envs\\rgee_py\\"



# initiate RGEE --------------------------------------------------------------------

## if everything was installed following the rgee_install_packages file, you should be good to start using this !

# load general packages used in the scripts
p_load(raster,
       rgdal,
       rgeos,
       gdalUtils,
       sp,
       sf,
       leaflet,
       mapview,
       caret)

# now some more specific packages related to using the rgee
p_load(rgee, geojsonio, remotes, reticulate, devtools, googledrive)
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



# Learning Objectives ----------------------------------------------------------------

## At the end of this lesson, I expect you to be able to:
# Explain and use basic operations in Google Earth Engine
# Filter and visualize geospatial data using RGEE
# Acquire/download geospatial data using RGEE
# Apply a machine learning method using the data we downloaded




# What is RGEE and why do I care ? ----------------------------------------------------------

# rgee: An R package for interacting with Google Earth Engine
# Cesar Aybar and Quisheng Wu and Lesly Bautista and Roy Yali and Antony Barja
# Official website: https://github.com/r-spatial/rgee
# 250+ RGEE Examples: https://csaybar.github.io/rgee-examples/

# Use of R language instead of javascript
# Use RStudio Interface - which is nice :)
# Dynamic codes and for-loops, e.g. change date of acquisition dynamically
# Visualize/plot data from GEE inside R and overlay with other datasets without uploading them to GEE
# Making a pipeline from data acquisition (GEE), download (Google Drive), and processing (locally)
#   and using it to run Algorithms that are NOT present in GEE e.g. Deep Learning :)



# GEE Constructors --------------------------------------------------------

# ee$Image - one image
# ee$ImageCollection - several images 
# ee$Feature - one vector file
# ee$Feature.Collection - several vector files 




# Difference between GEE and RGEE -----------------------------------------

# . becomes $
# : becomes =
# Parameters do not need to be written between 'quotes'
# Don't need to state 'var' before objects/variables
# Good help documentation
# You don't have the Console to easily check some variables
# ... but you can use R code to make for loops and other nice things!




# Where to find data? -----------------------------------------------------

# Search at https://developers.google.com/earth-engine/datasets/ 
# Community datasets at https://github.com/samapriya/awesome-gee-community-datasets




# Image: SRTM elevation -------------------------------------------

# load world's elevation from SRTM
srtm = ee$Image("USGS/SRTMGL1_003")

# get/check the metadata
ee_print(srtm)

# for visualization we use Map$addLayer
Map$addLayer(eeObject = srtm, name = 'srtm')

# improving the visualization with some user-specified visualization parameters
vis_srtm = list(
  bands = "elevation",
  min = 0,
  max = 500
)
Map$addLayer(eeObject = srtm, visParams = vis_srtm, name = 'srtm')

# centering the plot using Map$setCenter
Map$setCenter(lon = -48, lat = -16, zoom = 4)
Map$addLayer(eeObject = srtm, visParams = vis_srtm, name = 'srtm')

# calculate slope of elevation
slope = ee$Terrain$slope(srtm);
ee_print(slope)
Map$addLayer(eeObject = slope, visParams = list(min = 0, max = 1), name = 'slope')




# Image: MapBiomas land cover map ---------------------------------

# mapbiomas dataset
land_cover = ee$Image("projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1")
ee_print(land_cover)
# take a look at the different bands

# load year 2020 land cover classification 
land_cover_2020 = ee$Image("projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1")$
  select("classification_2020")
ee_print(land_cover_2020)

# color palette
# official: https://mapbiomas-br-site.s3.amazonaws.com/downloads/Colecction%206/Cod_Class_legenda_Col6_MapBiomas_BR.pdf
idx_mapbiomas = c(1, 3, 4, 5, 49, 10, 11, 12, 32, 29, 13, 14, 15, 18, 19, 39, 20, 40, 41,  36,  46,  47,  48,  9, 
                  21,  22,  23,  24,  30,  25,  26,  33,  31,  27) 
hex_mapbiomas = c("#129912", "#006400", "#00ff00", "#687537", "#6b9932", "#BBFCAC", "#45C2A5", "#B8AF4F", "#968c46",
                  "#665a3a", "#f1c232", "#FFFFB2", "#FFD966", "#E974ED", "#D5A6BD", "#e075ad", "#C27BA0", "#982c9e", "#e787f8", "#f3b4f1",
                  "#cca0d4", "#d082de", "#cd49e4", "#ad4413", "#fff3bf", "#EA9999", "#DD7E6B", "#aa0000", "#af2a2a", "#ff3d3d", "#0000FF",
                  "#0000FF", "#02106f", "#D5D5E5")
map_biomas_palette = rep("#FFFFFF", 49)
map_biomas_palette[idx_mapbiomas] = hex_mapbiomas
vis_mapbiomas = list(
  min = 1,
  max = 49,
  palette = map_biomas_palette
)
Map$addLayer(eeObject = land_cover_2020, visParams = vis_mapbiomas, 'mapbiomas_2020')

## You can then plot your own shapefiles on top of this view to visualize land cover

# now lets get the 1985 map
land_cover_1985 = ee$Image("projects/mapbiomas-workspace/public/collection6/mapbiomas_collection60_integration_v1")$
  select("classification_1985")
ee_print(land_cover_1985)

# and plot both 2010 and 2020 to see the changes
Map$addLayer(eeObject = land_cover_1985, visParams = vis_mapbiomas, 'mapbiomas_1985') +
  Map$addLayer(eeObject = land_cover_2020, visParams = vis_mapbiomas, 'mapbiomas_2020')




# ImageCollection: TRMM Precipitation data example -----------------------

# load some shapefile from USA
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
mapview(nc)

# first lets take a look at the product metadata
trmm = ee$ImageCollection("TRMM/3B43V7")
ee_print(trmm)

# get TRMM precipitation data 
# obs: note the pipes %>% to connect the commands
trmm = ee$ImageCollection("TRMM/3B43V7") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>% 
  ee$ImageCollection$map(function(x) x$select("precipitation")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("%02d",1:12)) # rename the bands of an image

# extract data 
ee_nc_rain <- ee_extract(x = trmm, y = nc["NAME"], sf = FALSE)

# plot
ee_nc_rain %>%
  pivot_longer(-NAME, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("X", "", month)) %>% 
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()



# ImageCollection: Sentinel-2 data --------------------------------------------------------

# To apply an algorithm such as a cloud mask per Image inside a ImageCollection...
# We use the map() function, which is a for-loop within the ImageCollection

# cloud mask function for Sentinel-2
maskS2clouds = function(image) {
  qa = image$select('QA60');
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask = bitwShiftL(1,10)
  cirrusBitMask = bitwShiftL(1, 11)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask_data = qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0));
  
  return(image$updateMask(mask_data)$divide(10000))
}

# Sentinel-2 data
sentinel2_data = ee$ImageCollection('COPERNICUS/S2_SR')$
  # Select the date of year
  filterDate('2019-11-01', '2020-01-01')$ 
  # Pre-filter to get less cloudy granules.
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
  # mask clouds
  map(maskS2clouds) 
ee_print(sentinel2_data)
# Take a look at number of images, bands, etc.


# Let's look at one specific area - Lagoa dos Patos - Rio Grande do Sul
ROI = ee$Geometry$Polygon(
  list(
    c(-51.58690536273452, -30.001166605573065),
    c(-51.58690536273452, -30.70271754544675),
    c(-50.46629989398452, -30.70271754544675),
    c(-50.46629989398452, -30.001166605573065)
  )
)
Map$addLayer(eeObject = ROI, name= 'ROI')

# Create a Feature from the Geometry.
ROI_feature = ee$Feature(ROI)#, list('foo' = 42, 'bar' = 'tart'))
ROI_feature$getInfo()

# Sentinel-2 data
sentinel2_data = ee$ImageCollection('COPERNICUS/S2_SR')$
  # filter for the ROI area
  filterBounds(ROI_feature$geometry())$
  map(function(image){ image$clip(ROI_feature) })$
  # Select the date of year
  filterDate('2019-11-01', '2020-01-01')$ 
  # Pre-filter to get less cloudy granules.
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
  # mask clouds
  map(maskS2clouds) 
ee_print(sentinel2_data)
# How many images did we get?

# plot one of them
sentinel2_data_n = length(sentinel2_data$getInfo()$features)
sentinel2_data_single = ee$Image(sentinel2_data$toList(sentinel2_data_n)$get(10)) # 0 to n
ee_print(sentinel2_data_single)
Map$centerObject(ROI_feature, zoom = 9)
Map$addLayer(eeObject = sentinel2_data_single,
             visParams = list(
               bands = c("B4", "B3", "B2"),
               min = 0,
               max = 0.3
             ),
             name = 'RGB')
# Try changing the number inside get() to visualize different images from the period




# Composites ImageCollection: Sentinel-2 data ---------------------------------

# This is where I think GEE really shines ! Compositing images.
# e.g. When we have many clouds, we can use a 'Reducer' (GEE Terminology) to make a composite
# That composite encompass a period of time and create an artificial image
# To do that we use the median() function, it will calculate per-pixel median within the Collection

# Sentinel-2 data
sentinel2_data_median = ee$ImageCollection('COPERNICUS/S2_SR')$
  # filter for the ROI area
  filterBounds(ROI_feature$geometry())$
  map(function(image){ image$clip(ROI_feature) })$
  # Select the date of year
  filterDate('2019-11-01', '2020-01-01')$ 
  # Pre-filter to get less cloudy granules.
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
  # mask clouds
  map(maskS2clouds)$
  median()
ee_print(sentinel2_data_median)
# note that this is an Image now because of the median() function

# Visualize our composite covering the whole area, with no clouds :) 
Map$addLayer(eeObject = sentinel2_data_median,
             visParams = list(
               bands = c("B4", "B3", "B2"),
               min = 0,
               max = 0.3
             ),
             name = 'RGB')



# NDVI ImageCollection: Sentinel-2 data -----------------------------------

# To calculate a vegetation index such as NDVI we use the in-built function normalizedDifference()
# In Sentinel-2 the bands we want for NDVI are B8 and B4

# calculate ndvi
calc_ndvi = function (image) {
  ndvi = image$normalizedDifference(c("B8", "B4"))
  return(image$addBands(ndvi$rename('NDVI')))
}

# Sentinel-2 data
sentinel2_data_median = ee$ImageCollection('COPERNICUS/S2_SR')$
  # filter for the ROI area
  filterBounds(ROI_feature$geometry())$
  map(function(image){ image$clip(ROI_feature) })$
  # Select the date of year
  filterDate('2019-11-01', '2020-01-01')$ 
  # Pre-filter to get less cloudy granules.
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
  # mask clouds
  map(maskS2clouds)$
  map(calc_ndvi)$
  median()
ee_print(sentinel2_data_median)
# note the NDVI band that was added

# Visualize the NDVI from the composite
Map$addLayer(eeObject = sentinel2_data_median,
             visParams = list(
               bands = c("NDVI"),
               min = 0,
               max = 1
             ),
             name = 'NDVI')




# Exporting Sentinel-2 images to Google Drive and copying to local computer ----------------------------------------

# We can export the data from GEE to Google Drive, then get it to our computer
# All using a few lines of code

# download data to drive
# obs: note I used select("NDVI") to only get NDVI, so its faster
task  = ee_image_to_drive(
  image = sentinel2_data_median$select("NDVI"),
  maxPixels = 1e12,
  region = ROI_feature$geometry(),
  scale = 10,
  folder = "RGEE_data",
  crs = "EPSG:32721",
  description = 'sentinel2_data_JF_2020',
  fileNamePrefix = 'sentinel2_data_JF_2020',
  timePrefix = FALSE
)
task$start()

# monitor the GEE saving the data to the Google Drive
ee_monitoring(eeTaskList = TRUE)
# took ~8 mins here

# copy files from Google Drive to local computer
fname = ee_drive_to_local(task = task)
fname

# move file from temporary location to a new location in your computer
file.rename(fname,
            paste0("E:\\", basename(fname)))
## ok now we have the image in our computer and do anything we want with it e.g. use more R


## The end, hope you learned some RGEE today :)

