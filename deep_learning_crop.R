# Script: Applying deep learning to satellite imagery to map rice crops in the local GPU ------------------------------------------------------------
# Author: Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2021-08-09
# R version 4.1.0 (2021-05-18)
#

## What this script does?
## i) Start with a raw satellite image
## ii) Overlay it with samples and crop the data into patches
## iii) Train a DL model, use the DL model to predict the class for all image
##      This step can be done in local machine with GPU or Google Colab
## iv) Combine the prediction (multiple patches) into a single mosaic
## v) Assess map accuracy quantitatively and qualitatively (visually)
## vi) Compare results with a previously produced Random Forests map


# clean environment
rm(list = ls()); gc()

# general libraries
library(pacman) # install.packages("pacman")
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

# directory to tensorflow python environment
## this is required if running DL in the local computer with GPU, otherwise you can run the DL in Google Colab
## this is only if you have a GPU and want to run in your own computer
## Guide to install here: https://doi.org/10.5281/zenodo.3929709
tensorflow_dir = "C:\\ProgramData\\Miniconda3\\envs\\r-tensorflow"

# set working directory
## here you need to set up your own directory containing the dataset
## https://zenodo.org/record/5504554/files/DL_Unet_CropExample_dataset.rar
setwd("D:\\2_Projects\\7_Presentation_Classes\\19_Minicourse_DeepLearning_Satellite\\")

# number of cores
no_cores = 7

# path to the gdal files and to the osgeo .bat (to run mosaic function)
## in case you don't have this installed, you need to download it from https://www.osgeo.org/projects/osgeo4w/
gdal_path = "C:\\OSGeo4W\\bin\\"
osgeo_path = "C:\\OSGeo4W"

# directory in the computer to save images
output_dir = "2_Images\\"
dir.create(output_dir, showWarnings = FALSE)



# functions ---------------------------------------------------------------

# function to extract data from a raster (x) from a SpatialPolygons object (y) faster than extract()
fast_extract = function(x,y) {
  value = list()
  for (i in 1:length(y)) {
    tmp = crop(x, y[i,])
    #value[[i]] = extract(tmp, y[i,])
    value[[i]] = as.numeric(na.omit(mask(tmp, y[i,])[]))
    rm(tmp)
  }
  return(value)
}

# function to extract data from a raster (x) from a SpatialPolygons object (y) faster than extract()
fast_extract_parallel = function(x,y) {
  
  # libraries needed
  require(parallel) # install.packages("parallel")
  require(doParallel) # install.packages("doParallel")
  require(foreach) # install.packages("foreach")
  
  # Begin cluster
  cl = parallel::makeCluster(detectCores()-1) # here you specify the number of processors you want to use, if you dont know you can use detectCores() and ideally use that number minus one
  #cl = parallel::makeCluster(3, outfile="D:/r_parallel_log.txt") # if you use this you can see prints in the txt
  registerDoParallel(cl)
  
  # apply the model in parallel
  # sometimes you need to specify in the package parameter (.packages) the name of package of the functions you are using
  value = foreach(i=1:length(y)) %dopar% { # note the %dopar% here
    require(raster)
    return(as.numeric(na.omit(mask(crop(x, y[i,]), y[i,])[])))
  }
  
  return(value)
}

# function to get the sp with the extent of a raster object
rasext_to_sp = function(x) {
  y = as(extent(x), "SpatialPolygons")
  crs(y) = crs(x)
  return(y)
}

# convert raster to vector using gdal_polygonize
# this version accept the python path and poligonizer path separetely
polygonizer_v2 <- function(x, outshape=NULL, pypath=NULL, polipath = NULL, readpoly=TRUE, 
                           fillholes=FALSE, aggregate=FALSE, 
                           quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL 
  # outshape: the path to the output shapefile (if NULL, a temporary file will 
  #           be created) 
  # pypath: the path to gdal_polygonize.py or OSGeo4W.bat (if NULL, the function 
  #         will attempt to determine the location)
  # readpoly: should the polygon shapefile be read back into R, and returned by
  #           this function? (logical) 
  # fillholes: should holes be deleted (i.e., their area added to the containing
  #            polygon)
  # aggregate: should polygons be aggregated by their associated raster value?
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly) || isTRUE(fillholes)) require(rgdal)
  
  #cmd <- Sys.which(paste0(pypath, '\\OSGeo4W.bat'))
  cmd = pypath
  if (is.null(pypath) | is.null(polipath)) {
    stop("Could not find gdal_polygonize.py or OSGeo4W on your system.")
  }
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  # system2(cmd, args=(
  #   sprintf('"%s" "%s" %s -f "ESRI Shapefile" "%s.shp"', 
  #           pypath, rastpath, ifelse(quietish, '-q ', ''), outshape)))
  system2(cmd, sprintf('"%s" "%s" %s -f "ESRI Shapefile" "%s.shp"', 
                       polipath, rastpath, ifelse(quietish, '-q ', ''), outshape))
  
  if(isTRUE(aggregate)||isTRUE(readpoly)||isTRUE(fillholes)) {
    shp <- readOGR(dirname(outshape), layer=basename(outshape), 
                   verbose=!quietish)    
  } else return(NULL)
  
  if (isTRUE(fillholes)) {
    poly_noholes <- lapply(shp@polygons, function(x) {
      Filter(function(p) p@ringDir==1, x@Polygons)[[1]]
    })
    pp <- SpatialPolygons(mapply(function(x, id) {
      list(Polygons(list(x), ID=id))
    }, poly_noholes, row.names(shp)), proj4string=CRS(proj4string(shp)))
    shp <- SpatialPolygonsDataFrame(pp, shp@data)
    if(isTRUE(aggregate)) shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  if(isTRUE(aggregate) & !isTRUE(fillholes)) {
    shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  ifelse(isTRUE(readpoly), return(shp), return(NULL))
}

# function from spatial.tools (does not work in R4.0.2 yet so we copied from previous version)
modify_raster_margins = function (x, extent_delta = c(0, 0, 0, 0), value = NA) 
{
  x_extents <- extent(x)
  res_x <- res(x)
  x_modified <- x
  if (any(extent_delta < 0)) {
    ul_mod <- extent_delta[c(1, 3)] * res_x
    ul_mod[ul_mod > 0] <- 0
    lr_mod <- extent_delta[c(2, 4)] * res_x
    lr_mod[lr_mod > 0] <- 0
    crop_extent <- c(x_extents@xmin, x_extents@xmax, x_extents@ymin, 
                     x_extents@ymax)
    crop_extent[c(1, 3)] <- crop_extent[c(1, 3)] - ul_mod
    crop_extent[c(2, 4)] <- crop_extent[c(2, 4)] + lr_mod
    x_modified <- crop(x_modified, crop_extent)
  }
  if (any(extent_delta > 0)) {
    ul_mod <- extent_delta[c(1, 3)] * res_x
    ul_mod[ul_mod < 0] <- 0
    lr_mod <- extent_delta[c(2, 4)] * res_x
    lr_mod[lr_mod < 0] <- 0
    extend_extent <- c(x_extents@xmin, x_extents@xmax, x_extents@ymin, 
                       x_extents@ymax)
    extend_extent[c(1, 3)] <- extend_extent[c(1, 3)] - ul_mod
    extend_extent[c(2, 4)] <- extend_extent[c(2, 4)] + lr_mod
    x_modified <- extend(x_modified, extend_extent, value = value)
  }
  return(x_modified)
}


# split the extent of a sp object
split_extent_gdal = function(x, block_size = 1000, na_rm = T, remove_all_zero = T, gdal_path = NULL) {
  if (is.null(gdal_path)) stop("Missing GDAL path.")
  # x = LIDAR_ANA_2017
  # x = LIDAR_ST1_2016
  x_ext = extent(x)
  
  # create a temporary raster within the extent with block_size as pixel size
  #n_x = ceiling(abs((x_ext[2] - x_ext[1])) / block_size)
  n_y = abs((x_ext[4] - x_ext[3])) / block_size
  # adjust extent to fit the cells
  x_ext_mod = x_ext
  x_ext_mod[4] = x_ext_mod[4] + ((ceiling(n_y) - n_y) * block_size)
  #
  r = raster(x_ext_mod, crs = crs(x), resolution = block_size)
  r[] = NA
  #
  #plot(extend(extent(r),100), asp=1)
  #plot(r, add=T, col="red")
  fname = paste0(tempfile(), ".tif")
  writeRaster(r, filename = fname, overwrite=T)
  
  if (inMemory(x)) {
    fname_x = paste0(tempfile(), "_x.tif")
    writeRaster(x, filename = fname_x)
    x = raster(fname_x)
  }
  
  # calculate the average of x inside the pixels
  #gdal_path = "C:\\GDAL_64\\"
  # command
  gdalwarp = paste(paste0(gdal_path,"gdalwarp")
                   #,"-r average -wm 9999"
                   ,"-r average -wm 2047"
                   ,x@file@name
                   ,fname
  )
  system(gdalwarp)
  
  # load
  r2 = raster(fname)
  #plot(r2)
  
  # convert raster to polygons - only those with values
  r2_pol = rasterToPolygons(r2, dissolve=F, na.rm=na_rm)
  # plot(r2_pol)
  
  # exclude all zero
  if (remove_all_zero) {
    idx = which(r2_pol@data[]==0)
    if (length(idx) > 0) r2_pol = r2_pol[-idx,]
  }
  
  # create extents
  ext_list = list()
  i=1
  for (i in 1:length(r2_pol)) {
    ext_list[[i]] = extent(r2_pol[i,])
  }
  
  unlink(fname)
  
  return(ext_list)
}
#

# split the extent of a sp object, border in meters
split_extent_gdal_border = function(x, block_size = 1000, na_rm = T, remove_all_zero = T, gdal_path = NULL, border = block_size*0.125) {
  if (is.null(gdal_path)) stop("Missing GDAL path.")
  # x = LIDAR_ANA_2017
  # x = LIDAR_ST1_2016
  x_ext = extent(x)
  
  # adjust border
  vect_overlap = c(-border, border, -border, border)
  x_ext = extent(x) - vect_overlap
  
  # create a temporary raster within the extent with block_size as pixel size
  #n_x = ceiling(abs((x_ext[2] - x_ext[1])) / block_size)
  n_y = abs((x_ext[4] - x_ext[3])) / block_size
  # adjust extent to fit the cells
  x_ext_mod = x_ext
  x_ext_mod[4] = x_ext_mod[4] + ((ceiling(n_y) - n_y) * block_size)
  #
  r = raster(x_ext_mod, crs = crs(x), resolution = block_size)
  r[] = NA
  #
  #plot(extend(extent(r),100), asp=1)
  #plot(r, add=T, col="red")
  fname = paste0(tempfile(), ".tif")
  writeRaster(r, filename = fname, overwrite=T)
  
  # we do this always now in order to always get only one layer for this purpose
  # otherwise, if the image has more than one band, the gdalwarp does not work
  #if (inMemory(x)) {
  fname_x = paste0(tempfile(), "_x.tif")
  writeRaster(x[[1]], filename = fname_x)
  x = raster(fname_x)
  #}
  
  # calculate the average of x inside the pixels
  #gdal_path = "C:\\GDAL_64\\"
  # command
  gdalwarp = paste(paste0(gdal_path,"gdalwarp")
                   #,"-r average -wm 9999"
                   ,"-r average -wm 2047"
                   ,x@file@name
                   ,fname
  )
  system(gdalwarp)
  
  # load
  r2 = raster(fname)
  #plot(r2)
  
  # convert raster to polygons - only those with values
  r2_pol = rasterToPolygons(r2, dissolve=F, na.rm=na_rm)
  # plot(r2_pol)
  
  # exclude all zero
  if (remove_all_zero) {
    idx = which(r2_pol@data[]==0)
    if (length(idx) > 0) r2_pol = r2_pol[-idx,]
  }
  
  # create extents
  ext_list = list()
  i=1
  for (i in 1:length(r2_pol)) {
    ext_list[[i]] = extent(r2_pol[i,])
  }
  
  unlink(fname)
  
  return(ext_list)
}
#

# split the extent of a sp object
split_extent_gdal_bottom = function(x, block_size = 1000, na_rm = T, remove_all_zero = T, gdal_path = NULL) {
  if (is.null(gdal_path)) stop("Missing GDAL path.")
  # x = LIDAR_ANA_2017
  # x = LIDAR_ST1_2016
  x_ext = extent(x)
  
  # create a temporary raster within the extent with block_size as pixel size
  #n_x = ceiling(abs((x_ext[2] - x_ext[1])) / block_size)
  n_y = abs((x_ext[4] - x_ext[3])) / block_size
  # adjust extent to fit the cells
  x_ext_mod = x_ext
  #x_ext_mod[4] = x_ext_mod[4] + ((ceiling(n_y) - n_y) * block_size)
  x_ext_mod[3] = x_ext_mod[3] - ((ceiling(n_y) - n_y) * block_size)
  #
  r = raster(x_ext_mod, crs = crs(x), resolution = block_size)
  r[] = NA
  #
  #plot(extend(extent(r),100), asp=1)
  #plot(r, add=T, col="red")
  fname = paste0(tempfile(), ".tif")
  writeRaster(r, filename = fname, overwrite=T)
  
  if (inMemory(x)) {
    fname_x = paste0(tempfile(), "_x.tif")
    writeRaster(x, filename = fname_x)
    x = raster(fname_x)
  }
  
  # calculate the average of x inside the pixels
  #gdal_path = "C:\\GDAL_64\\"
  # command
  gdalwarp = paste(paste0(gdal_path,"gdalwarp")
                   #,"-r average -wm 9999"
                   ,"-r average -wm 2047"
                   ,x@file@name
                   ,fname
  )
  system(gdalwarp)
  
  # load
  r2 = raster(fname)
  #plot(r2)
  
  # convert raster to polygons - only those with values
  r2_pol = rasterToPolygons(r2, dissolve=F, na.rm=na_rm)
  # plot(r2_pol)
  
  # exclude all zero
  if (remove_all_zero) {
    idx = which(r2_pol@data[]==0)
    if (length(idx) > 0) r2_pol = r2_pol[-idx,]
  }
  
  # create extents
  ext_list = list()
  i=1
  for (i in 1:length(r2_pol)) {
    ext_list[[i]] = extent(r2_pol[i,])
  }
  
  unlink(fname)
  
  return(ext_list)
}
#

# function to remove the last 4 digits of a string (usually the extension e.g. ".tif") and substitute it for another string
sub_extension = function (x, y) {
  return(paste0(substr(x, 1, nchar(x)-4), y))
}



# 1) load field data -----------------------------------------------------

# load field data
field_data = readOGR("1_Field\\ARROZ-RS_Safra_2019_2020\\RS_ARROZ_IRRIG_INUND_1920.shp")

# lets filter these data to only one municipality, list municipalities and get only one
unique(field_data$NM_MUNICIP)
field_data = field_data[field_data$NM_MUNICIP == "URUGUAIANA",]

# reproject to the same projection of the satellite data
field_data = spTransform(field_data, crs("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))

# visualization
if (FALSE) {
  # simple plot
  plot(field_data)
  
  # visualize it over google map
  map = leaflet() %>%
    #addTiles() %>%
    addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
    addPolygons(data = field_data)
  map ## show the map
  
}





# 2) define images to use and visualize ----------------------------------------------------

# list images
img_list = list.files("2_Images", full.names=T, pattern = "singledate")

# which bands to use?
bands_to_use = 1:4

# name to add for this experiment
exp_str = "singledate"

# set up deep learning
training_data_dir = "5_sampling1_singledate"
dir.create(training_data_dir, showWarnings = F)
prediction_data_dir = "6_sampling1_prediction_singledate"
dir.create(training_data_dir, showWarnings = F)
weights_fname = "5_sampling1_singledate\\weights_r_save\\unet_tf2_385_0.9311.h5" # best weight
result_fname = "rice_map_single"



# visualize the images
if (FALSE) {
  s2 = stack(img_list)
  plot(s2[[5]])
  
  # for singledate
  mapview(s2[[5]]) + field_data # ndvi
  mapview(s2[[4]]) + field_data # nir
  
  # for multidate
  mapview(s2[[4]]) + field_data # Feb 2020 NDVI
  
}


# 
img_dir = paste0(training_data_dir, "./input/image")
class_dir = paste0(training_data_dir,"./input/class")





# 3) deep learning create grid of patches -----------------------------------------------

# load one image
img = stack(img_list[1])

# create a temporary raster within the extent with block_size as pixel size
# block_size is the size of each tile in meters
block_size = 128*10

#
x_ext = extent(img)

# get the number of cells in the y axis
#n_x = ceiling(abs((x_ext[2] - x_ext[1])) / block_size)
n_y = abs((x_ext[4] - x_ext[3])) / block_size
# adjust extent to fit the cells
x_ext_mod = x_ext
x_ext_mod[4] = x_ext_mod[4] + ((ceiling(n_y) - n_y) * block_size)

# create a raster with this adjusted extent which resolution is our patch size
r = raster(x_ext_mod, crs = crs(img), resolution = block_size)
r[] = 1

# convert raster pixels to polygons
r_pol = rasterToPolygons(r, dissolve=F)#, na.rm=na_rm)
r_pol$id = 1:length(r_pol)

# visualize
x11()
plot(img[[1]])
plot(r_pol,add=T)


# save
writeOGR(r_pol, dsn = "4_grid", layer = "grid", overwrite_layer = T, driver = "ESRI Shapefile")





# 4) deep learning sampling ----------------------------------------------------------------
## the aim of this part is to crop the image into patches that we just created for the locations that have samples
## Note: a limitation to the current code we are using is that it only takes 4 bands
## this is because the use of the PNG file, need to adjust the code to use tif
## so we can use more bands, this is why we just get 4 bands in the [[]] here
## this takes ~15min

# libraries
p_load(raster, rgdal, rgeos, gdalUtils, sp, sf)

# load files
img = stack(img_list)[[bands_to_use]]
samples = st_as_sf(field_data)
grid = st_read("4_grid\\grid.shp")

# reproject samples to the same CRS of the img and grid
samples = st_transform(samples, as.character(crs(grid)))



# create other folders
dir.create(paste0(training_data_dir, "\\input\\image\\"), showWarnings = F, recursive=T)
dir.create(paste0(training_data_dir, "\\input\\class\\"), showWarnings = F, recursive=T)


# rasterize the samples
library(fasterize)
samples_raster = fasterize(samples, img[[1]])
samples_raster[is.na(samples_raster)]=0

# visualize
plot(samples_raster)


# libraries needed
p_load(parallel, doParallel, foreach)

# Begin cluster
cl = parallel::makeCluster(no_cores) # here you specify the number of processors you want to use, if you dont know you can use detectCores() and ideally use that number minus one
#cl = parallel::makeCluster(3, outfile="D:/r_parallel_log.txt") # if you use this you can see prints in the txt
registerDoParallel(cl)

# for each extent
i=1
#foreach(i = 1:1500, .inorder=F, .errorhandling='remove') %dopar% { # test just a few files
foreach(i = 1:length(grid$id), .inorder=F, .errorhandling='remove') %dopar% {
  require(raster)
  require(png)
  require(rgeos)
  require(sf)

  # function to get the sp with the extent of a raster object
  rasext_to_sp = function(x) {
    y = as(extent(x), "SpatialPolygons")
    crs(y) = crs(x)
    return(y)
  }
  
  # check if there is intersection
  if (!gIntersects(rasext_to_sp(img), as_Spatial(grid[i,]))) return(NA)
  
  # crop img and mask
  img_tmp = crop(img, grid[i,]) 
  class_tmp = crop(samples_raster, grid[i,])
  
  # define class presence or absence
  class_presence = "NOO"
  if(1 %in% getValues(class_tmp[[1]])) class_presence="YES"
  print(class_presence)
  
  # row and columns are the same
  if (dim(class_tmp)[1] == dim(class_tmp)[2]) {
    
    # visualization 
    if (FALSE) {
      #if (class_presence == "YES") {
      
      plot(class_tmp); plot(img_tmp)
      plot(stack(img_tmp, class_tmp), main = paste("layer:",i))
      
      # plot(extent(img_tmp),asp=1)
      # plotRGB(img_tmp, r=1, g=2, b=3, add=T, stretch="lin")
      # plot(extent(img_tmp),asp=1)
      # plot(class_tmp,add=T)
    }
    
    # save if YES - only if it has samples
    if (class_presence == "YES") {
      
      # we divide by the scale of data
      # for 1 layer
      #img_tmp2 = matrix(img_tmp, ncol = ncol(img_tmp), byrow = T) / 2047
      
      # for more layers
      #img_tmp = stack(crop(img, grid[i,]) ,crop(img, grid[i,])*2 ,crop(img, grid[i,])*3,crop(img, grid[i,])*4 )
      img_tmp2 = array(img_tmp, c(nrow(img_tmp), ncol(img_tmp), nlayers(img))) #/ 2047
      img_tmp2 = aperm(img_tmp2, c(2,1,3))
      #
      png::writePNG(img_tmp2, paste0(training_data_dir,"\\input\\image\\img_",class_presence,"_",sprintf("%05.0f",i),".png"))
      
      #
      class_tmp = matrix(class_tmp, ncol = ncol(class_tmp), byrow = T)
      png::writePNG(class_tmp, paste0(training_data_dir,"\\input\\class\\cla_",class_presence,"_",sprintf("%05.0f",i),".png"))
      
    }
    
  }
  
  print(paste0(i,ifelse(class_presence=="YES", " YES","")))
  
  
  
  
}

# finish cluster
stopCluster(cl)




# 4a) Visualize some patch samples --------------------------------------------

# list samples
list_img = list.files(img_dir, pattern = "*.png", full.names = TRUE)
list_mask = list.files(class_dir, pattern = "*.png", full.names = TRUE)
length(list_img)

# plot
# i=1
i = sample(1:length(list_img), 1)
r = stack(list_img[i], list_mask[i])
plot(r)
r
print(i)




# 5) DL data visualization -----------------------------------------------


# libraries we're going to need later
# always put reticulate and use_python as the first packages to load, or you will not be able to choose the conda env/python
p_load(reticulate)
use_python(tensorflow_dir, required = T)
p_load(keras, tfdatasets, tidyverse, rsample, magick)
#py_config()


# Quick visualization
## it picks a random sample to show
## if you run multiple times you will see different samples
images <- tibble(
  img = list.files(img_dir, pattern = "*.png", full.names = TRUE),
  mask = list.files(class_dir, pattern = "*.png", full.names = TRUE)
) %>% 
  sample_n(2) %>% 
  map(. %>% magick::image_read())
#
out <- magick::image_append(c(
  magick::image_append(images$img, stack = TRUE), 
  magick::image_append(images$mask, stack = TRUE)
))
#
plot(out)





# 6) deep learning training U-Net --------------------------------------------------

# libraries we're going to need later
# always put reticulate and use_python as the first packages to load, or you will not be able to choose the conda env/python
p_load(reticulate)
use_python(tensorflow_dir, required = T)
p_load(keras, tfdatasets, tidyverse, rsample, magick)
#py_config()

# parameters
#epochs = 400L
epochs = 15L
batch_size = 32L
lr_rate = 0.0001
decay_rate = 0.0001
img_dir = paste0(training_data_dir, "./input/image")
class_dir = paste0(training_data_dir,"./input/class")
data_n_layers = 4


# Quick visualization
images <- tibble(
  img = list.files(img_dir, pattern = "*.png", full.names = TRUE),
  mask = list.files(class_dir, pattern = "*.png", full.names = TRUE)
) %>% 
  sample_n(2) %>% 
  map(. %>% magick::image_read())
#
out <- magick::image_append(c(
  magick::image_append(images$img, stack = TRUE), 
  magick::image_append(images$mask, stack = TRUE)
))
#
plot(out)


# load all data
data_full <- tibble(
  img = list.files(img_dir, pattern = "*.png", full.names = TRUE),
  mask = list.files(class_dir, pattern = "*.png", full.names = TRUE)
)

# random sorting of the data
set.seed(10)
random_order=sample(1:dim(data_full)[1],dim(data_full)[1])
data_full_reorder <- data_full[random_order,]

# split the data between training and validation
data_full_reorder <- initial_split(data_full_reorder, prop = 0.8)
train_samples = length(data_full_reorder$in_id)
train_fname = training(data_full_reorder)$img
test_fname = testing(data_full_reorder)$img

# find the id on the name of imgs string
idx_last_underline = regexpr("\\_[^\\_]*$", basename(test_fname)[1])[1]
ids_validation = as.numeric(substr(basename(test_fname), idx_last_underline + 1, nchar(basename(test_fname))[1] - 4))

# find the id on the name of imgs string
idx_last_underline = regexpr("\\_[^\\_]*$", basename(train_fname)[1])[1]
ids_train = as.numeric(substr(basename(train_fname), idx_last_underline + 1, nchar(basename(train_fname))[1] - 4))

# get the polygons inside each block id
grid = readOGR("4_grid\\grid.shp")
samples_patches_validation = grid[grid$id %in% ids_validation,]
samples_patches_train = grid[grid$id %in% ids_train,]
plot(samples_patches_train, main="train = black, validation = red")
lines(samples_patches_validation, col="red")

# save
save(samples_patches_train, samples_patches_validation, file = paste0("deep_learning_patch_samples_",exp_str,".RData"))




## the model

# mixed precision
tf$keras$mixed_precision$experimental$set_policy('mixed_float16')

dice_coef <- custom_metric("custom", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  result <- (2 * intersection + smooth) / 
    (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  return(result)
})

bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) +
    (1 - dice_coef(y_true, y_pred))
  return(result)
}

#
get_unet_128 <- function(input_shape = c(128, 128, data_n_layers),
                         num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  # 128
  
  down1 <- inputs %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down1_pool <- down1 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 64
  
  down2 <- down1_pool %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down2_pool <- down2 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 32
  
  down3 <- down2_pool %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down3_pool <- down3 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 16
  
  down4 <- down3_pool %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down4_pool <- down4 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 8
  
  center <- down4_pool %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  # center
  
  up4 <- center %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down4, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 16
  
  up3 <- up4 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down3, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 32
  
  up2 <- up3 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down2, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 64
  
  up1 <- up2 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down1, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 128
  
  classify <- layer_conv_2d(up1,
                            filters = num_classes, 
                            kernel_size = c(1, 1),
                            dtype = 'float32', # mixed precision
                            activation = "sigmoid")
  
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  model %>% compile(
    optimizer = optimizer_rmsprop(lr = lr_rate, decay = decay_rate),
    loss = bce_dice_loss,
    #loss = bce_dice_loss_flooding,
    metrics = c(dice_coef)
  )
  
  return(model)
}
#
model <- get_unet_128()






## data augmentation


# random brightness, contrast, hue
random_bsh <- function(img) {
  img <- img %>%
    tf$image$random_brightness(max_delta = 0.2) %>%
    tf$image$random_contrast(lower = 0.9, upper = 1.1) # %>%
  
  # img <- tf$math$multiply(img,tf$random$uniform(shape =  shape(1L), minval = 0.4 ,maxval = 1.6 ,dtype = tf$float32))
  
  # for RGB
  # img_aug <- img[,,1:3]  %>%
  #   tf$image$random_saturation(lower = 0.9, upper = 1.1) %>%
  #   tf$image$random_hue(max_delta = 0.2) #%>%
  # 
  # img <- tf$keras$backend$concatenate(
  #   list(img_aug[,,1:3,drop=FALSE],img[,,4,drop=FALSE]), axis=-1L
  # ) %>% tf$clip_by_value(0, 1)
  
}
random_flip_up_down <- function(x,y) { tf$cond(tf$less(y , 0.25) , function() tf$image$flip_up_down(x), function() x) }
random_flip_left_right <- function(x,y) { tf$cond(tf$greater(y , 0.75), function() tf$image$flip_left_right(x), function() x) }


# map data
create_dataset <- function(data, train, batch_size = 8L, data_n_layers = 3) {
  
  dataset <- data %>%  
    mutate(rot = ifelse(runif(dim(data)[1])>0.75,1,0)*runif(dim(data)[1],min=0,max=2*pi)) %>%
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img =  tf$image$decode_png(tf$io$read_file(.x$img),channels=data_n_layers),
      mask = tf$image$decode_png(tf$io$read_file(.x$mask),channels=1)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
      #mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$uint8)
      mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
    )) 
  
  # set rot variable to a random uniform value
  dataset <- dataset %>% dataset_map(~.x %>% list_modify(
    rot =tf$random$uniform(shape =  shape(1L), minval = 0 ,maxval = 1 ,dtype = tf$float32)
  ))
  
  # apply up/down and left/right flip conditioned by rot 
  dataset <- dataset  %>%
    dataset_map(~.x %>% list_modify(
      img = random_flip_up_down(x=.x$img,y=.x$rot),
      mask = random_flip_up_down(x=.x$mask,y=.x$rot)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = random_flip_left_right(x=.x$img,y=.x$rot),
      mask = random_flip_left_right(x=.x$mask,y=.x$rot)
    ))
  
  
  # data augmentation performed on training set only
  if (train) {
    dataset <- dataset %>%
      dataset_map(~.x %>% list_modify(
        img = random_bsh(.x$img)
      ))
  }
  
  # shuffling on training set only
  if (train) {
    dataset <- dataset %>%
      #  dataset_shuffle(buffer_size = batch_size*128) #
      dataset_shuffle(buffer_size = batch_size*128,seed=666,reshuffle_each_iteration=FALSE)
  }
  
  # train in batches; batch size might need to be adapted depending on
  # available memory
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  
  dataset %>% 
    # output needs to be unnamed
    dataset_map(unname) 
}




## Train

# Training and test set creation now is just a matter of two function calls.
training_dataset <- create_dataset(training(data_full_reorder), train = TRUE, data_n_layers = data_n_layers)
validation_dataset <- create_dataset(testing(data_full_reorder), train = FALSE, data_n_layers = data_n_layers)

# callbacks
dir.create(paste0(training_data_dir, "./epoch_history/"), showWarnings=F)
dir.create(paste0(training_data_dir, "./weights/"), showWarnings=F)
dir.create(paste0(training_data_dir, "./weights_r_save/"), showWarnings=F)
callbacks_list <- list(
  callback_csv_logger(paste0(training_data_dir, "./epoch_history/epoch_history.csv"), separator = ";", append = FALSE),
  callback_model_checkpoint(filepath = paste0(training_data_dir, "./weights/unet_tf2_{epoch:03d}_{val_custom:.4f}.h5"),
                            monitor = "val_custom",save_best_only = TRUE,
                            save_weights_only = TRUE, mode = "max" ,save_freq = NULL)
)

## start training from a set of weights
# load_model_weights_hdf5(model, "./weights_r_save/unet_tf2_111_0.6083_noBSH.h5") # example

# train
training_dataset <- dataset_repeat(training_dataset, count = epochs)
fit_generator(model,training_dataset,validation_data = validation_dataset, workers = 1, steps_per_epoch = as.integer(train_samples / batch_size), epochs = epochs,callbacks = callbacks_list)


# clear GPU
tf$keras.backend$clear_session()
py_gc <- import('gc')
py_gc$collect()




## visualize prediction

if (FALSE) {
  
  ## load saved weights 
  load_model_weights_hdf5(model, weights_fname) # sample5
  
  # example of prediction on the validation data set
  #predictions <- predict(model, validation_dataset)
  # example of prediction on a batch of the the validation data set
  batch <- validation_dataset %>% as_iterator() %>% iter_next()
  predictions <- predict(model, batch)
  
  # clear GPU
  tf$keras.backend$clear_session()
  py_gc <- import('gc')
  py_gc$collect()
  
  # visualize
  images <- tibble(
    image = batch[[1]] #%>% tf$image$central_crop(0.8)
    %>% array_branch(1),
    predicted_mask = predictions %>% array_branch(1),
    mask = batch[[2]]  %>% array_branch(1)
  ) %>% 
    sample_n(2) %>% 
    map_depth(2, function(x) {
      as.raster(x[,,1]) %>% magick::image_read()
    }) %>% 
    map(~do.call(c, .x))
  #
  out <- magick::image_append(c(
    magick::image_append(images$mask, stack = TRUE),
    magick::image_append(images$image, stack = TRUE), 
    magick::image_append(images$predicted_mask, stack = TRUE)
  ))
  
  plot(out)
  
  
}





# 7) Prediction: load some parameters ------------------------------------------

# test dir
test_dir <- paste0(prediction_data_dir, "./pred_input/")


# raster opts
dir.create(paste0(prediction_data_dir, "\\tmp"), showWarnings = F)
rasterOptions(tmpdir="tmp")
#rasterOptions(maxmemory = 5e+10)
#rasterOptions(chunksize = 1e+09)



# create dirs
dir.create(paste0(prediction_data_dir, "\\pred_input"), showWarnings = F, recursive=T)
dir.create(paste0(prediction_data_dir,"\\pred_output"), showWarnings = F)
dir.create(paste0(prediction_data_dir,"\\pred_mosaic"), showWarnings = F, recursive=T)
dir.create(paste0(prediction_data_dir,"\\pred_vector"), showWarnings = F, recursive=T)

# clear some folders before starting
#unlink(list.files(paste0(prediction_data_dir, "./pred_input/"), full.names = TRUE))
#unlink(list.files(paste0(prediction_data_dir, "./pred_output/"), full.names = TRUE))
unlink(list.files(paste0(prediction_data_dir, "./tmp/"), full.names = TRUE))



# load data
img_list = list.files("2_Images", full.names=T, pattern = "singledate")

# load
#predictor_data = stack(img_list)
predictor_data_expand = stack(img_list)[[bands_to_use]]


# define the extents
block_size = 512*10
#predictor_data_ext = split_extent_gdal_bottom(x = predictor_data_expand[[1]], block_size = block_size, na_rm = F, remove_all_zero = F, gdal_path = gdal_path)
predictor_data_ext = split_extent_gdal_border(x =predictor_data_expand, block_size = block_size, na_rm = T, remove_all_zero = F, gdal_path = gdal_path)
length(predictor_data_ext)

# visualize the patches
plot(predictor_data_expand[[1]])
for (i in 1:10) plot(predictor_data_ext[[i]], add=T, col=i)
# for (i in 1:length(predictor_data_ext)) plot(predictor_data_ext[[i]], add=T, col=i)

## to fix borders, one possibility is to create tiles only for the borders of bottom and right


# 32 meters, or 64 pixels (0.5 m)
#vect_overlap = c(-32,32,-32,32)
vect_overlap = c(-res(predictor_data_expand[[1]])[1]*64,res(predictor_data_expand[[1]])[1]*64,-res(predictor_data_expand[[1]])[1]*64, res(predictor_data_expand[[1]])[1]*64)




# 7a) crop image into patches -------------------------------------------------


# libraries needed
p_load(parallel, doParallel, foreach)

# Begin cluster
cl = parallel::makeCluster(no_cores) # here you specify the number of processors you want to use, if you dont know you can use detectCores() and ideally use that number minus one
#cl = parallel::makeCluster(3, outfile="D:/r_parallel_log.txt") # if you use this you can see prints in the txt
registerDoParallel(cl)

# for each extent
i=1
foreach(i = 1:length(predictor_data_ext), .inorder=F, .errorhandling='remove') %dopar% {
  require(raster)
  require(png)
  
  # crop img
  #img_tmp = crop(predictor_data_expand_64, predictor_data_ext[[i]] + vect_overlap)
  img_tmp = crop(predictor_data_expand, predictor_data_ext[[i]] + vect_overlap)
  
  # change NA to 0
  img_tmp[is.na(img_tmp),] = 0
  
  # row and columns are the same
  if (dim(img_tmp)[1] == dim(img_tmp)[2]) {
    
    # we divide by the scale of data
    if (nlayers(img_tmp) == 1) {
      # for 1 layer
      img_tmp2 = matrix(img_tmp, ncol = ncol(img_tmp), byrow = T) #/ 2047
      
    } else {
      
      # for more layers
      img_tmp2 = array(img_tmp, c(nrow(img_tmp), ncol(img_tmp), nlayers(img_tmp))) #/ 2047
      img_tmp2 = aperm(img_tmp2, c(2,1,3))
      
    }
    
    # save
    png::writePNG(img_tmp2, paste0(prediction_data_dir,"\\pred_input\\img","_",sprintf("%05.0f",i),".png"))
    
    
  }
  
}

# finish cluster
stopCluster(cl)





# 7b) deep learning prediction U-Net ------------------------------------------


# libraries we're going to need later
# always put reticulate and use_python as the first packages to load, or you will not be able to choose the conda env/python
p_load(reticulate)
use_python(tensorflow_dir, required = T)
p_load(keras,
       tfdatasets,
       tidyverse,
       rsample
)
#py_config()

## Config

# config
batch_size = 4
data_n_layers = 4
lr_rate = 0.0001



# load data
list_png=list.files(test_dir, pattern = "*.png$", full.names = TRUE)

data <- tibble(
  img = list_png
)

## the model 

# mixed precision
tf$keras$mixed_precision$experimental$set_policy('mixed_float16')

dice_coef <- custom_metric("custom", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  result <- (2 * intersection + smooth) / 
    (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  return(result)
})

bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) +
    (1 - dice_coef(y_true, y_pred))
  return(result)
}

get_unet_128 <- function(input_shape = c(640, 640, data_n_layers),
                         num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  # 128
  
  down1 <- inputs %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down1_pool <- down1 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 64
  
  down2 <- down1_pool %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down2_pool <- down2 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 32
  
  down3 <- down2_pool %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down3_pool <- down3 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 16
  
  down4 <- down3_pool %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  down4_pool <- down4 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  # 8
  
  center <- down4_pool %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") 
  # center
  
  up4 <- center %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down4, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 16
  
  up3 <- up4 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down3, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 32
  
  up2 <- up3 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down2, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 64
  
  up1 <- up2 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down1, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  # 128
  
  classify <- layer_conv_2d(up1,
                            filters = num_classes, 
                            kernel_size = c(1, 1),
                            dtype = 'float32', # mixed precision
                            activation = "sigmoid")
  
  
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  model %>% compile(
    optimizer = optimizer_rmsprop(lr = lr_rate),
    loss = bce_dice_loss,
    metrics = c(dice_coef)
  )
  
  return(model)
}

model <- get_unet_128()

# map data
create_dataset <- function(data, batch_size = 4L, data_n_layers = 3) {
  
  dataset <- data %>%  
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img =  tf$image$decode_png(tf$io$read_file(.x$img), channels=data_n_layers),
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
    )) 
  
  # train in batches; batch size might need to be adapted depending on
  # available memory
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  
  dataset %>% 
    # output needs to be unnamed
    dataset_map(unname) 
}




## Predict 

## load saved weights
load_model_weights_hdf5(model, weights_fname)

# predict
test_dataset <- create_dataset(data, data_n_layers = data_n_layers, batch_size = batch_size)
system.time({ preds <- predict(model, test_dataset) })

print("Prediction end.")

# clear GPU
tf$keras.backend$clear_session()
py_gc <- import('gc')
py_gc$collect()


# load preds
# save(preds, file = "preds.RData")





# 7c) create images from DL results ------------------------------------------------
## ~1min20


# load object from DL prediction
load("preds.RData", verbose=T)


# list files
images_dir = test_dir
images_iter <- list.files(images_dir, pattern = ".png", full.names = FALSE)#[samples_index] 
images_iter2 <- list.files(images_dir, pattern = ".png", full.names = TRUE)#[samples_index] 

# load one file to get vect_overlap   
r_tmp=raster(images_iter2[1])
vect_overlap_pix=c(-(64*res(r_tmp)[1]),(64*res(r_tmp)[1]),-(64*res(r_tmp)[1]),(64*res(r_tmp)[1]))

# libraries needed
p_load(parallel, doParallel, foreach)

# Begin cluster
cl = parallel::makeCluster(no_cores) # here you specify the number of processors you want to use, if you dont know you can use detectCores() and ideally use that number minus one
#cl = parallel::makeCluster(3, outfile="D:/r_parallel_log.txt") # if you use this you can see prints in the txt
registerDoParallel(cl)

# get results from the prediction and save it as a raster without overlap (e.g. 512 x 512)
i=1
foreach(i = 1:dim(preds)[1], .inorder=F, .errorhandling='remove') %dopar% {
  require(raster)
  
  # classe 1
  #new_img1=t(preds[i, , , 1])
  new_img1=preds[i, , , 1]
  new_img1=ifelse(new_img1 < 0.5, 0, 1)
  # classe 2
  # new_img2=t(preds[i, , , 2])
  # new_img2=ifelse(new_img2 < 0.5, 0, 1)
  
  img_array=array(data = NA, dim = c(640,640,1),dimnames = NULL)
  img_array[,,1]=as.matrix(new_img1)
  # img_array[,,2]=as.matrix(new_img1)*0
  # img_array[,,3]=as.matrix(new_img2)
  
  # get the original geographical file to assign the values
  #r=readAll(raster(images_iter2[i]))
  r = crop(predictor_data_expand, predictor_data_ext[[i]] + vect_overlap)[[1]]
  r[]=img_array[,,1]
  
  # crop to the original extent without overlap
  r = crop(r, extent(r) - vect_overlap)
  
  writeRaster(r, paste0(prediction_data_dir, "./pred_output/", sub_extension(images_iter[i], ".tif")), datatype='INT1U', overwrite=TRUE)
  #print(i)
  
}

# finish cluster
stopCluster(cl)

# msg
print("Prediction images saved.")

# clear prediction file
rm(preds)





# 7d) mosaic images -----------------------------------------------------------

# gdal functions
gdalbuildvrt = shQuote(shortPathName(normalizePath(file.path(gdal_path, "gdalbuildvrt.exe"))))
gdal_translate = shQuote(shortPathName(normalizePath(file.path(gdal_path, "gdal_translate.exe"))))

# list files  
list_datamask=unlist(list.files(paste0(prediction_data_dir, "./pred_output/"), full.names = TRUE, pattern=".tif$"))
#list_datamask=list_datamask[-grep("vrt",list_datamask)]
list_datamask=paste(list_datamask,collapse =" ")
name_final_vrt=paste(prediction_data_dir, "./pred_mosaic/", result_fname, ".vrt",sep="")
name_final_TIF=paste(prediction_data_dir, "./pred_mosaic/", result_fname, ".tif",sep="")


system(paste(gdalbuildvrt, name_final_vrt ,list_datamask ))
system(paste(gdal_translate, "-co COMPRESS=NONE -a_nodata 255", name_final_vrt, name_final_TIF))


# 
gc()

#
print("Mosaic done.")



# clear intermediate files
unlink(list.files(paste0(prediction_data_dir, "./tmp/"), full.names = TRUE))
#unlink(list.files(paste0(prediction_data_dir, "./pred_input/"), full.names = TRUE))
#unlink(list.files(paste0(prediction_data_dir, "./pred_output/"), full.names = TRUE))
#unlink(list.files(paste0(prediction_data_dir, "./pred_mosaic/"), full.names = TRUE)) # optional if you want to keep the mosaic
gc()
print(Sys.time())
print("Finished.")




# 8) compare results visually ---------------------------------------------------------


# load U-Net single
unet_map = raster("6_sampling1_prediction_singledate\\pred_mosaic\\rice_map_single.tif")
names(unet_map) = "rice_map"

# load RF
rf_map = crop(raster("s2_stack_classified_rf_2019_2020_singledate.tif"), unet_map)


# visualize
plot(rf_map, main = "rf")
plot(unet_map, main = "unet")


# bigger screen of rf and unet 
x11()
plot(rf_map, main = "rf")
x11()
plot(unet_map, main = "unet")



# visualize in a zoomed area
# new_ext = drawExtent()
# dput(new_ext)
#
# area 1 
new_ext = new("Extent", xmin = 514949.863470392, xmax = 546954.943641848, 
              ymin = 6726688.02349579, ymax = 6741196.99317352)
#plot(rf_map)
#lines(rasext_to_sp(new_ext))

x11()
plot(crop(rf_map,new_ext), main = "rf")
lines(crop(field_data, new_ext))
x11()
plot(crop(unet_map,new_ext), main = "unet")
lines(crop(field_data, new_ext))


# area 2 
new_ext = new("Extent", xmin = 475690.298460073, xmax = 502574.565804096, 
              ymin = 6675479.89522146, ymax = 6694256.20892205)
#plot(rf_map)
#lines(rasext_to_sp(new_ext))

x11()
plot(crop(rf_map,new_ext), main = "rf")
lines(crop(field_data, new_ext))
x11()
plot(crop(unet_map,new_ext), main = "unet")
lines(crop(field_data, new_ext))




# 9) compare results quantitatively ----------------------------------------------


# load U-Net single
unet_map = raster("6_sampling1_prediction_singledate\\pred_mosaic\\rice_map_single.tif")
names(unet_map) = "rice_map"

# load RF
rf_map = crop(raster("s2_stack_classified_rf_2019_2020_singledate.tif"), unet_map)


# load validation data
load("data_df_split_singledate.RData", verbose=T)
length(data_df_valid$class)

# load ML models
load("model_rf_singledate.RData", verbose=T)
fit_rf_single = fit

# predict results for validation data
fit_rf_single_pred = predict(fit_rf_single, data_df_valid)
length(fit_rf_single_pred)


## do the same for the deep learning
# get the polygons that were not used in training
load("deep_learning_patch_samples_singledate.RData", verbose=T)
#
samples_patches_validation = crop(samples_patches_validation, unet_map)





# sample inside the tiles
set.seed(1)
i=1
for (i in 1:length(samples_patches_validation)) {
  tmp = crop(unet_map, samples_patches_validation[i,])
  tmp2 = sampleRandom(tmp, 4, sp=T)
  tmp3 = aggregate(crop(field_data, samples_patches_validation[i,]))
  # plot(tmp)
  # points(tmp2, col="red")
  # lines(tmp3)
  #
  ref = as.numeric(gIntersects(tmp2, tmp3, byid=T))
  map = tmp2$rice_map
  
  if (i == 1) {
    validation_points = cbind(ref,map)
  } else {
    validation_points = rbind(validation_points, cbind(ref,map))
  }
  print(i)
}
validation_points = data.frame(validation_points)
validation_points$ref = as.factor(validation_points$ref)
validation_points$map = as.factor(validation_points$map)



# verify results with confusion matrices
confusionMatrix(data = fit_rf_single_pred, reference = data_df_valid$class, mode="prec_recall", positive = "Rice")
confusionMatrix(data = validation_points$map, reference = validation_points$ref, mode="prec_recall", positive = "1")





# visualize some validation patches
i=12
x11()
par(mfrow = c(4,4), mar = c(1,1,1,1))
for (i in 1:16+50) {
  plot(crop(unet_map, samples_patches_validation[i,]))
  lines(crop(field_data, samples_patches_validation[i,]))
  legend("topright", legend = "unet", bty="n")
}


# visualize some validation patches - RF
x11()
i=12
par(mfrow = c(4,4), mar = c(1,1,1,1))
for (i in 1:16+50) {
  plot(crop(rf_map, samples_patches_validation[i,]))
  lines(crop(field_data, samples_patches_validation[i,]))
  legend("topright", legend = "rf", bty="n")
}



# 10) Final Considerations ---------------------------------------------------

## things to consider when interpreting these results:
## 1) Samples for RF considered for target-class -30 m buffer and bigger polygons -> overestimated performance
## 2) The RF showed lots of noise across the image which were not accounted in the validation -> use better ways to validate based on polygons
## 3) No tuning was done for the DL method -> can improve
## 4) We did not refine the samples for DL, and this affect the DL learning -> can improve
## 5) This is an easy target to detect.
