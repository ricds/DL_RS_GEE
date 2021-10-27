# Script: Using RGEE to download Sentinel-2 data and run Random Forests ------------------------------------------------------------
# Author: Ricardo Dal'Agnol da Silva (ricds@hotmail.com)
# Date Created: 2021-08-09
# R version 4.1.0 (2021-05-18)
#

## The experiment - what is in this script?
# We want to get data from Sentinel-2 satellite from GEE to perform crop classification
# The aim is to detect rice or no-rice
# We have field data (polygons) from CONAB/Brazil for rice crops between 2019/2020 https://portaldeinformacoes.conab.gov.br/mapeamentos-agricolas.html
# We will extract data for rice (inside polygons) and no-rice (outside - this is a simplification)
# Then we will train a Random Forests model in R using the S2 data and the field information


# clean environment
rm(list = ls()); gc()

# libraries
library(pacman)
p_load(raster,
       rgdal,
       rgeos,
       gdalUtils,
       sp,
       sf,
       leaflet,
       mapview,
       caret,
       doParallel)

# set working directory
setwd("D:\\2_Projects\\7_Presentation_Classes\\27_Short_Course_IEEE_GRSS_ISPRS_GEE\\")

# directory in the drive to save results (temporarily)
drive_dir = "rgee_temp3"

# directory in the computer to save images
output_dir = "2_Images\\"
dir.create(output_dir, showWarnings = FALSE)

# set directory for your conda environment
## Change this to your environment directory as it was installed in the rgee_install_packages 
## if you forgot environment name, open anaconda prompt in windows, type in: conda env list
rgee_environment_dir = "C:\\ProgramData\\Miniconda3\\envs\\rgee_py\\"

# number of cores
# to check number of cores in your computer type in detectCores(), then use that number minus one
no_cores = 2



# 1) initiate RGEE --------------------------------------------------------------------

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





# 2) load field data -----------------------------------------------------

# load field data
# get these data from https://doi.org/10.5281/zenodo.5504554
field_data = readOGR("1_Field\\ARROZ-RS_Safra_2019_2020\\RS_ARROZ_IRRIG_INUND_1920.shp")

# lets filter these data to only one municipality, list municipalities and get only one
unique(field_data$NM_MUNICIP)
field_data = field_data[field_data$NM_MUNICIP == "URUGUAIANA",]

# reproject to the same projection of the satellite data
field_data = spTransform(field_data, crs("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
field_data_ee = sf_as_ee(as(field_data,"sf"))

# visualization
if (FALSE) {
  # simple plot
  plot(field_data)
  
  # visualize it over google map
  mapview(field_data)
  
}

# define the area covering the field_data
if (!file.exists("3_field_data_convex\\field_data_convex.shp")) {
  dir.create("3_field_data_convex", showWarnings=F)
  field_data_convex = gConvexHull(field_data)
  field_data_convex$id = 1
  writeOGR(field_data_convex, dsn = "3_field_data_convex", layer = "field_data_convex", driver = "ESRI Shapefile")
} else {
  field_data_convex = readOGR("3_field_data_convex\\field_data_convex.shp")
}



# 3) RGEE get satellite data  ------------------------------------------------------

# define periods to extract data between, 1st to 2nd, 2nd to 3rd, etc.
periods_extract = c("2020-02","2020-03")
bands_extract = c("B2", "B3", "B4", "B8", "NDVI")

# Example how you would set up for multiple dates
#periods_extract = c("2019-12","2020-01","2020-02", "2020-03")
#bands_extract = c("B2", "B3", "B4", "B8", "NDVI")


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

# calculate ndvi
calc_ndvi = function (image) {
  ndvi = image$normalizedDifference(c("B8", "B4"))
  return(image$addBands(ndvi$rename('NDVI')))
}

# get geometry based on the field_data extent
p_load(sf)
field_data_sp = as(extend(extent(field_data), 0.1), "SpatialPolygons")
crs(field_data_sp) = crs(field_data)
#field_data_sp = spTransform(field_data_sp, CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
field_data_sp_ee = sf_as_ee(as(field_data_sp, "sf"))
Map$addLayer(eeObject = field_data_sp_ee, name = 'ROI') +
  Map$addLayer(eeObject = field_data_ee, name= 'Field')


# Loop through the period intervals acquiring the data
i=1
tasks = list()
for (i in 1:(length(periods_extract)-1)) {
  
  # get the data, filter for clouds and make a temporal mosaic
  ## rice planted in sep-dec, harvest in feb-may
  ## image from january and february should be in high productivity
  sentinel2_data = ee$ImageCollection('COPERNICUS/S2_SR')$
    # filter the ROI
    filterBounds(field_data_sp_ee$geometry())$
    map(function(image){ image$clip(field_data_sp_ee) })$
    # Select the date of year
    filterDate(paste0(periods_extract[i],'-01'), paste0(periods_extract[i+1],'-01'))$
    # Pre-filter to get less cloudy granules.
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE',20))$ 
    # filter clouds
    map(maskS2clouds)$
    map(calc_ndvi)$
    median()
  
  # center to the field data
  Map$setCenter(gCentroid(field_data)@coords[1], gCentroid(field_data)@coords[2], 14);
  
  # create the plot
  gee_map = Map$addLayer(eeObject = sentinel2_data, 
                         visParams = list(
                           bands = c("B4", "B3", "B2"),
                           min = 0,
                           max = 0.3
                         ),
                         name = 'RGB') 
  
  # display both local spatial data and the map from GEE
  # other way to do it is this but it does not allow to disable each layer separately: mapview(field_data, map = gee_map)
  mapview(field_data)@map + gee_map
  
  # download data to drive
  tasks[[i]]  = ee_image_to_drive(
    image = sentinel2_data$select(bands_extract),
    maxPixels = 1e12,
    region = field_data_sp_ee$geometry(),
    scale = 10,
    folder = drive_dir,
    crs = "EPSG:32721",
    description = paste0('sentinel2_data_', periods_extract[i]),
    fileNamePrefix = paste0('sentinel2_data_', periods_extract[i]),
    timePrefix = FALSE
  )
  tasks[[i]]$start()
  
  print(i)
}

# monitor tasks and then save the results to disk
ee_monitoring(eeTaskList = TRUE)
## this took here 1280s or 21.3 minutes

# copy files to computer
for (i in 1:(length(periods_extract)-1)) {
  # copy file from drive to computer (windows temp dir)
  fname = ee_drive_to_local(task = tasks[[i]])
  # move file to output_dir
  file.rename(fname,
              paste0(output_dir, basename(fname)))
  rm(fname)
}

# visualize 
r=stack(paste0(output_dir, basename(fname)))
r
plot(r[[5]])


## RGEE ends here, now we will do some machine learning (bonus lesson !)



# 4a) extract s2 data from field locations RANDOMLY ONE PIXEL ------------------------------------
## we want to check the signal across samples to see if makes sense
## this picks just one pixel inside each field polygon to avoid spatial autocorrelation

# list images
img_list = list.files("2_Images", full.names=T)

# create a shrinked version of the field data to use for sampling
# i did this to remove edge effects and small polygons
field_data_shrink = disaggregate(buffer(field_data, -30))
field_data_shrink$area = gArea(field_data_shrink, byid=T)
field_data_shrink = field_data_shrink[field_data_shrink$area > 10000,]

# pick one pixel inside each of the field polygons
set.seed(1)
random_points_in_sp = list()
for (i in 1:length(field_data_shrink)) {
  random_points_in_sp[[i]] = spsample(field_data_shrink[i,], n = 1, type = "random", iter = 100)
}
random_points_in_sp = bind(random_points_in_sp)
random_points_in_sp
plot(random_points_in_sp)


# loop through the images we acquired extracting the data
i=1
if (exists('s2_field_mean_attr')) { rm(s2_field_mean_attr) }
for (i in 1:length(img_list)) {
  
  # load sentinel2 data
  s2 = stack(img_list[i])
  
  # extract data
  s2_extracted = raster::extract(s2, random_points_in_sp)#[1:100,])
  
  #
  hist(s2_extracted)
  boxplot(s2_extracted)
  
  # assign values to the matrix
  if (!exists("s2_field_mean_attr")){
    s2_field_mean_attr = s2_extracted
  } else {
    s2_field_mean_attr = cbind(s2_field_mean_attr, s2_extracted)
  }
  
  # clear variables
  rm(s2_extracted, s2)
  
  # counter
  print(i)
}

# plot the signal 
boxplot(s2_field_mean_attr)




# 4b) random sampling outside of field_data -----------------------------------
# This is for the non-rice class

# load sentinel2 data
s2 = stack(img_list[1])

# visualize
plot(s2[[5]])
lines(field_data)
lines(field_data_convex, col="red")

# random sample inside the area
# to do the spatial sampling
# https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/rSSI
# example: https://rpubs.com/jguelat/sampling
p_load(spatstat)

# create window around the data
p_load(maptools)
peri_owin = as.owin(W= field_data_convex)

# Sample points with a minimal distance between each other
# we do this to minimize spatial autocorrelation
set.seed(1)
pts <- rSSI(r = 1000, length(field_data) * 10, peri_owin)
plot(pts)
pts

# Check the distances
coord <- cbind(pts$x, pts$y)
distmat <- dist(coord)
min(distmat)

# create sp object with the result
pts_sampled = SpatialPoints(data.frame(x = pts$x, y = pts$y))
crs(pts_sampled) = crs(field_data)
plot(pts_sampled)

# filter by field_data areas
#pts_sampled_buffer = disaggregate(buffer(pts_sampled, 15))
pts_sampled_int = gIntersects(pts_sampled, field_data, byid=T, returnDense=F)
pts_sampled = pts_sampled[-as.numeric(names(unlist(pts_sampled_int))),]

# get the same number of samples as field data
set.seed(1)
pts_sampled = pts_sampled[sample(1:length(pts_sampled), length(field_data)),]
plot(pts_sampled)



# 4c) extract data from random samples ----------------------------------------

i=1
if (exists('s2_random_mean_attr')) { rm(s2_random_mean_attr) }
for (i in 1:length(img_list)) {
  
  # load sentinel2 data
  s2 = stack(img_list[i])
  
  # extract data
  s2_extracted = raster::extract(s2, pts_sampled)#[1:100,])
  
  #
  hist(s2_extracted)
  boxplot(s2_extracted)
  
  # assign values to the matrix
  #s2_random_mean_attr[,i] = s2_extracted
  if (!exists("s2_random_mean_attr")){
    s2_random_mean_attr = s2_extracted
  } else {
    s2_random_mean_attr = cbind(s2_random_mean_attr, s2_extracted)
  }
  
  # clear variables
  rm(s2_extracted, s2)
  
  # counter
  print(i)
}

# plot the signal over time
boxplot(s2_random_mean_attr)



# 5) training and validation datasets ----------------------------------------

# full dataset
data_df = rbind(data.frame(s2_field_mean_attr, class = "Rice"),
                data.frame(s2_random_mean_attr, class = "Other"))
data_df$class = factor(data_df$class)

# visualize the data frame
View(data_df)

# create data partition splitting data between training (70%) and validation (30%)
p_load(caret)
set.seed(1)
prop = 0.7
idx_train = createDataPartition(data_df$class, p = prop)$Resample1
data_df_train = data_df[idx_train,]
data_df_valid = data_df[-idx_train,]
print(c(dim(data_df_train)[1], dim(data_df_valid)[1]))

# save samples
save(data_df, data_df_train, data_df_valid, file = "data_df_split.RData")



# 6) Data visualization ---------------------------------------------------

p_load(lattice)

# load data
load("data_df_split.RData", verbose=T)

# visualize data
trellis.par.set(theme = col.whitebg(), warn = FALSE)
print(featurePlot(x = data_df[,bands_extract],
                  y = data_df$class,
                  plot = "density"))

print(featurePlot(x = data_df[,bands_extract],
                  y = data_df$class,
                  plot = "box"))

print(featurePlot(x = data_df[,bands_extract],
                  y = data_df$class,
                  plot = "pairs"))



# 7) Machine Learning model training --------------------------------------------------------------

# load data
load("data_df_split.RData", verbose=T)

# # set train control
# fitControl = trainControl(
#   method = 'boot'                   # k-fold cross validation
#   ,number = 30,                      # number of folds
#   #repeats = n_tests
#   #,seeds=1
# )

# set train control
fitControl = trainControl(
  method = 'repeatedcv',                   # k-fold cross validation
  #number = 10,                      # number of folds
  repeats = 10
  #,seeds=1
)

# random forest
p_load(doParallel)
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
fit = train(class ~ ., data = data_df_train, method='rf', trControl = fitControl, tuneLength=10)
stopCluster(cl)

# check training performance
fit

# check variable importance
fit_imp = varImp(fit, scale=F, conditional = TRUE)
plot(fit_imp)

# save model
save(fit, file = "model_rf.RData")





# 8) ML model predict -----------------------------------------------------------

# load model
load("model_rf.RData", verbose=T)
fit

# load sentinel2 data
s2_stack = stack(img_list)[[bands_extract]]
s2_stack_crop = s2_stack

# Some locations within the area for zoomed visualization
# x11(); plot(s2_stack[[5]]); new_ext = drawExtent()
# extent with some different crops/pastures
new_ext1 = new("Extent", xmin = 498275.457842248, xmax = 505702.928377153,
              ymin = 6688974.10244787, ymax = 6697144.32003627)
# extent with river
new_ext2 = new("Extent", xmin = 498245.263116387, xmax = 505747.971062663,
    ymin = 6716611.34952561, ymax = 6725286.3555885)

# crop image to a smaller extent to predict - so its faster for this tutorial
new_ext = new_ext1
s2_stack_crop = crop(s2_stack, new_ext)


# predict rice for the image
system.time({
s2_stack_classified = predict(s2_stack_crop, fit, na.rm=T)
})
## this took 29 min for the whole image, and 9 seconds for the smaller extent
## to predict for the whole image you should skip the crop process

# optional: load the full predicted image - in case you dont run it yourself, just uncomment line below
# s2_stack_classified = raster("s2_stack_classified_rf_2019_2020_singledate.tif")


# visualize
x11(width = 10, height = 10)
plot(new_ext, asp=1)
plotRGB(s2_stack_crop, r=1, g=2, b=3, main = "RGB", stretch= "lin",add=T)
plot(field_data, add=T, border = "red")
x11(width = 10, height = 10)
plot(new_ext, asp=1)
plot(s2_stack_classified, main = "RF", add=T)
plot(field_data, add=T)
plot(field_data_convex, border="blue", add=T, lwd=2)

# save result
# writeRaster(s2_stack_classified, filename = "s2_stack_classified_rf_2019_2020_singledate.tif")


## Thats it! Hope you learned something :)

