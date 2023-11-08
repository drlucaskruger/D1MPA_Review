#### krillpodym 

# modeled  krill biomass data from:

# Green, D. B., Titaud, O., Bestley, S., Corney, S. P., Hindell, M. A., 
# Trebilco, R., Conchon, A. and Lehodey, P. 2023. 
# KRILLPODYM: a mechanistic, spatially resolved model of Antarctic krill 
# distribution and abundance. - Frontiers in Marine Science 
# https://doi.org/10.3389/fmars.2023.1218003

# data stored in
# https://doi.org/10.25959/895K-K978

rm(list=ls())

gc()


# Load required libraries
library(raster)
library(ncdf4)
library(dplyr)
library(lubridate)
# load mask and read the coordinate reference system CRS 

mask <- raster("C:/D1MPA_Review/D1_Mask.tif") # mask

plot(mask)

EPSG6932<-crs(mask) # extract the coordinate spatial system
EPSG6932

# Specify the folder where your .nc files are stored
folder_path <- "C:/D1MPA_Review/KrillPodym/Data/"

# List all .nc files in the specified folder
nc_files <- list.files(path = folder_path, pattern = ".nc", full.names = TRUE)

# Create empty lists to store rasters for each variable within warm and cold periods
warm_krill_adults <- list()
warm_krill_juveniles <- list()
warm_krill_late_larvae <- list()

cold_krill_adults <- list()
cold_krill_juveniles <- list()
cold_krill_late_larvae <- list()

# Loop through each .nc file
for (file_path in nc_files) {
  # Read the NetCDF file as a raster brick
  krill_data <- brick(file_path,varname="krill_adults")
  
  # Extract time dimension values{
  
  time_values <- getZ(krill_data)
  months<-month(as.POSIXct(time_values,format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  
  # Check if the month is in a warm or cold period
  if (any(months %in% c(10, 11, 12, 1, 2, 3))) {
    warm_krill_adults <- append(warm_krill_adults, (krill_data))
    
  } else if (any(months %in% c(4, 5, 6, 7, 8, 9))) {
    cold_krill_adults <- append(cold_krill_adults, (krill_data))
    
  }
}



# Loop through each .nc file
for (file_path in nc_files) {
  # Read the NetCDF file as a raster brick
  krill_data <- brick(file_path,varname="krill_juveniles")
  
  # Extract time dimension values{
  
  time_values <- getZ(krill_data)
  months<-month(as.POSIXct(time_values,format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  
  # Check if the month is in a warm or cold period
  if (any(months %in% c(10, 11, 12, 1, 2, 3))) {
    warm_krill_juveniles <- append(warm_krill_juveniles, (krill_data))
    
  } else if (any(months %in% c(4, 5, 6, 7, 8, 9))) {
    cold_krill_juveniles <- append(cold_krill_juveniles, (krill_data))
    
  }
}



# Loop through each .nc file
for (file_path in nc_files) {
  # Read the NetCDF file as a raster brick
  krill_data <- brick(file_path,varname="krill_late_larvae")
  
  # Extract time dimension values{
  
  time_values <- getZ(krill_data)
  months<-month(as.POSIXct(time_values,format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  
  # Check if the month is in a warm or cold period
  if (any(months %in% c(10, 11, 12, 1, 2, 3))) {
    warm_krill_late_larvae <- append(warm_krill_late_larvae, (krill_data))
    
  } else if (any(months %in% c(4, 5, 6, 7, 8, 9))) {
    cold_krill_late_larvae <- append(cold_krill_late_larvae, (krill_data))
    
  }
}


plot(cold_krill_adults[[1]])

# Stack and calculate mean rasters for each variable within warm and cold periods
warm_mean_krill_adults <- mean(stack(warm_krill_adults))
warm_mean_krill_juveniles <- mean(stack(warm_krill_juveniles))
warm_mean_krill_late_larvae <- mean(stack(warm_krill_late_larvae))

cold_mean_krill_adults <- mean(stack(cold_krill_adults))
cold_mean_krill_juveniles <- mean(stack(cold_krill_juveniles))
cold_mean_krill_late_larvae <- mean(stack(cold_krill_late_larvae))

plot(log(warm_mean_krill_late_larvae+1))




#### ------------- reproject rasters and extract using the doman 1 mask -------

# wind cold
# reproject the raster to EPSG 6932

wmka <- projectRaster(warm_mean_krill_adults, crs = EPSG6932)
wmkj <- projectRaster(warm_mean_krill_juveniles, crs = EPSG6932)
wmkl <- projectRaster(warm_mean_krill_late_larvae, crs = EPSG6932)

cmka <- projectRaster(cold_mean_krill_adults, crs = EPSG6932)
cmkj <- projectRaster(cold_mean_krill_juveniles, crs = EPSG6932)
cmkl <- projectRaster(cold_mean_krill_late_larvae, crs = EPSG6932)


# Set the extent of 'tmp_raster' to match the mask raster ('mask')
wmka <- crop(wmka, extent(mask))
wmkj <- crop(wmkj, extent(mask))
wmkl <- crop(wmkl, extent(mask))

cmka <- crop(cmka, extent(mask))
cmkj <- crop(cmkj, extent(mask))
cmkl <- crop(cmkl, extent(mask))


#resample the raster based on the cell size of the mask

wmka<- resample(wmka, mask, method = "bilinear")
wmkj<- resample(wmkj, mask, method = "bilinear")
wmkl<- resample(wmkl, mask, method = "bilinear")

cmka<- resample(cmka, mask, method = "bilinear")
cmkj<- resample(cmkj, mask, method = "bilinear")
cmkl<- resample(cmkl, mask, method = "bilinear")




# Mask 'tmp_raster' using 'nc'
wmka <- mask(wmka, mask = mask)
wmkj <- mask(wmkj, mask = mask)
wmkl <- mask(wmkl, mask = mask)

cmka <- mask(cmka, mask = mask)
cmkj <- mask(cmkj, mask = mask)
cmkl <- mask(cmkl, mask = mask)



plot(log(cmka+1))


writeRaster(wmka,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_adults_warm.asc")
writeRaster(cmka,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_adults_cold.asc")

writeRaster(wmkj,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_juveniles_warm.asc")
writeRaster(cmkj,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_juveniles_cold.asc")

writeRaster(wmkl,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_larvae_warm.asc")
writeRaster(cmkl,"C:/D1MPA_Review/KrillPodym/MeanValues/krill_larvae_cold.asc")


rm(list=ls())

gc()
