library(raster)
library(rasterVis)
library(maptools)
library(maps)

library(reshape2)
library(ggplot2)
library(lubridate)

library(raster)
library(terra)
library(sf)
library(dplyr)
library(ncdf4)
library(tidyverse)

# load mask and read the coordinate reference system CRS 

nc <- raster("C:/D1MPA_Review/D1_Mask.tif") # mask
plot(nc)


EPSG6932<-crs(nc) # extract the coordinate spatial system
EPSG6932



### load and process variables

### PS. I changed the name of some files to have the data and date ranges

#### ------------Sea ice cover period 1------------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-026-grepv1-ice-monthly_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "siconc_mean"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_sd)
plot(cold_mean)

# SIC warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/SIC_warm_2010_2015.asc")



# SIC cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/SIC_cold_2010_2015.asc")





#### ------------Sea ice cover period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-026-grepv1-ice-monthly_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "siconc_mean"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# SIC warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/SIC_warm_2016_2020.asc")



# SIC cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/SIC_cold_2016_2020.asc")




#### ------------Chlorophyll-a concentration period 1------------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_mod_glo_bgc_my_0.25_P1M-m_CHL_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "chl"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_mean)
plot(cold_mean)

# chl warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/chl_warm_2010_2015.asc")



# chl cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/chl_cold_2010_2015.asc")





#### ------------Chlrophyll period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_mod_glo_bgc_my_0.25_P1M-m_CHL_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "chl"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# chl warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/chl_warm_2016_2020.asc")



# chl cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/chl_cold_2016_2020.asc")



### ----------Mixed Layer Depth period 1---------------


ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-031-grepv2-monthly_MLD_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "mlotst_cglo"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_mean)
plot(cold_mean)

# mld warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/mld_warm_2010_2015.asc")



# mld cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/mld_cold_2010_2015.asc")





#### ------------mixed layer depth period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-031-grepv2-monthly_MLD_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "mlotst_cglo"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# mld warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/mld_warm_2016_2020.asc")



# mld cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/mld_cold_2016_2020.asc")




###--------------- sea surface temperature period 1---------------



ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-026-grepv1-monthly_SST_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "thetao_mean"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_mean)
plot(cold_mean)

# sst warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/sst_warm_2010_2015.asc")



# sst cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/sst_cold_2010_2015.asc")





#### ------------Sea surface temperature period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "global-reanalysis-phy-001-026-grepv1-monthly_SST_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "thetao_mean"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# sst warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/sst_warm_2016_2020.asc")



# sst cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/sst_cold_2016_2020.asc")




###   ------------- northward wind speed period 1-----------------



ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_obs-wind_glo_phy_my_l4_P1M_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "northward_wind"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_mean)
plot(cold_mean)

# wind warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Nwind_warm_2010_2015.asc")



# wind cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Nwind_cold_2010_2015.asc")





#### ------------north wind speed period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_obs-wind_glo_phy_my_l4_P1M_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "northward_wind"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# wind warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Nwind_warm_2016_2020.asc",overwrite=TRUE)



# wind cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Nwind_cold_2016_2020.asc",overwrite=TRUE)






###   ------------- eastward wind speed period 1-----------------



ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_obs-wind_glo_phy_my_l4_P1M_2010_2015"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "eastward_wind"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()


plot(warm_mean)
plot(cold_mean)

# wind warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Ewind_warm_2010_2015.asc")



# wind cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Ewind_cold_2010_2015.asc")





#### ------------north wind speed period 2---------------

ncpath <- "C:/D1MPA_Review/EnviData/"
ncname <- "cmems_obs-wind_glo_phy_my_l4_P1M_2016_2020"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "eastward_wind"
tmp_raster <- brick(ncfname, varname=dname)

# Extract time dimension values (assuming time dimension is the first dimension)
time_values <- getZ(tmp_raster)

# Create empty lists to store rasters for each time unit
warm_rasters <- list()
cold_rasters <- list()


# Loop through time units and separate rasters based on seasons (Southern Hemisphere)
for (i in 1:length(time_values)) {
  month <- as.POSIXlt(time_values[i], origin = "2010-01-01")$mon + 1 # Add 1 to convert from 0-based index to 1-based index
  
  # Warm months: October to March
  if (month %in% c(10,11,12,1,2,3)) {
    warm_rasters[[length(warm_rasters) + 1]] <- tmp_raster[[i]]
  }
  # Cold months: April to September
  else if (month %in% c(4,5,6,7,8,9)) {
    cold_rasters[[length(cold_rasters) + 1]] <- tmp_raster[[i]]
  }
  
}
warm_rasters


# Calculate mean for each season
warm_mean <- stack(warm_rasters) %>% mean()
cold_mean <- stack(cold_rasters) %>% mean()

plot(warm_mean)
plot(cold_mean)

# wind warm
# reproject the raster to EPSG 6932

warm_raster <- projectRaster(warm_mean, crs = EPSG6932)

plot(warm_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(warm_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Ewind_warm_2016_2020.asc")



# wind cold
# reproject the raster to EPSG 6932

cold_raster <- projectRaster(cold_mean, crs = EPSG6932)

plot(cold_raster)

# Set the extent of 'tmp_raster' to match the mask raster ('nc')
tmp_raster <- crop(cold_raster, extent(nc))

#resample the raster based on the cell size of the mask

tmp_raster <- resample(tmp_raster, nc, method = "bilinear")

# Mask 'tmp_raster' using 'nc'
masked_raster <- mask(tmp_raster, mask = nc)

plot(masked_raster)

writeRaster(masked_raster,"C:/D1MPA_Review/EnviData/MeanVars/Ewind_cold_2016_2020.asc")



### eastward and northward wind speed needs further processing

eww1<-raster("C:/D1MPA_Review/EnviData/MeanVars/Ewind_warm_2010_2015.asc")
nww1<-raster("C:/D1MPA_Review/EnviData/MeanVars/Nwind_warm_2010_2015.asc")


ww1<-sqrt((eww1*eww1)+(nww1*nww1))
plot(ww1)

eww2<-raster("C:/D1MPA_Review/EnviData/MeanVars/Ewind_warm_2016_2020.asc")
nww2<-raster("C:/D1MPA_Review/EnviData/MeanVars/Nwind_warm_2016_2020.asc")

ww2<-sqrt((eww2*eww2)+(nww2*nww2))
plot(ww2)



ewc1<-raster("C:/D1MPA_Review/EnviData/MeanVars/Ewind_cold_2010_2015.asc")
nwc1<-raster("C:/D1MPA_Review/EnviData/MeanVars/Nwind_cold_2010_2015.asc")


wc1<-sqrt((ewc1*ewc1)+(nwc1*nwc1))
plot(wc1)

ewc2<-raster("C:/D1MPA_Review/EnviData/MeanVars/Ewind_cold_2016_2020.asc")
nwc2<-raster("C:/D1MPA_Review/EnviData/MeanVars/Nwind_cold_2016_2020.asc")

wc2<-sqrt((ewc2*ewc2)+(nwc2*nwc2))
plot(wc2)





writeRaster(ww1,"C:/D1MPA_Review/EnviData/MeanVars/windspeed_warm_2010_2015.asc")
writeRaster(ww2,"C:/D1MPA_Review/EnviData/MeanVars/windspeed_warm_2016_2020.asc")

writeRaster(wc1,"C:/D1MPA_Review/EnviData/MeanVars/windspeed_cold_2010_2015.asc")
writeRaster(wc2,"C:/D1MPA_Review/EnviData/MeanVars/windspeed_cold_2016_2020.asc")


