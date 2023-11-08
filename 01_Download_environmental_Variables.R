### download environmental data from 
system("pip install motuclient==1.8.4")

install.packages("CopernicusMarine")
library(matrixStats)
library(CopernicusMarine)
library(reshape2)
library(ggplot2)
library(lubridate)

library(raster)
library(terra)
library(sf)
library(dplyr)
library(ncdf4)




# ------------Set parameters to subset and run the download -------------

CopernicusMarine_uid = "XXXXXX"           #ID
CopernicusMarine_pwd = "XXXXXX"  #password






region=data.frame(-87,-80,-30,-55) #(xmin,ymin,xmax,ymax)


# -------CHL---------
# Global Ocean Biogeochemistry Hindcast DOI https://doi.org/10.48670/moi-00019

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_MULTIYEAR_BGC_001_029" ,
  layer = "cmems_mod_glo_bgc_my_0.25_P1M-m",
  variable = "Mass concentration of chlorophyll a in sea water",
  output = "netcdf",
  region = region, 
  timerange = c("2010-01-01 12:00:00","2015-12-31 12:00:00"),
  sub_variables = c("chl"),
  verticalrange = c(0, 50)
)


copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_MULTIYEAR_BGC_001_029" ,
  layer = "cmems_mod_glo_bgc_my_0.25_P1M-m",
  variable = "Mass concentration of chlorophyll a in sea water",
  output = "netcdf",
  region = region, 
  timerange = c("2016-01-01 12:00:00","2020-12-31 12:00:00"),
  sub_variables = c("chl"),
  verticalrange = c(0, 50)
)


# ---------sea ice-----------

#GLobal Ocean Physics Reanalysis DOI https://doi.org/10.48670/moi-00021

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_026" ,
  layer = "global-reanalysis-phy-001-026-grepv1-ice-monthly",
  variable = "Sea ice area fraction",
  output = "netcdf",
  region = region, 
  timerange = c("2010-01-01 12:00:00","2015-12-31 12:00:00"),
  sub_variables = c("siconc_mean"),
  verticalrange = c(0, 100)
)

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_026" ,
  layer = "global-reanalysis-phy-001-026-grepv1-ice-monthly",
  variable = "Sea ice area fraction",
  output = "netcdf",
  region = region, 
  timerange = c("2016-01-01 12:00:00","2020-12-31 12:00:00"),
  sub_variables = c("siconc_mean"),
  verticalrange = c(0, 100)
)





### sea water potential temperature


#GLOBAL_REANALYSIS_PHY_001_026  https://doi.org/10.48670/moi-00023

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_026" ,
  layer = "global-reanalysis-phy-001-026-grepv1-monthly",
  variable = "Sea water potential temperature",
  output = "netcdf",
  region = region, 
  timerange = c("2010-01-01 12:00:00","2015-12-31 12:00:00"),
  sub_variables = c("thetao_mean"),
  verticalrange = c(0, 100)
)



copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_026" ,
  layer = "global-reanalysis-phy-001-026-grepv1-monthly",
  variable = "Sea water potential temperature",
  output = "netcdf",
  region = region, 
  timerange = c("2016-01-01 12:00:00","2020-12-31 12:00:00"),
  sub_variables = c("thetao_mean"),
  verticalrange = c(0, 100)
)


# mixed layer depth

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_031" ,
  layer = "global-reanalysis-phy-001-031-grepv2-monthly",
  variable = "Ocean mixed layer thickness defined by sigma theta",
  output = "netcdf",
  region = region, 
  timerange = c("2010-01-01 12:00:00","2015-12-31 12:00:00"),
  sub_variables = c("mlotst_cglo"),
  verticalrange = c(0, 20000)
)

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "GLOBAL_REANALYSIS_PHY_001_031" ,
  layer = "global-reanalysis-phy-001-031-grepv2-monthly",
  variable = "Ocean mixed layer thickness defined by sigma theta",
  output = "netcdf",
  region = region, 
  timerange = c("2016-01-01 12:00:00","2020-12-31 12:00:00"),
  sub_variables = c("mlotst_cglo"),
  verticalrange = c(0, 20000)
)

#### surface wind speed

#WIND_GLO_PHY_CLIMATE_L4_MY_012_003 DOI https://doi.org/10.48670/moi-00181

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "WIND_GLO_PHY_CLIMATE_L4_MY_012_003" ,
  layer = "cmems_obs-wind_glo_phy_my_l4_P1M",
  variable = "Wind speed",
  output = "netcdf",
  region = region, 
  timerange = c("2010-01-01 12:00:00","2015-12-31 12:00:00"),
  sub_variables = c("WIND"),
  verticalrange = c(0, 20000)
)

copernicus_download_motu(
  username = CopernicusMarine_uid,
  password = CopernicusMarine_pwd,
  destination = "C:/D1MPA_Review/EnviData",
  product = "WIND_GLO_PHY_CLIMATE_L4_MY_012_003" ,
  layer = "cmems_obs-wind_glo_phy_my_l4_P1M",
  variable = "Wind speed",
  output = "netcdf",
  region = region, 
  timerange = c("2016-01-01 12:00:00","2020-12-31 12:00:00"),
  sub_variables = c("WIND"),
  verticalrange = c(0, 20000)
)





