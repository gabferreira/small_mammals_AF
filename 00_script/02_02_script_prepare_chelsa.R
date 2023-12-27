#' ---
#' title: var - download
#' author: mauricio vancine
#' date: 2022-04-21
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(doParallel)
library(parallelly)
library(foreach)

# options
options(timeout = 1e6)

# prepare -----------------------------------------------------------------

# directory
dir.create("01_data/02_variables/01_climate_present/01_cropped")
dir.create("01_data/02_variables/02_climate_future/01_cropped")

### import vector ----
li <- terra::vect("01_data/02_variables/00_limit/merge_limites_MA_buffer20km_WGS84.shp")
li
plot(li)

### wc present ----
files <- dir(path = "/media/mude/afe69132-ffdb-4892-b809-a0f7d2b8f423/spatial_data_base/02_raster/chelsa/bioclimatic/", 
             pattern = "1981-2010", full.names = TRUE)
files

chelsa_p <- terra::rast(files)
chelsa_p

names(chelsa_p) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
chelsa_p

chelsa_p_li <- terra::crop(chelsa_p, li, mask = TRUE) 
chelsa_p_li

chelsa_p_li_5km <- terra::aggregate(chelsa_p_li, fact = 5)
chelsa_p_li_5km

plot(chelsa_p_li_5km[[1]])
lines(li)

terra::writeRaster(chelsa_p_li_5km, 
                   paste0("01_data/02_variables/01_climate_present/01_cropped/", names(chelsa_p_li_5km), ".tif"), 
                   overwrite = TRUE)

### wc future ----
pattern <- paste0(c(
    #"2011-2040_mpi-esm1-2-hr_ssp370",
    #"2011-2040_mpi-esm1-2-hr_ssp585",
    "2041-2070_mpi-esm1-2-hr_ssp370",
    "2041-2070_mpi-esm1-2-hr_ssp585",
    
    #"2011-2040_ipsl-cm6a-lr_ssp370",
    #"2011-2040_ipsl-cm6a-lr_ssp585",
    "2041-2070_ipsl-cm6a-lr_ssp370",
    "2041-2070_ipsl-cm6a-lr_ssp585",
    
    #"2011-2040_ukesm1-0-ll_ssp370",
    #"2011-2040_ukesm1-0-ll_ssp585",
    "2041-2070_ukesm1-0-ll_ssp370",
    "2041-2070_ukesm1-0-ll_ssp585"), collapse = "|")

files <- dir(path = "/media/mude/afe69132-ffdb-4892-b809-a0f7d2b8f423/spatial_data_base/02_raster/chelsa/bioclimatic", 
             pattern = pattern, full.names = TRUE)
files

doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=files) %dopar% {
    
    print(i)
    terra::rast(i) %>% 
        terra::crop(li, mask = TRUE) %>% 
        terra::aggregate(fact = 5) %>% 
        terra::writeRaster(paste0("01_data/02_variables/02_climate_future/01_cropped/", basename(i)), overwrite = TRUE)
    
}
doParallel::stopImplicitCluster()

# end ---------------------------------------------------------------------