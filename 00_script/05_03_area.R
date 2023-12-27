# estimating the percentage of area for each species in pres and future
library(terra)

binPath <- "/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals/04_results/binary/"

bin.files <- list.files(path = binPath, pattern='.tif',
                           all.files=TRUE)
binPath
bin.files

fut_2011_2040_SSP370 <- rast(paste0(binPath, bin.files[[1]]))
fut_2011_2040_SSP585 <- rast(paste0(binPath, bin.files[[2]]))
fut_2041_2070_SSP370 <- rast(paste0(binPath, bin.files[[3]]))
fut_2041_2070_SSP585 <- rast(paste0(binPath, bin.files[[4]]))
Baseline <- rast(paste0(binPath, bin.files[[5]]))

# pres.rast <- rast("/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals/04_results/binary/binary_Baseline.tif")
# names(pres.rast)
# minmax(pres.rast)
# plot(pres.rast)

# calculating area
# devtools::install_github("gabferreira/phyloraster", force = TRUE)
library(phyloraster)

area_Baseline <- range_size(Baseline, 
                            cellSz = terra::cellSize(Baseline))

area_2011_2040_SSP370 <- range_size(fut_2011_2040_SSP370, 
                                    cellSz = terra::cellSize(Baseline))

area_2011_2040_SSP585 <- range_size(fut_2011_2040_SSP585, 
                                    cellSz = terra::cellSize(Baseline))

area_2041_2070_SSP370 <- range_size(fut_2041_2070_SSP370, 
                                    cellSz = terra::cellSize(Baseline))

area_2041_2070_SSP585 <- range_size(fut_2041_2070_SSP585, 
                                    cellSz = terra::cellSize(Baseline))

# making a table with the results of area calculation
table <- data.frame(area_Baseline, area_2011_2040_SSP370, area_2011_2040_SSP585,
           area_2041_2070_SSP370, area_2041_2070_SSP585)
table

# saving
write.csv(table, "/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals/04_results/area_scenarios.csv")
