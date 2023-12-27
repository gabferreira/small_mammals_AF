#' ---
#' title: phylogenetic diversity calculation
#' author: gabriela alves ferreira
#' date: 2023-12-11
#' ---

setwd("/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals")

# packages
library(terra)
library(phyloraster)
library(ape)

###########################
ma <- vect("01_data/02_variables/00_limit/merge_limites_MA_buffer20km_WGS84.shp")
plot(ma)

# load binary rasters
bi_pres <- rast("04_results/binary/binary_Baseline.tif")
bi_2040_SSP370 <- rast("04_results/binary/binary_2011_2040_SSP370.tif")
bi_2040_SSP585 <- rast("04_results/binary/binary_2011_2040_SSP585.tif")
bi_2070_SSP370 <- rast("04_results/binary/binary_2041_2070_SSP370.tif")
bi_2070_SSP585 <- rast("04_results/binary/binary_2041_2070_SSP585.tif")

###########################
# replace NA by 0 and mask by AF
bi_pres <- subst(bi_pres, NA, 0)
bi_pres <- mask(bi_pres, ma, 0)

bi_2040_SSP370 <- subst(bi_2040_SSP370, NA, 0)
bi_2040_SSP370 <- mask(bi_2040_SSP370, ma, 0)

bi_2040_SSP585 <- subst(bi_2040_SSP585, NA, 0)
bi_2040_SSP585 <- mask(bi_2040_SSP585, ma, 0)

bi_2070_SSP370 <- subst(bi_2070_SSP370, NA, 0)
bi_2070_SSP370 <- mask(bi_2070_SSP370, ma, 0)

bi_2070_SSP585 <- subst(bi_2070_SSP585, NA, 0)
bi_2070_SSP585 <- mask(bi_2070_SSP585, ma, 0)

###########################
# calculate richness
sr_pres <- rast.sr(bi_pres)
sr_2040_SSP370 <- rast.sr(bi_2040_SSP370)
sr_2040_SSP585 <- rast.sr(bi_2040_SSP585)
sr_2070_SSP370 <- rast.sr(bi_2070_SSP370)
sr_2070_SSP585 <- rast.sr(bi_2070_SSP585)
plot(sr_pres)

###########################
# calculate PD for each scenario and year
# load phylogenetic tree
tree <- read.tree("01_data/03_phylo_tree/Tree_bootstrap_500.txt")

# calculate pd
pd_pres <- rast.pd(bi_pres, tree)
pd_2040_SSP370 <- rast.pd(bi_2040_SSP370, tree)
pd_2040_SSP585 <- rast.pd(bi_2040_SSP585, tree)
pd_2070_SSP370 <- rast.pd(bi_2070_SSP370, tree)
pd_2070_SSP585 <- rast.pd(bi_2070_SSP585, tree)

# visualize the results
pdf("pd_mammals.pdf", height = 6, width = 6)
par(mfrow = c(3, 2))

plot(pd_pres, main = "PD Baseline")
plot(pd_2040_SSP370, main = "PD 2011-2040 SSP370")
plot(pd_2040_SSP585, main = "PD 2011-2040 SSP585")
plot(pd_2070_SSP370, main = "PD 2041-2070 SSP370")
plot(pd_2070_SSP585, main = "PD 2041-2070 SSP585")
dev.off()

###########################
# save the rasters for PD
writeRaster(pd_pres, "04_results/phylo_div/phylo_div_Baseline.tif",
            overwrite=TRUE)
writeRaster(pd_2040_SSP370, "04_results/phylo_div/phylo_div_2011_2040_SSP370.tif",
            overwrite=TRUE)
writeRaster(pd_2040_SSP585, "04_results/phylo_div/phylo_div_2011_2040_SSP585.tif",
            overwrite=TRUE)
writeRaster(pd_2070_SSP370, "04_results/phylo_div/phylo_div_2041_2070_SSP370.tif",
            overwrite=TRUE)
writeRaster(pd_2070_SSP585, "04_results/phylo_div/phylo_div_2041_2070_SSP585.tif", 
            overwrite=TRUE)

# or
pd <- c(pd_pres, pd_2040_SSP370, pd_2040_SSP585, pd_2070_SSP370, pd_2070_SSP585)
names(pd) <- c("pd_pres", "pd_2040_SSP370", "pd_2040_SSP585", "pd_2070_SSP370", 
               "pd_2070_SSP585")
x11()
plot(pd)
writeRaster(pd, "04_results/pd_mammals.tif")

###########################
# save the rasters for SR
writeRaster(sr_pres, "04_results/richness/richness_Baseline.tif",
            overwrite=TRUE)
writeRaster(sr_2040_SSP370, "04_results/richness/richness_2011_2040_SSP370.tif",
            overwrite=TRUE)
writeRaster(sr_2040_SSP585, "04_results/richness/richness_2011_2040_SSP585.tif",
            overwrite=TRUE)
writeRaster(sr_2070_SSP370, "04_results/richness/richness_2041_2070_SSP370.tif",
            overwrite=TRUE)
writeRaster(sr_2070_SSP585, "04_results/richness/richness_2041_2070_SSP585.tif", 
            overwrite=TRUE)

# or
sr <- c(sr_pres, sr_2040_SSP370, sr_2040_SSP585, sr_2070_SSP370, sr_2070_SSP585)
names(sr) <- c("sr_pres", "sr_2040_SSP370", "sr_2040_SSP585", "sr_2070_SSP370", 
               "sr_2070_SSP585")
x11()
plot(sr)
writeRaster(sr, "04_results/sr_mammals.tif")
