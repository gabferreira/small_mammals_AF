#########
library(terra)

setwd("/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals")

pts <- read.csv("01_data/01_occurrences/pontos_final _corrected.csv", sep = ";")

spps.list <- lapply(unique(pts$sp), function(i, x) { 
  x[x$sp == i,]
}, x = pts)

names(spps.list) <- unique(pts$sp) 

pts.v <- vect(pts, geom = c("long", "lat"))

pts.b <- aggregate(buffer(pts.v, 1))

ext(pts.b)

e <- c(-58.2, -37.2, -32.4, -6)

##############################
##############################
##############################
# DEPOIS DE EXECUTAR O LOOP DA LINHA 44, SE O LOOP DA LINHA 109 NAO FUNCIONAR
# EXECUTAR A LINHA 25 E A LINHA 44 NOVAMENTE (INCLUINDO AS LINHAS 74-78)
# r.dim <- rast("04_results/Akodon_cursor_Baseline.tif")
############################
############################
############################

path <- "04_results2/"

# files <- list.files(paste0("03_sdm/", names(spps.list)[1]), pattern = paste0("_thr_", names(spps.list)[1], ".tif"), full.names = TRUE)

files.names <- c("Baseline", "2011_2040_SSP370", "2011_2040_SSP585", "2041_2070_SSP370", "2041_2070_SSP585")

files.names <- files.names[c(1, 5)]

ma <- vect("01_data/02_variables/00_limit/merge_limites_MA_buffer20km_WGS84.shp")

# pdf("binary_occ_clipped2.pdf", width = 12, height = 6)  # Adjust width and height as needed
# par(mfrow = c(1, 4))

# Loop over species
for (i in seq_along(spps.list)) {
  # Get a list of raster files for the current species
  files <- list.files(paste0("03_sdm/", names(spps.list)[i]), 
                      pattern = paste0("_thr_", names(spps.list)[i], ".tif"), full.names = TRUE)
  
  files <- files[c(1, 5)]
  
  # Loop over raster files for the current species
  for (j in seq_along(files)) {
    # Read the raster file
    r <- rast(files[j])
    
    # Read the corresponding vector data
    v <- vect(spps.list[[i]], geom = c("long", "lat"))
    
    # Create a buffer around the vector data
    b <- aggregate(buffer(v, 1))
    
    crs(b) <- "epsg:4326"
    
    crs(r) <- "epsg:4326"
    
    # Crop the raster data using the buffered vector data
    # r.crop <- crop(r, b, mask = TRUE)
    r.crop <- mask(r, b)
    
    # r.crop <- extend(r.crop, ext(pts.b) + 1)
    
    ################
    ################
    ################
    # set.ext(r.crop, c(-59.1778, -36.3972, -33.2467, -5.1667))
    # 
    # r.crop <- resample(r.crop, r.dim)
    # 
    # r.crop <- crop(r.crop, pts.b, mask = TRUE)
    ################
    ################
    ################
    
    # plot(ext(ma), main = paste0(names(spps.list)[i], "\n", files.names[j]))
    # 
    # # Plot the cropped raster data
    # plot(r.crop, legend = FALSE, add = TRUE)
    # 
    # # Plot the original vector data
    # plot(v, pch = 20, cex = 1.2, col = "red", add = TRUE)
    # 
    # plot(ma, add = TRUE)
    # 
    # text(x = -55, y = -10, paste0("occs:", nrow(spps.list[[i]])))
    
    writeRaster(r.crop >= 1, paste0(path,
                                    names(spps.list)[i],
                                    "_",
                                    files.names[j],
                                    ".tif"), overwrite = TRUE)
  }
}
# dev.off()
my.list <- list()

sp.nm <- sort(unique(pts$sp))

sp.nm <- sp.nm[-43]

for (k in seq_along(files.names)) {
  rich <- list.files(path, pattern = files.names[k], full.names = TRUE)
  my.list[[k]] <- rast(rich)
  names(my.list[[k]]) <- sp.nm
}

names(my.list) <- files.names

for (l in seq_along(my.list)) {
  rich <- terra::app(my.list[[l]], sum, na.rm = TRUE)
  writeRaster(rich, paste0("richness_", names(my.list[l]), ".tif"), overwrite = TRUE)
  writeRaster(my.list[[l]], paste0("binary_", names(my.list[l]), ".tif"), overwrite = TRUE)
}

rich.base <- rast("richness_Baseline.tif")
rich.7085 <- rast("richness_2041_2070_SSP585.tif")

plot(ext(ma), main = "Richness\nBaseline")

plot(rich.base, legend = FALSE, add = TRUE)

plot(ma, add = TRUE)

plot(ext(ma), main = "Richness\n2041_2070_SSP585")

plot(rich.7085, add = TRUE, legend = FALSE)

plot(ma, add = TRUE)
# dev.off()
