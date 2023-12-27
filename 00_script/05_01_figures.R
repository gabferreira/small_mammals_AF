#' ---
#' title: figures manuscript mammals
#' author: gabriela alves ferreira
#' date: 2023-12-11
#' ---

setwd("/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals")
# setwd("D:/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals/")

# packages
library(terra)

# load richness rasters
sr_pres <- rast("04_results/richness/richness_Baseline.tif")
sr_2040_SSP370 <- rast("04_results/richness/richness_2011_2040_SSP370.tif")
sr_2040_SSP585 <- rast("04_results/richness/richness_2011_2040_SSP585.tif")
sr_2070_SSP370 <- rast("04_results/richness/richness_2041_2070_SSP370.tif")
sr_2070_SSP585 <- rast("04_results/richness/richness_2041_2070_SSP585.tif")

# load phylogenetic diversity rasters
pd_pres <- rast("04_results/phylo_div/phylo_div_Baseline.tif")
pd_2040_SSP370 <- rast("04_results/phylo_div/phylo_div_2011_2040_SSP370.tif")
pd_2040_SSP585 <- rast("04_results/phylo_div/phylo_div_2011_2040_SSP585.tif")
pd_2070_SSP370 <- rast("04_results/phylo_div/phylo_div_2041_2070_SSP370.tif")
pd_2070_SSP585 <- rast("04_results/phylo_div/phylo_div_2041_2070_SSP585.tif")

# load phylogenetic delta croped by PAs and remnants
delta_pd_rem <- rast("04_results/phylo_div/delta_pd_2070_585_REMNANTS.tif")
delta_pd_pas <- rast("04_results/phylo_div/delta_pd_2070_585_PAs.tif")

# shapefile south america
amer <- vect("/media/gabriela/Gabi_HD/shapes_rasters/shapes/americadoSul/South_America_Countries/South_America.shp")
bras <- vect("/media/gabriela/Gabi_HD/shapes_rasters/shapes/UFs_Brasil/estados_2010.shp")

##############################
# richness maps

# breaks for richness maps
breaks_rq <- seq(0, 28, by = 0.1)
colors_rq <- colorRampPalette(c("grey90","#7ec4ef","#0053AF","green","yellow",
                                "#FD982E", "#FD0000"))(length(breaks_rq)-1)

# ploting and saving
setwd("04_results/figures")
jpeg("richness_baseline.jpg", width = 560, height = 640, quality = 100)

plot(sr_pres, col = c(colors_rq), range = c(0, 28),
     axes = T,
     main = "(a) Baseline",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()

# ploting
jpeg("figure S3.jpeg", width = 620, height = 620, quality = 100)
par(mfrow = c(2,2))
# jpeg("richness_SSP585.jpg", width = 1020, height = 620, quality = 100)
# par(mfrow = c(1,2))

plot(sr_2040_SSP370, col = c(colors_rq), range = c(0, 28),
     axes = T,
     main = "(a) 2050 SSP370",
     plg = list( # parameters for drawing legend
       # title = "2050 RCP 85",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(sr_2040_SSP585, col = c(colors_rq), range = c(0, 28),
     axes = T,
     main = "(b) 2050 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(sr_2070_SSP370, col = c(colors_rq), range = c(0, 28),
     axes = T,
     main = "(c) 2070 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(sr_2070_SSP585, col = c(colors_rq), range = c(0, 28),
     axes = T,
     main = "(d) 2070 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
# dev.off()

# jpeg("richness_SSP370.jpg", width = 1020, height = 620, quality = 100)
# par(mfrow = c(1,2))
dev.off()

##############################
# delta
library(phyloraster)
delta2040_370 <- delta.grid(sr_pres, sr_2040_SSP370)
delta2040_585 <- delta.grid(sr_pres, sr_2040_SSP585)

delta2070_370 <- delta.grid(sr_pres, sr_2070_SSP370)
delta2070_585 <- delta.grid(sr_pres, sr_2070_SSP585)

## breaks para os mapas de delta riqueza
minmax(c(delta2040_370, delta2040_585,
         delta2070_370, delta2070_585))

breaks_dt <- seq(-19, 4, by = 0.2)
# colors_dt <- colorRampPalette(c("#993636","#AC5341","#BF6F4D","#D18C58","#F7C56F","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))
# colors_dt <- colorRampPalette(c("#A20A19","#BB061C","#D40320","#ED0024","#f07470","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))
colors_dt <- colorRampPalette(c("#0F0E0F","#450B0C","#7B0708",
                                "#E60001","#D65D42","#C0C6CB","blue"))(
                                  length(breaks_dt))


# plotando
setwd("04_results/figures")
# tiff("delta_richness2.tiff", width = 920, height = 920, res = 600)
# tiff("delta_richness2.jpeg", width = 9, height = 9, res = 400, units = "in")
jpeg("delta_richness.jpeg", width = 620, height = 620, quality = 100)

par(mfrow = c(2,2))
plot(delta2040_370, col = c(colors_dt), range = c(-19, 4),
     axes = T,
     main = "(b) 2050 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(delta2040_585, col = c(colors_dt), range = c(-19, 4),
     axes = T,
     main = "(c) 2050 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

# breaks_dt <- seq(-17, 7, by = 0.1)
# colors_dt <- colorRampPalette(c("#450B0C","#660000","#B30D02","#FF0800","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))


plot(delta2070_370, col = c(colors_dt), range = c(-19,4),
     axes = T,
     main = "(d) 2070 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(delta2070_585, col = c(colors_dt), range = c(-19,4),
     axes = T,
     main = "(e) 2070 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()

##############################
# phylogenetic diversity maps

# breaks for phylodiv maps
breaks_pd <- seq(0, 7.269466, by = 0.07)
colors_pd <- colorRampPalette(c("grey90","#FBF15E", "#5DC863FF","#21908CFF",
                                "#3B528BFF","#440154FF"))(length(breaks_pd)-1)
# colors_pd <- colorRampPalette(c("white","#EB6122","#C9532C","#A84535","#86383F","#652A48","#431C52"))(length(breaks_pd)-1)

# ploting and saving
setwd("04_results/figures")
jpeg("phylodiv_baseline.jpg", width = 560, height = 640, quality = 100)

plot(pd_pres, col = c(colors_pd), range = c(0, 7.269466),
     axes = T,
     main = "(a) Baseline",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()

# ploting
jpeg("figure S4.jpeg", width = 620, height = 620, quality = 100)
par(mfrow = c(2,2))
# jpeg("richness_SSP585.jpg", width = 1020, height = 620, quality = 100)
# par(mfrow = c(1,2))

plot(pd_2040_SSP370, col = c(colors_pd), range = c(0, 7.269466),
     axes = T,
     main = "(a) 2050 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(pd_2040_SSP585, col = c(colors_pd), range = c(0, 7.269466),
     axes = T,
     main = "(b) 2050 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
# dev.off()

# jpeg("richness_SSP370.jpg", width = 1020, height = 620, quality = 100)
# par(mfrow = c(1,2))

plot(pd_2070_SSP370, col = c(colors_pd), range = c(0, 7.269466),
     axes = T,
     main = "(c) 2070 SSP370",
     plg = list( # parameters for drawing legend
       # title = "2050 RCP 85",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(pd_2070_SSP585, col = c(colors_pd), range = c(0, 7.269466),
     axes = T,
     main = "(d) 2070 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()

##############################
# delta
delta_pd2040_370 <- delta.grid(pd_pres, pd_2040_SSP370)
delta_pd2040_585 <- delta.grid(pd_pres, pd_2040_SSP585)

delta_pd2070_370 <- delta.grid(pd_pres, pd_2070_SSP370)
delta_pd2070_585 <- delta.grid(pd_pres, pd_2070_SSP585)

## breaks para os mapas de delta riqueza
minmax(c(delta_pd2040_370, delta_pd2040_585,
         delta_pd2070_370, delta_pd2070_585))

breaks_dt <- seq(-4.233147, 1.228576, by = 0.08)
# colors_dt <- colorRampPalette(c("#993636","#AC5341","#BF6F4D","#D18C58","#F7C56F","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))
# colors_dt <- colorRampPalette(c("#A20A19","#BB061C","#D40320","#ED0024","#f07470","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))
colors_dt <- colorRampPalette(c("#0F0E0F","#450B0C","#7B0708",
                                "#E60001","#C0C6CB","blue"))(length(breaks_dt))

# plotando
# tiff("delta_richness2.tiff", width = 920, height = 920, res = 600)
# tiff("delta_richness2.jpeg", width = 9, height = 9, res = 400, units = "in")
jpeg("delta_phylodiv.jpeg", width = 620, height = 620, quality = 100)

par(mfrow = c(2,2))
plot(delta_pd2040_370, col = c(colors_dt), range = c(-4.233147, 1.228576),
     axes = T,
     main = "(b) 2050 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(delta_pd2040_585, col = c(colors_dt), range = c(-4.233147, 1.228576),
     axes = T,
     main = "(c) 2050 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

# breaks_dt <- seq(-17, 7, by = 0.1)
# colors_dt <- colorRampPalette(c("#450B0C","#660000","#B30D02","#FF0800","#C0C6CB","#00A6D7","#0058B3","darkblue"))(length(breaks_dt))

plot(delta_pd2070_370, col = c(colors_dt), range = c(-4.233147, 1.228576),
     axes = T,
     main = "(d) 2070 SSP370",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(delta_pd2070_585, col = c(colors_dt), range = c(-4.233147, 1.228576),
     axes = T,
     main = "(e) 2070 SSP585",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()

##############################
# delta PAs and REMNANTS
delta_pd_pas
delta_pd_rem

# breaks for delta PA and delta remnant maps
breaks_delta_pa_rm <- seq(-4.011922, 1.201255, by = 0.1)
colors_delta_pa_rm <- colorRampPalette(c("red", "orange","yellow",
                                         "grey70", "green"))(length(
                                           breaks_delta_pa_rm)-1)
# colors_delta_pa_rm <- colorRampPalette(c("brown","#836953",
#                                          "#0b9abd"))(length(breaks_delta_pa_rm)-1)

# ploting and saving
setwd("04_results/figures")

jpeg("delta_PD_PAs_remnants_2070_585.jpg", width = 1020, height = 520, 
     quality = 100)
par(mfrow = c(1,2))

plot(delta_pd_pas, col = c(colors_delta_pa_rm), range = c(-4.011922, 
                                                          1.201255),
     axes = T,
     main = "(a) Protected areas",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax = list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)

plot(delta_pd_rem, col = c(colors_delta_pa_rm), range = c(-4.011922, 1.201255),
     axes = T,
     main = "(b) Atlantic Forest Remnants",
     plg = list( # parameters for drawing legend
       # title = "b)",
       # title.cex = 2, # Legend title size
       cex = 1.3), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.5), # Axis text size
     cex.main = 2) # Title text size
terra::lines(bras, lwd = 0.3, alpha = 0.9)
terra::lines(amer, lwd = 0.3, alpha = 0.9)
dev.off()
