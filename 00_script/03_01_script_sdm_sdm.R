#' ---
#' title: sdm - sdm
#' author: mauricio vancine
#' date: 2023-04-21
#' ---

# prepare r -------------------------------------------------------------

sdm::installAll()

# packages
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(earth)
library(e1071)
library(randomForest)
library(tmap)
library(tmaptools)
library(usdm)
library(sdm)
library(rJava)

# options
options(scipen = 2)
tmap_options(show.messages = FALSE, show.warnings = FALSE)

# maxent
download.file(url = "https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
              destfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"), mode = "wb")
unzip(zipfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"),
      exdir = system.file("java", package = "dismo"), junkpaths = TRUE)
dir(system.file("java", package = "dismo"))

# import data -------------------------------------------------------------

setwd("/media/gabriela/Gabi_HD/Manuscritos_em_producao/Manuscrito_Ricardo_Div_Filogenetica/novas_analises/enm_mammals")

# limit
li <- sf::st_read("01_data/02_variables/00_limit/merge_limites_MA_buffer20km_WGS84.shp")
li
terra::ext(terra::vect(li))

## occurrence ----
occ <- readr::read_csv2("01_data/01_occurrences/pontos_final.csv") %>% 
    dplyr::rename(species = sp) %>% 
    dplyr::mutate(longitude = as.numeric(paste0(stringr::str_sub(long, end = 3), ".", stringr::str_sub(long, start = 3))),
                  latitude = as.numeric(paste0(stringr::str_sub(lat, end = 3), ".", stringr::str_sub(lat, start = 3)))) %>% 
    dplyr::select(species, longitude, latitude)
occ
unique(occ$species)

# occ <- subset(occ, species == "Cavia_fulgida")
# occ

occ <- subset(occ, species == "Phyllomys_pattoni")
occ

dplyr::count(occ, species, sort = TRUE)

sf::st_as_sf(occ, coords = c("longitude", "latitude"), crs = 4326) %>% 
    tm_shape() +
    tm_bubbles(col = "species", size = .2) +
    tm_shape(li) +
    tm_borders() +
    tm_layout(legend.show = FALSE)

## variables ----
var_p <- dir(path = "01_data/02_variables/01_climate_present/01_cropped", pattern = ".tif", full.names = TRUE) %>% 
    terra::rast()
var_p

tm_shape(var_p[[1]]) +
    tm_raster(legend.show = FALSE) +
    tm_shape(li) +
    tm_borders()

var_f <- dir(path = "01_data/02_variables/02_climate_future/01_cropped", pattern = ".tif", full.names = TRUE) %>% 
    terra::rast()
var_f

tm_shape(var_f[[1]]) +
    tm_raster(legend.show = FALSE) +
    tm_shape(li) +
    tm_borders()

## selection ----
var_p_vif <- usdm::vifstep(x = var_p, th = 2) # selecao das variaveis para cada limite

var_p_sel <- raster::brick(usdm::exclude(var_p, var_p_vif))
var_f_sel <- terra::subset(var_f, grep(paste0(var_p_vif@results$Variables, collapse = "|"), names(var_f)))

readr::write_csv(var_p_vif@results, "01_data/02_variables/02_var_vif.csv")

## oppc ----
var_id <- var_p[[1]]
var_id[!is.na(var_id)] <- terra::extract(var_id, terra::as.points(var_id), cells = TRUE)[, 1]
var_id
plot(var_id)

occ_oppc <- occ %>% 
    dplyr::mutate(oppc = terra::extract(var_id, occ[, 2:3])[, 2]) %>% 
    dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
    tidyr::drop_na(oppc) %>% 
    dplyr::select(-oppc) %>% 
    dplyr::add_count(species) %>% 
    dplyr::arrange(-n)
occ_oppc

dplyr::count(occ_oppc, species, sort = TRUE) %>% 
    dplyr::mutate(n10 = case_when(n >= 10 ~ 1, TRUE ~ 0)) %>% 
    dplyr::count(n10)

# occ_oppc_n10 <- occ_oppc %>%
#     dplyr::filter(n >= 10)
# occ_oppc_n10

# map
tm_shape(var_p[[1]]) +
    tm_raster(pal = "Spectral") +
    tm_shape(sf::st_as_sf(occ_oppc, coords = c("longitude", "latitude"), crs = 4326)) +
    tm_bubbles(size = .1, col = "species") +
    tm_graticules(lines = FALSE) +
    tm_layout(legend.outside = TRUE)

# sdm ---------------------------------------------------------------------

# directory
dir.create("03_sdm")

# 0. parameters ----
names(sdm::getmethodNames())

m <- c(
    "glm", # Generalized Linear Model
    "mars", # Multivariate Adaptive Regression Splines
    "rf", # Ranfom Forest
    "svm", # Support Vector Machines
    "maxent", # MaxEnt
    "maxlike" # MaxLike
)

n <- 10 # réplicas
ncores <- 4 # parallel::detectCores()
t <- 30 # particao

stat <- c("AUC", "COR", "TSS", "Kappa", "threshold", "Deviance", 
          "obs.prevalence", "sensitivity", "specificity", "NMI", "phi", "ppv", 
          "npv", "ccr", "prevalence")

fut <- c("2011-2040_mpi-esm1-2-hr_ssp370",
         "2011-2040_mpi-esm1-2-hr_ssp585",
         "2041-2070_mpi-esm1-2-hr_ssp370",
         "2041-2070_mpi-esm1-2-hr_ssp585",
         
         "2011-2040_ipsl-cm6a-lr_ssp370",
         "2011-2040_ipsl-cm6a-lr_ssp585",
         "2041-2070_ipsl-cm6a-lr_ssp370",
         "2041-2070_ipsl-cm6a-lr_ssp585",
         
         "2011-2040_ukesm1-0-ll_ssp370",
         "2011-2040_ukesm1-0-ll_ssp585",
         "2041-2070_ukesm1-0-ll_ssp370",
         "2041-2070_ukesm1-0-ll_ssp585")

# sdm
# for(i in unique(occ_oppc_n10$species)){
for(i in unique(occ_oppc$species)){
    
    # information
    print(paste0("Model to ", i))
    
    # directory
    dir.create(paste0("03_sdm/", i))
    
    # filter
    # occ_i <- occ_oppc_n10 %>% 
    occ_i <- occ_oppc %>% 
        dplyr::filter(species == i) %>% 
        dplyr::mutate(species = 1)
    readr::write_csv(occ_i, paste0("03_sdm/", i, "/01_occ_oppc_", i, ".csv"))
    
    # coords
    occ_i_c <- occ_i[, -4]
    coordinates(occ_i_c) <- c("longitude", "latitude")
    crs(occ_i_c) <- "+proj=longlat +datum=WGS84 +no_defs"
    
    # 1. prepare data ----
    sdm_data <- sdm::sdmData(formula = species~.,
                             train = occ_i_c,
                             predictors = var_p_sel,
                             bg = list(n = nrow(occ_i_c),
                                       method = "gRandom",
                                       remove = TRUE))
    saveRDS(sdm_data, file = paste0("03_sdm/", i, "/03_01_sdm_data_", i, ".rds"))
    
    # 2. fit models ----
    sdm_fit <- sdm::sdm(species ~ .,
                        data = sdm_data,
                        replication = "boot",
                        n = n,
                        test.percent = t,
                        parallelSetting = list(ncores = ncores, method = "parallel"),
                        methods = m)
    saveRDS(sdm_fit, paste0("03_sdm/", i, "/03_02_sdm_fit_", i, ".rds"))
    
    # 3. evaluation ----
    
    ## evaluation table ----
    evaluation <- sdm::getEvaluation(sdm_fit, stat = stat, opt = 2) %>%  # opt = 2 > max_sen_esp
        dplyr::left_join(sdm_fit@run.info, ., by = "modelID") %>% 
        tibble::as_tibble()
    readr::write_csv(evaluation, paste0("03_sdm/", i, "/03_03_sdm_eval_data_", i, ".csv"))
    
    # tss para a media
    evaluation_filter <- evaluation %>% 
        tidyr::drop_na() %>% 
        dplyr::filter(TSS > mean(TSS))
    
    # ## roc ----
    for(j in evaluation_filter$method %>% unique()){
        png(paste0("03_sdm/", i, "/03_03_sdm_eval_roc_", i, "_", j, ".png"), wi = 30, he = 20, un = "cm", res = 300)
        sdm::roc(sdm_fit, p = (evaluation_filter[evaluation_filter$method == j, ]$modelID), method = j, main = paste0("ROC plot filter - ", j))
        dev.off()
    }
    
    png(paste0("03_sdm/", i, "/03_03_sdm_eval_roc_", i, ".png"), wi = 30, he = 20, un = "cm", res = 300)
    sdm::roc(sdm_fit, p = evaluation_filter$modelID)
    dev.off()
    
    # ## response curves ----
    data_response <- sdm::rcurve(sdm_fit, id = evaluation_filter$modelID)
    plot_response <- data_response + 
        facet_wrap(~variable, scales = "free") +
        labs(title = "") +
        theme_bw(base_size = 20) +
        theme(panel.grid = element_blank())
    ggsave(filename = paste0("03_sdm/", i, "/03_03_sdm_eval_response_", i, ".png"),
           plot = plot_response, wi = 35, he = 20, un = "cm", dpi = 300)
    
    ## variable importance ----
    data_varimp <- sdm::getVarImp(sdm_fit, id = evaluation_filter$modelID, wtest = "test.dep")@varImportanceMean$corTest
    
    readr::write_csv(data_varimp, paste0("03_sdm/", i, "/03_03_sdm_eval_varimp_", i, ".csv"))
    
    plot_varimp <- data_varimp %>% 
        ggplot(aes(x = forcats::fct_relevel(variables, rev(data_varimp$variables)), y = corTest)) +
        geom_bar(stat = "identity", fill = "gray50") +
        geom_errorbar(aes(ymin = lower, ymax = upper), 
                      width = .2,
                      position = position_dodge(.9)) +
        coord_flip() +
        labs(x = "Variables", y = "Relative Variable Importante") +
        theme_bw(base_size = 20) +
        theme(panel.grid = element_blank())
    ggsave(filename = paste0("03_sdm/", i, "/03_03_sdm_eval_varimp_", i, ".png"),
           plot = plot_varimp, wi = 35, he = 20, un = "cm", dpi = 300)
    
    # 4. ensemble ----
    
    ## present ----
    ens_p <- sdm::ensemble(x = sdm_fit,
                           newdata = var_p_sel,
                           filename = paste0("03_sdm/", i, "/04_01_sdm_ensemble_present_", i, ".tif"),
                           parallelSetting = list(ncores = ncores, method = "parallel"),
                           overwrite = TRUE,
                           setting = list(
                               id = evaluation_filter$modelID,
                               method = "weighted",
                               stat = "TSS",
                               opt = 2
                           ))
    names(ens_p) <- sub("X04_01_", "", names(ens_p))
    
    ## future ----
    ens_f <- raster::stack()
    for(j in fut){
        
        var_f_sel_j <- raster::brick(terra::subset(var_f_sel, grep(j, names(var_f_sel))))
        names(var_f_sel_j) <- names(var_p_sel)
        
        ens_f_i_sel_j <- sdm::ensemble(x = sdm_fit,
                                       newdata = var_f_sel_j,
                                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_", i, "_", j, ".tif"),
                                       parallelSetting = list(ncores = ncores, method = "parallel"),
                                       overwrite = TRUE,
                                       setting = list(
                                           id = evaluation_filter$modelID,
                                           method = "weighted",
                                           stat = "TSS",
                                           opt = 2
                                       ))
        names(ens_f_i_sel_j) <- sub("X04_02_", "", names(ens_f_i_sel_j))
        ens_f <- raster::stack(ens_f, ens_f_i_sel_j)
        
    }
    
    # mean
    ens_f_2011_2040_ssp370 <- mean(subset(subset(ens_f, grep("2011.2040", names(ens_f))), grep("ssp370", names(subset(ens_f, grep("2011.2040", names(ens_f)))))))
    ens_f_2011_2040_ssp585 <- mean(subset(subset(ens_f, grep("2011.2040", names(ens_f))), grep("ssp585", names(subset(ens_f, grep("2011.2040", names(ens_f)))))))
    ens_f_2041_2070_ssp370 <- mean(subset(subset(ens_f, grep("2041.2070", names(ens_f))), grep("ssp370", names(subset(ens_f, grep("2041.2070", names(ens_f)))))))
    ens_f_2041_2070_ssp585 <- mean(subset(subset(ens_f, grep("2041.2070", names(ens_f))), grep("ssp585", names(subset(ens_f, grep("2041.2070", names(ens_f)))))))
    
    # export
    terra::writeRaster(x = rast(ens_f_2011_2040_ssp370), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2011_2040_ssp370_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2011_2040_ssp585), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2011_2040_ssp585_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2041_2070_ssp370), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2041_2070_ssp370_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2041_2070_ssp585), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2041_2070_ssp585_", i, ".tif"),
                       overwrite = TRUE)
    
    # 5. threshold ------------------------------------------------------------
    
    # metrics
    sdm_eval <- sdm::evaluates(x = as.data.frame(sdm_data)$species, 
                               p = raster::extract(ens_p, coordinates(sdm_data)))
    sdm_eval
    
    sdm_eval@threshold_based %>% 
        readr::write_csv(paste0("03_sdm/", i, "/04_03_sdm_ensemble_thr_", i, ".csv"))
    
    ## metrics ----
    tibble::tibble(Prevalence = sdm_eval@statistics$Prevalence,
                   AUC = sdm_eval@statistics$AUC,
                   TSS = sdm_eval@threshold_based$TSS[2],
                   COR = sdm_eval@statistics$COR[1],
                   p = sdm_eval@statistics$COR[2],
                   Deviance = sdm_eval@statistics$Deviance) %>% 
        readr::write_csv(paste0("03_sdm/", i, "/04_03_sdm_ensemble_eval_", i, ".csv"))
    
    
    ## threshold max(se+sp) ----
    thr <- sdm_eval@threshold_based$threshold[2]
    
    # cut
    ens_p_thr <- ens_p >= thr
    ens_f_2011_2040_ssp370_thr <- ens_f_2011_2040_ssp370 >= thr
    ens_f_2011_2040_ssp585_thr <- ens_f_2011_2040_ssp585 >= thr
    ens_f_2041_2070_ssp370_thr <- ens_f_2041_2070_ssp370 >= thr
    ens_f_2041_2070_ssp585_thr <- ens_f_2041_2070_ssp585 >= thr
    
    # export
    terra::writeRaster(x = rast(ens_p_thr), 
                       filename = paste0("03_sdm/", i, "/04_01_sdm_ensemble_present_thr_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2011_2040_ssp370_thr), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2011_2040_ssp370_thr_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2011_2040_ssp585_thr), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2011_2040_ssp585_thr_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2041_2070_ssp370_thr), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2041_2070ssp370_thr_", i, ".tif"),
                       overwrite = TRUE)
    
    terra::writeRaster(x = rast(ens_f_2041_2070_ssp585_thr), 
                       filename = paste0("03_sdm/", i, "/04_02_sdm_ensemble_future_2041_2070ssp585_thr_", i, ".tif"),
                       overwrite = TRUE)
    
}

# end ---------------------------------------------------------------------
