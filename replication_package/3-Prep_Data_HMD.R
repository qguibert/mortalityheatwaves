#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-06-09
#####################################################################################################
#' @Information :
#' This script extracts and formats data from HMD.
#' Cela nécessite au préalable d'avoir crééer un compte sur le site
#'<https://www.mortality.org/> et de se servir de son login et mot de passe
#'pour se connnecter et récupérer les données.
#####################################################################################################

rm(list=ls(all=TRUE))

# Folds
folder <- getwd() # If necessary, please set your working directory
fold_data <- paste0(folder, "/data/")
fold_data_raw <- paste0(folder, "/data_raw/")
fold_bib <- paste0(folder, "/functions/")
folderTables <- paste0(folder, "/tables/")
folderFigures <- paste0(folder, "/figures/")

# Source functions
for (f in list.files(fold_bib))
  source(paste(fold_bib,f,sep=""), encoding = "UTF-8")

# Load packages
invisible(lapply(c(
  "MultiMoMo", # Please use the modified version of the package provided for this paper
  "HMDHFDplus",
  "systemfit",
  "lattice",
  "pander",
  "grid",
  "ggplot2",
  "gridExtra",
  "locfit",
  "scales",
  "lubridate",
  "splines",
  "mgcv",
  "data.table",
  "formattable",
  "RColorBrewer",
  "readxl",
  "kableExtra",
  "gtsummary",
  "dplyr",
  "tidyr",
  "tidyverse",
  "rmarkdown",
  "ggthemes",
  "ggforce",
  "cowplot",
  "ggridges",
  "viridis",
  "hrbrthemes",
  "colorspace",
  "ggbeeswarm",
  "ggfan"),
  instal.import.package))

# parameters
xv <- 0:105
yv <- 1980:2019
countries <- c("FRATNP")
group <- c("Female", "Male", "Total")

# ---- TO COMPLETE ----
username <- "xxxxx"
password <- "yyyyy"
# ---------------------

# Downloading the mortality data

# Get Death and exposure
dtx_fr <- readHMDweb(countries, "Deaths_1x1", username, password, fixup = TRUE)
etx_fr <- readHMDweb(countries, "Exposures_1x1", username, password, fixup = TRUE)

# Transform to matrix
dtx_list <- lapply(group, function(x) {matrix_mort_data(dtx_fr, yv, xv, x)})
names(dtx_list) <- group
etx_list <- lapply(group, function(x) {matrix_mort_data(etx_fr, yv, xv, x)})
names(etx_list) <- group

# Save date
save(dtx_fr, file =  paste0(fold_data, "/dtx_fr.RData"))
save(dtx_list, file =  paste0(fold_data, "/dtx_list.RData"))
save(etx_list, file =  paste0(fold_data, "/etx_list.RData"))



