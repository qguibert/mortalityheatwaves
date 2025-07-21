#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-07-01
#####################################################################################################
#' @Information :
#' This script installs all dependencies locally, and should be run once.
#####################################################################################################

rm(list=ls(all=TRUE))

##############################################################################
##- Function for installing and loading R packages:
instal.import.package <- function(package){
  if(!(package %in% installed.packages()[,1])) install.packages(package)
  if(!(paste0("package:", package) %in% search())) library(package, character.only=TRUE, quietly = T)
}
##############################################################################

# Folds
folder <- getwd() # If necessary, please set your working directory
fold_data <- paste0(folder, "/data/")
fold_data_raw <- paste0(folder, "/data_raw/")
fold_bib <- paste0(folder, "/functions/")
folderTables <- paste0(folder, "/tables/")
folderFigures <- paste0(folder, "/figures/")

# Load packages
invisible(lapply(c(
  "MultiMoMo",
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
  "dlnm",
  "MASS",
  "mvtnorm",
  "Rfast",
  "parallel",
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
  "furrr",
  "purrr",
  "ggfan",
  "ggpubr"),
  instal.import.package))


# Source functions
for (f in list.files(fold_bib))
  source(paste(fold_bib,f,sep=""), encoding = "UTF-8")

# Path to the tar.gz source package file of Multimomo
package_file <- paste0(folder, "/multimomo/MultiMoMo_0.0.0.9000.tar.gz")

# Install the Multimomo package from source
install.packages(package_file, repos = NULL, type = "source")

# In case of an error, you can manually install the package. To do this,
# after installing the devtools package:
# 1. Open RStudio by opening the 'MultiMoMo.Rproj' project file located in the 'multimomo' directory.
# 2. In the 'Build' tab, click on 'Clean and Install'.
# You can also clck on 'Build Source Pacakge' to recreate the file named 'MultiMoMo_0.0.0.9000.tar.gz'

