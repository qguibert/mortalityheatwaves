#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-05-26
#####################################################################################################
#' @Information :
#' This script extracts and formats climate scenario data from DRIAS.
#' It produces two datasets: the first for Metropolitan France, stored in the
#' '/data/climate_trajectory' directory, and the second at the city level,
#' stored in '/data/climate_trajectory_city'.
#' The generated '.RData' files are named using the following format:
#' [Climate_Model_Name]_[RCP_Number]
#####################################################################################################

rm(list=ls(all=TRUE))

# Folds
folder <- getwd() # If necessary, please set your working directory
fold_data <- paste0(folder, "/data/")
fold_data_raw <- paste0(folder, "/data_raw/DRIAS/")
fold_bib <- paste0(folder, "/functions/")
folderTables <- paste0(folder, "/tables/")
folderFigures <- paste0(folder, "/figures/")

# Source functions
for (f in list.files(fold_bib))
  source(paste(fold_bib,f,sep=""), encoding = "UTF-8")

# Load packages
invisible(lapply(c(
  "pander",
  "lattice",
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

# list model
info_model <- read_excel("data/info_modeles.xlsx")

# period parameters
start_year <- 2006
end_year <- 2100
year_breaks <- c(2000, 2009, 2019, 2029, 2039, 2049, 2059, 2069, 2079, 2089, 2100)
year_labels <- c("2000s", "2010s", "2020s", "2030s", "2040s",
                 "2050s", "2060s", "2070s", "2080s","2090s")
# heatwave parameter
min_level <- 17
max_level <- 30


#  ---------- Metropolitan France ----------

# extracts and save dataset
model <- lapply(1:nrow(info_model), function(i)
{
  print(paste0(info_model$Id[i],"- RCP", info_model$RCP[i]))
  treat_rcp(info_model$Id[i], info_model$RCP[i], info_model,
            min_level, max_level, start_year, end_year,
            new_calcul= TRUE, city = FALSE, fold_data, fold_data_raw)
})

#  ---------- City ----------
# extracts and save dataset

model_city <- lapply(1:nrow(info_model), function(i)
{
  print(paste0(info_model$Id[i],"- RCP", info_model$RCP[i]))
  treat_rcp(info_model$Id[i], info_model$RCP[i], info_model,
            min_level, max_level, start_year, end_year,
            new_calcul= TRUE, city = TRUE, fold_data, fold_data_raw)
})
model_city <- do.call("rbind", model_city)

