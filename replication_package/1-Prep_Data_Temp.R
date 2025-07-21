#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-06-20
#####################################################################################################
#' @Information :
#' This script extracts and formats climate scenario data from GHCN.
#' It produces the dataset of daily temperature "temp_journaliere_indicateur.RData.RData".
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

# Load data - Data GHCN 1950 to 2020
temp <- read_excel(
  paste0(fold_data_raw, "/GHCN/GHCN_1950-2020.xlsx"),
  col_types = c("text", "text", "date", "numeric", "numeric", "numeric")
)

# Preprocess Temperature Data

# Fill in missing TAVG values by averaging TMIN and TMAX
row_miss <- which(is.na(temp$TAVG))
temp$TAVG[row_miss]<-(temp$TMIN[row_miss] + temp$TMAX[row_miss])/2

# Convert to data.table
bd <- data.table(temp)
# Convert the DATE column to Date format
bd$DATE <- ymd(bd$DATE)
bd$YEARS <- year(bd$DATE)
bd$MONTHS <- month(bd$DATE)
bd$DAY <- day(bd$DATE)

# Aggregate temperature across 14 French stations
bd_france4 <- bd[, .(TAVG = mean(TAVG)), by = .(DATE, YEARS, MONTHS, DAY)]

# Filter data to keep only the years up to 2019
bd_france4 <- bd_france4 %>%
  dplyr::filter(YEARS <= 2019)

# Variable used in an older version - Forced to 1
bd_france4$Ind_canicule <- 1

# Save daily temperature data  with object: bd_france4
save(bd_france4,
     file = paste0(fold_data,"temp_journaliere_indicateur.RData"))

