#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-06-11
#####################################################################################################
#' @Information :
#' This script extracts and formats climate scenario data from INSEE (Quetelet-Progedo).
#' It produces the dataset of daily death counts "data_deces.RData".
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

# period parameters
start_year <- 2006
end_year <- 2100
year_breaks <- c(2000, 2009, 2019, 2029, 2039, 2049, 2059, 2069, 2079, 2089, 2100)
year_labels <- c("2000s", "2010s", "2020s", "2030s", "2040s",
                 "2050s", "2060s", "2070s", "2080s","2090s")
# heatwave parameter
min_level <- 17
max_level <- 30

#  ---------- Load and prepare data ----------
## Load mortality data
data_deces_raw <- read.csv(paste0(fold_data_raw, "/PROGEDO/tableau_2.csv"), sep = "\t")

## Prepare data
data_deces <- data_deces_raw %>%
  as.data.table() %>%
  mutate(DateDec = ymd(paste(ADEC, MDEC, JDEC, sep = "-" ))) %>%
  select(DateDec, ADEC, SEXE, GROUPAGE, Nombre_de_deces) %>%
  arrange(DateDec)

data_deces$GROUPAGE3 <- ifelse(
  data_deces$GROUPAGE < 50, "0-49", ifelse(
    data_deces$GROUPAGE < 55, "50-54", ifelse(
      data_deces$GROUPAGE < 60, "55-59", ifelse(
        data_deces$GROUPAGE < 65, "60-64", ifelse(
          data_deces$GROUPAGE < 70, "65-69", ifelse(
            data_deces$GROUPAGE < 75, "70-74", ifelse(
              data_deces$GROUPAGE < 80,  "75-79", ifelse(
                data_deces$GROUPAGE < 85, "80-84", ifelse(
                  data_deces$GROUPAGE < 90, "85-89", "90 et +")
              ))))))))

# Aggregate data
data_deces <- data_deces[,
                         .(Nombre_de_deces = sum(Nombre_de_deces)),
                         by = .(DateDec, ADEC, SEXE, GROUPAGE3)
]

# arrange data
data_deces <- setorder(data_deces, ADEC,  GROUPAGE3, DateDec, SEXE)
base <- data_deces[, (c("ADEC", "SEXE", "Nombre_de_deces")) := lapply(.SD, as.numeric),
                         .SDcols = c("ADEC", "SEXE", "Nombre_de_deces")]
# save data
save(base, file = paste0(fold_data,"data_deces.RData"))
