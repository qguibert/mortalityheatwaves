#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-06-23
#####################################################################################################
#' @Information :
#' This script generates results of the sensitivity analysis conducted in Appendix C.3.1.
#' of the paper.
#####################################################################################################

rm(list=ls(all=TRUE))

## Folds
folder <- getwd() # If necessary, please set your working directory
fold_data <- paste0(folder, "/data/")
fold_data_raw <- paste0(folder, "/data_raw/")
fold_bib <- paste0(folder, "/functions/")
folderTables <- paste0(folder, "/tables/")
folderFigures <- paste0(folder, "/figures/")

# Load functions
# Source functions
for (f in list.files(fold_bib))
  source(paste(fold_bib,f,sep=""), encoding = "UTF-8")

# Load packages
invisible(lapply(c(
  "lattice",
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
  "dplyr",
  "tidyr",
  "tidyverse",
  "rmarkdown",
  "ggthemes",
  "cowplot",
  "ggridges",
  "viridis",
  "hrbrthemes",
  "colorspace",
  "ggbeeswarm",
  "furrr",
  "ggfan"),
  instal.import.package))

# Parameters definition

start_time <- Sys.time()

## Main parameters
list_sexe <- list(m = 1,f = 2)
age_breaks <- c(0, 64, 74, 84, Inf)
age_labels <- c("0-64", "65-74", "75-84", "85+")

## training period
annee_deb <- 1980
annee_fin <- 2019
annee_in_sample <- 1980:2015
annee_out_sample <- 2016:2019
outlier <- 999 # exclude no atypical year

# other parameters
AGE_MAX <- 108

# Load data series
base_nb_deces <- lapply(list_sexe, function(s){
  import_data_death(s, annee_deb, annee_fin, outlier)
})

base_nb_deces <- lapply(base_nb_deces, function(data){
  data$age_bk <- ifelse(
    data$age_bk == "0-49", "0-64", ifelse(
      data$age_bk == "50-54", "0-64", ifelse(
        data$age_bk == "55-59", "0-64", ifelse(
          data$age_bk == "60-64", "0-64", ifelse(
            data$age_bk == "65-69", "65-74", ifelse(
              data$age_bk == "70-74", "65-74", ifelse(
                data$age_bk == "75-79", "75-84", ifelse(
                  data$age_bk == "80-84", "75-84", ifelse(
                    data$age_bk == "85-89", "85+", ifelse(
                      data$age_bk == "90 et +", "85+", NA
                    ))))))))))

  data <- data %>%
    group_by(DateDec, age_bk) %>%
    summarise(Nombre_de_deces = sum(Nombre_de_deces),
              YEARS = min(YEARS),
              TAVG = min(TAVG),
              Ind_canicule = min(Ind_canicule),
              time = min(time),
              dow = min(dow),
              month = min(month)
    )
  return(as.data.table(data))
})

# --- Generate sensitivity of the DLNM model

# percentiles of the temperature distribution
list_quant_knot <- list(
  c(10, 75, 90)
)

# number of knots for the lag dimension
list_nk <- 1:10 # defaut 3
# degree of freedom for seasonality
list_seas <- 4:12 # defaut 8

# df parameters
param_df <- expand_grid(
  quant_knot = list_quant_knot,
  nk = list_nk,
  seas = list_seas
)

# Define sensitity functions
sensi_function <- function(x){

  # Define parameters
  param_dlnm_whole <- list(
    # main model, cubic natural spline with three internal knots in
    #   the 10th, 75th, 90th percentiles of the temperature distribution
    varfun = "bs",
    vardegree = 2,
    varper  = param_df$quant_knot[[x]],
    ## lag function specification
    # Definition of the maximum lag, that is, 21 days
    lag  = 21,
    lagnk  = param_df$nk[[x]],
    ## degree of freedom for seasonality
    dfseas  = param_df$seas[[x]],
    ## degree of freedom for trend
    dftrend = NULL
  )

  # fit
  fit <- lapply(names(base_nb_deces), function(x){
    # Try fit_dlnm avec tryCatch
    res <- tryCatch({
      fit_dlnm(base_nb_deces[[x]], param_dlnm_whole, per_age = T, summer = F, psi = NULL)
    }, error = function(e) {
      return(NULL)
    })

    # If error
    if (is.null(res)) {
      return(data.frame(
        sex = x,
        age_bk = NA,
        qaic = NA,
        qbic = NA
      ))
    }

    # If no problem
    return(
      data.frame(
        sex = x,
        age_bk = names(res),
        qaic = sapply(res, function(obj) {
          # Compute fqaic with tryCatch
          tryCatch({
            fqaic(obj$model)
          }, error = function(e) {
            return(NA)
          })
        }),
        qbic = sapply(res, function(obj) {
          # Compute fqbic with tryCatch
          tryCatch({
            fqbic(obj$model)
          }, error = function(e) {
            return(NA)
          })
        })
      )
    )
  })
  fit <- do.call("rbind", fit)

  return(bind_cols(param_df[x,], fit))
}



# Use paralell
no_cores <- availableCores() - 2
plan(multicore, workers = no_cores)
sensi_results <- map_df(1:nrow(param_df), sensi_function)

# save results
save(sensi_results, file =  paste0(fold_data, "sensi_results.RData"))

end_time <- Sys.time()

# Calculer la durée totale
time_diff <- end_time - start_time

