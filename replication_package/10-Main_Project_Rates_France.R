#####################################################################################################
#' @author : Quentin GUIBERT
#' @date : 2025-06-03
#####################################################################################################
#####################################################################################################
#' @Information :
#' Forecast mortality rates with temperature effects for Metropolitan France
#####################################################################################################

rm(list=ls(all=TRUE))

#----- PARAMETERS TO CHANGE BY THE REPLICATOR ----------------------------------
# .libPaths("/home/quentinguibert/R/x86_64-pc-linux-gnu-library/4.2") # If necessary, please define the .libPaths
folder <- getwd() # If necessary, please set your working directory
#-------------------------------------------------------------------------------

# Folds
loc_folder <- "adj_proj_rates"
fold_data <- paste0(folder, "/data/")
fold_temp_results <- paste0(folder, "/data/", loc_folder)
fold_data_raw <- paste0(folder, "/data_raw/DRIAS/")
fold_bib <- paste0(folder, "/functions/")
folderTables <- paste0(folder, "/tables/")
folderFigures <- paste0(folder, "/figures/")

# Source functions
for (f in list.files(fold_bib))
  source(paste(fold_bib,f,sep=""), encoding = "UTF-8")

# Load packages
invisible(lapply(c(
  "MASS",
  "readxl",
  "lubridate",
  "systemfit",
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
  "ggpubr"),
  instal.import.package))
options(dplyr.summarise.inform = FALSE)

#----------------------------------------------------------
#theme graphique
theme_set(theme_bw())
trellis.device(color = FALSE)

# Parameters definition
#---------------------------------------------------------------------------------------------------
# General
list_sexe <- list(m = 1,f = 2)
age_breaks <- c(0, 64, 74, 84, Inf)
age_labels <- c("0-64", "65-74", "75-84", "85+")

# training period
annee_deb <- 1980
annee_fin <- 2019

# forecasting period
start_year <- 2020
end_year <- 2100
year_breaks <- c(1980, 1989, 1999, 2009, 2019, 2029, 2039, 2049, 2059, 2069, 2079, 2089, 2100)
year_labels <- c("1980s", "1990s", "2000s", "2010s", "2020s", "2030s", "2040s",
                 "2050s", "2060s", "2070s", "2080s","2090s")

# climate parameters
# list model
info_model <- read_excel("data/info_modeles.xlsx")

# heatwave parameter
min_level <- 17
max_level <- 30

# Monte-Carlo simulations
nsim <- 20L # number of simulations
parallel <- "snow" # parallel option
ncpus <- 3L # number of cores

# other parameters
AGE_MAX <- 105

## Forecasting Parameters
arima_spec <- list(K.t_M = "RWD", k.t_M = "AR1.1", k.t_F = "AR1.1")
n_ahead    <- length(start_year:end_year)
n_sim      <- nsim
est_method <- "PORT"
#---------------------------------------------------------------------------------------------------
# Load data and previous models
load(file =  paste0(fold_data, "/base_nb_deces.RData"))

# Quantiles for extreme hot and cold
q025 <- quantile(base_nb_deces$m$TAVG, 0.025)
q975 <- quantile(base_nb_deces$m$TAVG, 0.975)
#---------------------------------------------------------------------------------------------------
# Load Climate models
clim_model <- lapply(1:nrow(info_model), function(i)
{
  print(paste0(info_model$Id[i],"- RCP", info_model$RCP[i]))
  treat_rcp(info_model$Id[i], info_model$RCP[i], info_model,
            min_level, max_level, start_year, end_year,
            new_calcul= FALSE, city = FALSE, fold_data, fold_data_raw)
})
clim_model <- do.call("rbind", clim_model)

#---------------------------------------------------------------------------------------------------
# Load other models
## DLNM model
load(file =  paste0(fold_data, "/fit_dlnm.RData"))
load(file =  paste0(fold_data, "/xs_mort_central.RData"))

## Multi-mortality models
load(file =  paste0(fold_data, "/mort_dt.RData"))
load(file =  paste0(fold_data, "/adj_mort_dt.RData"))
load(file =  paste0(fold_data, "/fit_M.RData"))
load(file =  paste0(fold_data, "/fit_F.RData"))
load(file =  paste0(fold_data, "/adj_fit_F.RData"))
load(file =  paste0(fold_data, "/adj_fit_M.RData"))
#---------------------------------------------------------------------------------------------------
# extract the last year 2019, for calculate exposure to risk
expo <- do.call("rbind", lapply(mort_dt$UNI, function(dt){
  dt <- dt$dtx[nrow(dt$dtx),  ]
  return(data.frame(age = 0:105,
                    w = dt,
                    age_bk = c(rep("0-64", 65), rep("65-74", 10), rep("75-84", 10), rep("85+", 21))
  ))
}))
expo$gender <- c(rep("f", 106), rep("m", 106))
# Agregate expo per age bucket
W <- sum(expo$w)
expo <- expo %>%
  group_by(age_bk, gender) %>%
  summarise(w = sum(w) / W)

# compute this weight as an average over the last five years.
un_an_weight <- lapply(names(xs_mort), function(x){
  # Add day and months
  # Filter on the last five years
  res <- xs_mort[[x]]$excess_mort %>%
    mutate(day = day(DateDec),
           month = month(DateDec)) %>%
    filter(YEARS %in% c(2015:2019), temp_effect == "all_effect") %>%
    mutate(un_an = cases - an) %>%
    dplyr::select(YEARS, age_bk, un_an, day, month) %>%
    group_by(age_bk, day, month) %>%
    summarise(un_an = sum(un_an))

  # Calculate the denominator of the weight
  xs_mort_agg <- res %>%
    group_by(age_bk) %>%
    summarise(un_an_sum = sum(un_an))

  res <- res %>%
    left_join(xs_mort_agg, by = "age_bk") %>%
    mutate(weight = un_an / un_an_sum) %>%
    mutate(un_an = NULL,
           un_an_sum = NULL)
  # Correct 29-02
  res$weight[res$day==29 & res$month==2] <- 5 *
    res$weight[res$day==29 & res$month==2]

  return(res)
})
names(un_an_weight) <- names(xs_mort)

#---------------------------------------------------------------------------------------------------
# Project Future mortality

## project parameters
set.seed(1234)
proj_par <- project_parameters(fit_M, fit_F, n_ahead, n_sim, arima_spec, est_method)
set.seed(1234)
adj_proj_par <- project_parameters(adj_fit_M, adj_fit_F, n_ahead, n_sim, arima_spec, est_method)

## project mortaltity rates
proj_rates <- project_mortality_rates(fit_M, fit_F, proj_par)
adj_proj_rates <- project_mortality_rates(adj_fit_M, adj_fit_F, adj_proj_par)

# reformat data
format_rates <- function(rates)
{
  rates <- melt(rates)
  names(rates) <- c("age", "year", "sim", "Qxt")
  rates <- rates %>%
    mutate(age_bk = cut(age, age_breaks, age_labels, include.lowest = T))
}
proj_rates <- lapply(proj_rates, format_rates)
names(proj_rates) <- c("m", "f")

adj_proj_rates <- lapply(adj_proj_rates, format_rates)
names(adj_proj_rates) <- c("m", "f")

save(proj_rates, file =  paste0(fold_data, "/proj_rates.RData"))
save(adj_proj_rates, file =  paste0(fold_data, "/adj_proj_rates.RData"))
#---------------------------------------------------------------------------------------------------
# Running simulations

## get mortality simulations
list_model <- unique(clim_model$model)
data_mort <- adj_proj_rates

# change names dates
clim_model <- clim_model %>% dplyr::rename(DateDec = DATE)

# Begin clock
start_time <- Sys.time()
mort_simu <- lapply(list_model, function(i)
{
  # select model
  temp <- dplyr::filter(clim_model, model == i)
  list_sc <- unique(temp$rcp)

  temp_traj <- lapply(list_sc, function(j)
  {
    # select scenario on projection years
    sc <- dplyr::filter(temp, rcp == j,
                        YEARS %in% start_year:end_year)
    # run simulations for males and females
    res <- lapply(names(data_mort), function(x)
    {
      # select population
      temp_dem <- data_mort[[x]] %>%
        filter(year %in% start_year:end_year)
      # select weight
      temp_weight <- un_an_weight[[x]]
      # launch forecasting along the rcp scenario with simulations and parallelization
      f_mort <- forecast_mort_dlnm(temp_dem, sc, fit[[x]], temp_weight,
                                   q_range = c(q025, q975),
                                   sensi_cen = 0,
                                   nsim = nsim, parallel = parallel, ncpus = ncpus)
      # Add rcp and gender indexes
      f_mort <- lapply(f_mort, function(tt){
        if(is.null(tt))
        {
          return(tt)
        } else{
          return(
            tt %>%
              dplyr::mutate(traj_clim = i, rcp = j, gender = x) %>%
              relocate(traj_clim, rcp, gender, temp_effect, .before =  sim)
          )
        }
      })
      return(f_mort)
    })
    # Merge more than two lists with the same element name
    res <- Reduce(function(...) Map("rbind", ...), res)
    return(res)
  })
  temp_traj <- Reduce(function(...) Map("rbind", ...), temp_traj)
  # Save
  save(temp_traj, file = paste0(fold_temp_results, "/dlnm_simu_", i,".RData"))
  return(NULL)
})
# mort_simu <- Reduce(function(...) Map("rbind", ...), mort_simu)

# End clock and report ti
end_time <- Sys.time()
time_diff <- end_time - start_time
print(paste("Runtime:", as.numeric(time_diff, units = "mins"), "mins"))

#---------------------------------------------------------------------------------------------------
# Generate table of results

## Collect output of climate scenarios

list_model <- unique(clim_model$model)

# Tab with life expenctancy
etab <- lapply(list_model, function(x){
  get(load( file = paste0(fold_temp_results, "/dlnm_simu_", x,".RData")))$tab_ex
})
etab <- do.call("rbind", etab)
save(etab, file = paste0(fold_temp_results, "/etab.RData"))

# Tab with attributable effects
afftab <- lapply(list_model, function(x){
  get(load( file = paste0(fold_temp_results, "/dlnm_simu_", x,".RData")))$tab_excess
})
afftab <- do.call("rbind", afftab)
save(afftab, file = paste0(fold_temp_results, "/afftab.RData"))


