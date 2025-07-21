#####################################################################################################
#' treat_rcp: process climate scenario data and generate heatwave indicators
#' This function loads and processes DRIAS climate data to produce a data table enriched with
#' heatwave indicators (e.g., threshold exceedances, consecutive days, severity) and other
#' meteorological summaries. It either recalculates these indicators from raw climate files
#' or loads pre-processed `.RData` files.
#####################################################################################################

treat_rcp <- function(model, scenario, info_modeles,
                      seuil_min, seuil_max, start_year, end_year,
                      new_calcul = FALSE, city = FALSE, path, path_raw)
{
  # Extract available model and scenario lists
  list_model <- unique(info_modeles$Id)
  names(list_model) <- seq(from = 1, to = 12, by = 1)
  list_scenario <- unique(info_modeles$RCP)

  # Input validation
  if( !(model %in% list_model))
  {
    stop("The model doesn't exist or is incorrectly filled in, see the list of effective models")
  }

  if( !(scenario %in% list_scenario))
  {
    stop("The scenario doesn't exist or is incorrectly filled in, see the list of effective scenario")
  }

  if( !(scenario %in% info_modeles[info_modeles$Id == model,]$RCP))
  {
    stop("The scenario doesn't exist for this model")
  }

  # Load preprocessed file if requested
  if(new_calcul == FALSE)
  {
    if(city == FALSE)
    {
      load(file = paste0(path, "climate_trajectory/",model,"_RCP", scenario,".RData"))
    } else
    {
      load(file = paste0(path, "climate_trajectory_city/",model,"_RCP", scenario, ".RData"))
    }
    return(data)
  } else {
    # Otherwise, load raw climate data and process it
    # Define fold name
    fold_data_model <- paste0(
      path_raw,
      info_modeles[info_modeles$Id == model & info_modeles$RCP == scenario ,]$Num," - ",
      info_modeles[info_modeles$Id == model & info_modeles$RCP == scenario,]$Id,"/RCP",
      info_modeles[info_modeles$Id == model & info_modeles$RCP == scenario,]$RCP
    )

    # Load data
    temp <- data.table(read.table(paste0(fold_data_model,"/", model, "_rcp",scenario,
                                         ".txt"),
                                  sep = " ", header = F, dec = '.'))
    names(temp) <- c("DATE","LAT","LONG","TMIN","TMAX","TAVG")

    # Convert and extract date components
    temp$DATE <- ymd(temp$DATE)
    temp$YEARS <- year(temp$DATE)

    # Filter years of interest
    select_period <- seq(from = start_year, to = end_year, by = 1)

    # Agregate by city if requested
    if(city)
    {
      # Aggregate by location and date
      bd_france <- temp[YEARS %in% select_period,
                        .(TMAX = mean(TMAX),
                          TMIN = mean(TMIN),
                          TAVG = mean(TAVG)),
                        by = .(YEARS, DATE, LAT, LONG)]

      # 3-day rolling average for each location
      bd_france <- bd_france %>%
        group_by(LAT, LONG) %>%
        mutate(
          TAVG3 = zoo::rollapply(TAVG, 3, mean, align ='right', fill = 0),
          TMIN3 = zoo::rollapply(TMIN, 3, mean, align ='right', fill = 0),
          TMAX3 = zoo::rollapply(TMAX, 3, mean, align ='right', fill = 0)
        )
    } else
    {
      # Aggregate nationally
      bd_france <- temp[YEARS %in% select_period,
                        .(TMAX = mean(TMAX),
                          TMIN = mean(TMIN),
                          TAVG = mean(TAVG)),
                        by = .(YEARS, DATE)]

      # 3-day rolling averages
      bd_france$TAVG3 <- zoo::rollapply(bd_france$TAVG, 3, mean, align ='right', fill = 0)
      bd_france$TMIN3 <- zoo::rollapply(bd_france$TMIN, 3, mean, align ='right', fill = 0)
      bd_france$TMAX3 <- zoo::rollapply(bd_france$TMAX, 3, mean, align ='right', fill = 0)
    }

    # detect and add heatwave indicators

    bd_france3 <- bd_france %>%
      mutate(Ind_canicule = ifelse(TMAX3 >= seuil_max & TMIN3 >= seuil_min ,1,0),
             Ind_canicule_consecutif = 0,
             Num_vague = 0,
             Severite_canicule = ifelse(TMAX3 >= seuil_max & TMIN3 >= seuil_min,
                                        TMIN3 - seuil_min + TMAX3 - seuil_max, 0)
      )

    # sort by date
    if(city)
    {
      bd_france3 <-   bd_france3[order(bd_france3$LAT, bd_france3$LONG, bd_france3$DATE), ]
    } else {
      bd_france3 <-   bd_france3[order(bd_france3$DATE), ]
    }

    # detect consecutive heatwave days
    for (i in 2:length(bd_france3$TAVG)){
      if (bd_france3$Ind_canicule[i] == 1){
        if (bd_france3$Ind_canicule_consecutif[i-1] == 0){
          bd_france3$Ind_canicule_consecutif[i] <- 1
        }else if(bd_france3$Ind_canicule_consecutif[i-1] > 0){
          bd_france3$Ind_canicule_consecutif[i] <- bd_france3$Ind_canicule_consecutif[i-1] + 1}
      }
    }

    # Number each distinct heatwave event (for national data only)
    if(city == FALSE) # Not usefull by city
    {
      # compute number of each heatwave
      for (years in unique(bd_france3$YEARS)) {
        if (nrow(bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]) != 0){
          bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,][1]$Num_vague <- 1
          if( nrow(bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]) != 1){
            for(i in 2:nrow(bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,])){


              if(bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Ind_canicule_consecutif[i] > bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Ind_canicule_consecutif[i-1]){
                bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Num_vague[i] <- bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Num_vague[i-1]
              }
              else{
                bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Num_vague[i] <- max(bd_france3[bd_france3$YEARS == years & bd_france3$Ind_canicule == 1,]$Num_vague+1)
              }
            }
          }
        }
      }
    }
    # Final save and return
    data <- bd_france3
    data$model <- model
    data$rcp <- scenario

    if(city == FALSE)
    {
      save(data, file = paste0(path, "climate_trajectory/",model,"_RCP",scenario,".RData"))
    } else
    {
      save(data, file = paste0(path, "climate_trajectory_city/",model,"_RCP",scenario,".RData"))
    }
    return(data)
  }
}
