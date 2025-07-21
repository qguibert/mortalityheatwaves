###################################
#' import_data_death: Import and preprocess daily mortality and temperature data
#' This function loads and merges daily mortality data with temperature and indicator data.
#' It filters by sex and year, excludes specified outlier years, aggregates deaths by date
#' and age group, and enriches the dataset with time, weekday, and month variables.
#' @param sexe Character. Sex to filter on.
#' @param annee_deb Integer. Start year of the period to include.
#' @param annee_fin Integer. End year of the period to include.
#' @param outlier Integer vector. Years to exclude due to anomalies (e.g., extreme heatwaves).
#'
#' @return A data.table with daily mortality counts by age group, merged with temperature data.
#' These files must be located in the directory specified by the global variable `fold_data`.
###################################

import_data_death <- function(sexe, annee_deb, annee_fin, outlier)
{
  # Load mortality data with object: base
  load(paste0(fold_data,"/data_deces.RData"))

  # Load daily temperature data and indicators (1950–2019) with object: bd_france4
  load(paste0(fold_data,"/temp_journaliere_indicateur.RData"))

  # Define the vector of years to retain
  select_annee <- seq(from = annee_deb, to = annee_fin, by = 1)

  # Filter and aggregate death data by date and age group
  base.aggreg <- base[
    SEXE == sexe &
      year(DateDec) %in% select_annee &
      (!(year(DateDec) %in% outlier)),
    .(Nombre_de_deces = sum(Nombre_de_deces)),
    by = .(DateDec, GROUPAGE3)
  ]

  # Add sex as a column
  base.aggreg$sexe <- sexe

  # Merge with temperature and indicator data by date
  base.merge <- merge(x = base.aggreg, y = bd_france4,
                      by.x = "DateDec", by.y = "DATE")

  # Initialize time column
  base.merge$time <- 0

  # For each age group, create a sequential time index
  for (age in unique(base.merge$GROUPAGE3)){
    base.merge[GROUPAGE3 == age,]$time <- seq(1:length(base.merge[GROUPAGE3 == age,]$time))
  }

  # Add day of the week
  base.merge$dow <- wday(base.merge$DateDec)

  # Add month number
  base.merge$month <- month(base.merge$DateDec)

  # Check that no outlier years remain (should be empty if filtering worked)
  if(nrow(base.merge[year(DateDec) %in% outlier,])!=0)
    stop("Years with outliers are not properly excluded")

  # Rename age group column for clarity
  base.merge <- base.merge %>% rename(age_bk = GROUPAGE3)

  return(base.merge)
}




