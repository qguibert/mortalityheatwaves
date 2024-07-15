########################################
#' fit_dlnm
#' Function for fitting dlnm models per age bucket or for all the population
#' @param data a list of daily dataset with number of deaths and covariates, dates
#' and age buckets.
#' @param param_dlnm a set of parameters to fit the DLMN model, see `dlnm`-package
#' @param per_age indicates if the model if one model is fittted per age bucket
#' or not
#' @param summer the model can be fit only on summer period (`summer = T`) or for the whole year
#' (`summer = F`)
#' @param psi imposes the MMT or not. If `NULL`, the MMT is calibrated.
#' @param ci.level defines the confidence interval level.
########################################

fit_dlnm <- function(data, param_dlnm, per_age = T, summer = F, psi = NULL,
                     ci.level = 0.95)
{
  # Create variables yday (day of the year), dow (day of the week), and year
  data <-  data %>%
    mutate(yday = yday(datedec),
           dow = substr(weekdays(datedec), 1, 3),
           year = year(datedec))

  # Function for fitting the DLNM model
  my_fit <- function(df)
  {
    # Set argvar, arglag lists, and crossbasis depending on whether it's summer
    # or whole year
    if(summer)
      # Summer period
    {
      argvar <- list(
        fun = param_dlnm$varfun,
        knots = quantile(df$tavg, param_dlnm$varper / 100, na.rm = T),
        Bound = range(df$tavg, na.rm = T)
      )
      arglag <- list(knots = logknots(param_dlnm$lag, param_dlnm$lagnk))

      #  summer periods are discontinuous so data should be grouped beforehand
      #  by creating a variable `indsummer`
      cb <- crossbasis(df$tavg, lag = param_dlnm$lag, argvar = argvar,
                       arglag = arglag, group = df$indsummer)

      # Model formula for summer period
      formula <- nb_deaths ~ cb + dow + ns(yday, df = param_dlnm$dfseas):factor(year) +
        ns(datedec, df = round(length(unique(year)) / param_dlnm$dftrend / 10))
    } else
      # Whole year
    {
      argvar <- list(
        fun = param_dlnm$varfun,
        knots = quantile(df$tavg, param_dlnm$varper / 100, na.rm=T),
        degree = param_dlnm$vardegree
      )
      arglag <- list(knots = logknots(param_dlnm$lag, param_dlnm$lagnk))

      cb <- crossbasis(df$tavg,lag = param_dlnm$lag, argvar = argvar,
                       arglag = arglag)
      # Model formula
      formula  <- nb_deaths ~ cb + dow + ns(datedec, df = param_dlnm$dfseas *
                                              length(unique(year)))

    }

    # Run the GLM model and prediction
    model <- glm(formula, data = df, family = quasipoisson, na.action = "na.exclude")
    # calibrate the MMT, which corresponds to the temperature of minimum
    # mortality.

    # Calibrate the minimum mortality temperature (mm) for estimating relative
    #  risk
    if(is.null(psi))
    {
      # Provisional centering point to have initial prediction
      cen <- mean(df$tavg, na.rm = T)
    } else
    {
      cen <- psi
    }

    # Reduction to overall cumulative
    red <- crossreduce(cb, model, cen = cen)
    coef <- coef(red)
    vcov <- vcov(red)

    # Define minimum mortality values: exclude low and very hot temperatures
    if(is.null(psi))
    {
      predvar <- quantile(df$tavg,1:99/100,na.rm=T)
      mmt_argvar <- argvar
      mmt_argvar$x = predvar
      bvar <- do.call(onebasis, mmt_argvar)
      cen <- (1:99)[which.min((bvar %*% coef))]
      cen <- quantile(df$tavg, cen/100, na.rm=T)
    } else
    {
      cen <- psi
    }

    # Prediction
    pred <- crosspred(cb, model, cen= cen, by=0.1, ci.level = ci.level)

    return(list(
      cb = cb,
      lag = param_dlnm$lag,
      argvar = argvar,
      arglag = arglag,
      model = model,
      pred = pred,
      cen = cen,
      coef = coef,
      vcov = vcov
    ))
  }

  # Run process
  if(summer)
  {
    # Number of summers for grouping data as summer period are discontinuous
    data$indsummer <- data$year - min(data$year) + 1
  }

  if(per_age)
  {
    list_age <- unique(data$age_bk)
    # Run DLNM model for each age range
    list_model <- lapply(list_age, function(age){
      # Select data
      df <- data[which(age_bk == paste0(age)), ]
      my_fit(df)
    })
    names(list_model) <- list_age
  } else
  {
    # Run DLNM model for all ages
    list_model <- list(
      "all_ages" = my_fit(data)
    )
  }
  return(list_model)
}
