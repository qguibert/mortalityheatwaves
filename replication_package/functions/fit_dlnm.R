########################################
#' fit_dlnm: function for fitting dlnm models per age bucket or for all the population
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

fit_dlnm <- function(data, param_dlnm, per_age = T, summer = F, psi = NULL, ci.level = 0.95)
{
  # create variables
  data <-  data %>%
    mutate(yday = yday(DateDec),
           dow = substr(weekdays(DateDec), 1, 3),
           year = year(DateDec))

  # function for fitting the model
  my_fit <- function(df)
  {
    # set argvar, arglag list, and crossbasis
    if(summer) # summer period
    {
      argvar <- list(
        fun = param_dlnm$varfun,
        knots = quantile(df$TAVG, param_dlnm$varper / 100, na.rm = T),
        Bound = range(df$TAVG, na.rm = T)
      )
      arglag <- list(knots = logknots(param_dlnm$lag, param_dlnm$lagnk))

      cb <- crossbasis(df$TAVG, lag = param_dlnm$lag, argvar = argvar, arglag = arglag,
                       group = df$indsummer) #  summer periods are discontinuous so data should be grouped

      ## model formula
      formula <- Nombre_de_deces ~ cb + dow + ns(yday, df = param_dlnm$dfseas):factor(year) +
        ns(DateDec, df = round(length(unique(year)) / param_dlnm$dftrend / 10))
    } else # whole year
    {
      argvar <- list(
        fun = param_dlnm$varfun,
        knots = quantile(df$TAVG, param_dlnm$varper / 100, na.rm=T),
        degree = param_dlnm$vardegree
      )
      arglag <- list(knots = logknots(param_dlnm$lag, param_dlnm$lagnk))

      cb <- crossbasis(df$TAVG,lag = param_dlnm$lag, argvar = argvar, arglag = arglag)
      # model formula
      formula  <- Nombre_de_deces ~ cb + dow + ns(DateDec, df=param_dlnm$dfseas*length(unique(year)))

    }

    # run the model and prediction
    model <- glm(formula, data = df, family = quasipoisson, na.action = "na.exclude", x = TRUE)
    # calibrate the mm, which corresponds to the temperature of minimum mortality, which will be used as
    #    as reference to estimate relative risks and as temperature threshold
    #    to differentiate the contribution of heat and cold to the total mortality
    #    attributable to non-optimal temperatures.
    if(is.null(psi))
    {
      # provisional centering point to have initial prediction for chosing the mmt
      cen <- mean(df$TAVG, na.rm = T)
      # cp <- crosspred(cb, model ,cen = cen, by = 0.1)
      # cen <- cp$predvar[which.min(cp$allRRfit)]
    } else
    {
      cen <- psi
    }

    # reduction to overall cumulative
    red <- crossreduce(cb, model, cen = cen)
    coef <- coef(red)
    vcov <- vcov(red)

    # DEFINE MINIMUM MORTALITY VALUES: EXCLUDE LOW AND VERY HOT TEMPERATURE
    if(is.null(psi))
    {
      predvar <- quantile(df$TAVG,1:99/100,na.rm=T)
      mmt_argvar <- argvar
      mmt_argvar$x = predvar
      bvar <- do.call(onebasis, mmt_argvar)
      cen <- (1:99)[which.min((bvar %*% coef))]
      cen <- quantile(df$TAVG, cen/100, na.rm=T)
    } else
    {
      cen <- psi
    }

    # prediction
    pred <- crosspred(cb, model, cen=cen, by=0.1, ci.level = ci.level)

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

  # run process
  if(summer)
  {
    # number of summer for grouping data as summer period are discontinuous
    data$indsummer <- data$year - min(data$year) + 1
  }

  if(per_age)
  {
    list_age <- unique(data$age_bk)
    # Run dlnm model for each age range
    list_model <- lapply(list_age, function(age){
      # select data
      df <- data[which(age_bk == paste0(age)), ]
      my_fit(df)
    })
    names(list_model) <- list_age
  } else
  {
    # Run dlnm for all ages
    list_model <- list(
      "all_ages" = my_fit(data)
    )
  }
  return(list_model)
}


# Q-AIC FUNCTION
# source : https://github.com/gasparrini/2017_gasparrini_Biomet_Rcodedata/blob/master
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

fqbic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  n <- length(model$fitted.values)
  qbic <- -2*loglik + log(n)*summary(model)$df[3]*phi
  return(qbic)
}

