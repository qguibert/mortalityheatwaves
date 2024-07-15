########################################
#' life_exp_ts
#' Function calculating periodic life expectancy
#' @param a data.frame containing variables `age` and `Qxt` and/or
#'  `Qxt_xs`
#' @param calc_xs is TRUE is periodic life expectancy should be calculated based on
#' variable `Qxt_xs`
########################################
life_exp_ts <- function(data, calc_xs)
{
  rr <- AGE_MAX + 1 - data$age
  if(calc_xs == FALSE)
  {
    # Life expectancy without temperature effects
    data$Ext <- wapply(data$Qxt, rr)
  } else
  {
    # Life expectancy with temperature effects
    data$Ext_xs <- wapply(data$Qxt_xs, rr)
  }
  return(data)
}

########################################
# functions for calculating efficiently periodic life expectancy
########################################
calc_period_ext <- function(xx) {sum(cumprod(1 - xx)) + 0.5}

# alternative to rollapply
wapply <- function(x, rr)
{
  lenX <- length(x)
  SEQ1 <- 1:lenX
  SEQ2 <- lapply(SEQ1, function(y) y:(y + rr[y] - 1))
  OUT <- lapply(SEQ2, function(a) calc_period_ext(x[a]))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}
