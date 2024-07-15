matrix_mort_data <- function(dt, yv, xv, var_str){
  temp <- dt %>%
    rename_with(~ c("var"), all_of(var_str)) %>%
    rename_with(tolower) %>%
    dplyr::filter(year %in% yv, age %in% xv) %>%
    dplyr::select(year, age, var) %>%
    pivot_wider(names_from = age, values_from = var)
  # convert to matrix
  temp <- as.matrix(temp)
  temp <- temp[, -1]
  dimnames(temp) <- list(yv, xv)
  return(temp)
}

#' @keywords internal
### Define weight matrix
w_matrix <- function(mat)
{
  temp <- matrix(data = 1, ncol = dim(mat)[2], nrow = dim(mat)[1])
  ind <- which(is.na(mat), arr.ind = TRUE)
  temp[ind] <- 0
  dimnames(temp) <- dimnames(mat)
  return(mat)
}



