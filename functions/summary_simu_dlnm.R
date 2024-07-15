##################################
# summary_simu_dlnm
##################################
summary_simu_dlnm <- function(data)
{

  # get names for plots and tables
  name_rcp <- unique(data$tab_excess$rcp)
  name_traj_clim <- unique(data$tab_excess$traj_clim)
  name_gender <- unique(data$tab_excess$gender)
  name_effect <- unique(data$tab_excess$temp_effect)
  name_age <- unique(data$tab_qx$age)

  # step 1 - focus on excess of mortality in number %
  # list of tabs with average excess rate over simulation and models
  temp_xs <- data$tab_excess %>%
    group_by(rcp, gender, temp_effect, year_bk, age_bk) %>%
    summarize(xs_rate = mean(xs_rate))
  # store in a list
  tab_xs <- lapply(name_rcp, function(i)
    {
    temp <- lapply(name_gender, function(j){
      temp_xs %>% dplyr::filter(rcp == i, gender == j) %>%
        pivot_wider(names_from = temp_effect, values_from = c(xs_rate))
    })
    names(temp) <- name_gender
    return(temp)
  })
  names(tab_xs) <- name_rcp

  # list of plots with average excess rate over simulation and models
  xs_mort_rate <- lapply(name_gender, function(g)
  {
    temp <- lapply(name_effect, function(k)
    {
      # filter by gender and temperature effect
      df <- data$tab_excess %>%
        filter(gender == g, temp_effect == k)

      # generate plot
      p <- ggplot(df, aes(x = year_bk, y = xs_rate , fill = rcp, color = rcp)) +
        geom_boxplot(outlier.colour="black", outlier.shape = 16, outlier.size = 1,
                     alpha = 0.5) +
        facet_wrap(~ age_bk,  scales = "free_y") +
        xlab("Year") + ylab("Excess mortality (%)") +
        labs(fill = "") +  labs(color = "") +
        scale_color_manual(values = c("orange", "red")) +
        scale_fill_manual(values = c("orange", "red"))
      return(p)
    })
    names(temp) <- name_effect
    return(temp)
  })
  names(xs_mort_rate) <- name_gender


  # step 2 - focus on excess of mortality in number
  # aggregate over simulation and models
  temp_Dxt_xs <- data$tab_excess %>%
    group_by(rcp, gender, temp_effect, year_bk, age_bk) %>%
    summarize(Dxt_xs = mean(Dxt_xs))
  # store in a list
  tab_Dxt_xs <- lapply(name_rcp, function(i)
  {
    temp <- lapply(name_gender, function(j){
      temp_Dxt_xs %>% dplyr::filter(rcp == i, gender == j) %>%
        pivot_wider(names_from = temp_effect, values_from = c(Dxt_xs))
    })
    names(temp) <- name_gender
    return(temp)
  })
  names(tab_Dxt_xs) <- name_rcp

  # list of plots with average excess rate over simulation and models
  xs_mort_nb <- lapply(name_gender, function(g)
  {
    temp <- lapply(name_effect, function(k)
    {
      # filter by gender and temperature effect
      df <- data$tab_excess %>%
        filter(gender == g, temp_effect == k)

      # generate plot
      p <- ggplot(df, aes(x = year_bk, y = Dxt_xs , fill = rcp, color = rcp)) +
        geom_boxplot(outlier.colour="black", outlier.shape = 16, outlier.size = 1,
                     alpha = 0.5) +
        facet_wrap(~ age_bk,  scales = "free_y") +
        xlab("Year") + ylab("Excess mortality (%)") +
        labs(fill = "") +  labs(color = "") +
        scale_color_manual(values = c("orange", "red")) +
        scale_fill_manual(values = c("orange", "red"))
      return(p)
    })
    names(temp) <- name_effect
    return(temp)
  })
  names(xs_mort_nb) <- name_gender

  # step 3 - focus on the life expectancy
  ## plot fan chart of life expectancy at birth
  xs_ext <- try(lapply(name_gender, function(g)
  {
    temp_ext <- lapply(name_effect, function(k)
    {
      intervals = c(1:18/20,91:99/100) # quantile range
      df <- do.call("rbind", lapply(name_rcp, function(sc)
      {
        temp <- data$tab_ex %>%
          filter(rcp == sc, gender == g, age == 0, temp_effect == k) %>%
          mutate(new_Ext = Ext_virt + Ext_xs) %>%
          dplyr::select(sim, traj_clim, year, new_Ext)
        temp <- calc_quantiles(temp, intervals=intervals, x_var = "year", y_var = "new_Ext")
        temp <- temp %>%
          mutate(rcp = sc) %>%
          relocate(rcp, .before =  x)
      }))

      p <- ggplot(df, aes(x=x, y=y, quantile=quantile)) + geom_fan(intervals=intervals) +
        facet_wrap(~ rcp) +
        scale_fill_distiller(palette="Spectral")+
        ylab("life expectancy with excess of mortality") + xlab("Year") +
        labs(fill = "eCI")
      return(p)
    })
    names(temp_ext) <- name_effect
    return(temp_ext)
  }))
  if(!inherits(xs_ext, "try-error"))
  {
    names(xs_ext) <- name_gender
  } else
  {
    xs_ext <- NULL
  }


  ## plot loss in life expectancy at birth
  loss_ext <- lapply(name_gender, function(g)
  {
    temp_ext <- lapply(name_effect, function(k)
    {
      df <- data$tab_ex %>%
      filter(gender == g, age == 0, temp_effect == k) %>%
      mutate(loss_Ext = Ext_xs) %>%
      group_by(rcp, year) %>%
      summarise(med = median(loss_Ext),
                lwr = quantile(loss_Ext, probs = 0.05),
                upr = quantile(loss_Ext, probs = 0.95)
      )

    p <- ggplot(df, aes(x = year, y = med, fill = rcp)) +
      geom_bar(stat="identity", position = "dodge") +
      geom_pointrange(aes(ymin=lwr, ymax=upr, fill = rcp),
                      position=position_dodge(width=.9)) +
      xlab("Year") + ylab("Life expectancy lost (years)") +
      labs(fill = "") +
      scale_fill_manual(values = c("orange", "red"))
    return(p)
    })
    names(temp_ext) <- name_effect
    return(temp_ext)
  })
  names(loss_ext) <- name_gender


  # step 4 - focus on the qx
  ## plot fan chart of qx at different age
  xs_qxt <- try(lapply(name_gender, function(g)
  {
    temp_ext <- lapply(name_effect, function(k)
    {
      intervals = c(1:18/20,91:99/100) # quantile range
      df <- do.call("rbind", lapply(name_rcp, function(sc)
      {
        do.call("rbind", lapply(name_age, function(a)
        {
          temp <- data$tab_qx %>%
            filter(rcp == sc, gender == g, temp_effect == k, age == a) %>%
            mutate(new_Qxt = Qxt_virt + Qxt_xs) %>%
            dplyr::select(sim, traj_clim, year, new_Qxt)
          temp <- calc_quantiles(temp, intervals=intervals, x_var = "year", y_var = "new_Qxt")
          temp <- temp %>%
            mutate(rcp = sc, age = a) %>%
            relocate(rcp, .before =  x)
        }))
      }))

      p <- ggplot(df, aes(x=x, y=y, quantile=quantile, colour = age)) + geom_fan(intervals=intervals) +
        facet_wrap(~ rcp) +
        scale_fill_distiller(palette="Spectral")+
        ylab("life expectancy with excess of mortality") + xlab("Year") +
        labs(fill = "eCI")
      return(p)
    })
    names(temp_ext) <- name_effect
    return(temp_ext)
  }))
  if(!inherits(xs_qxt, "try-error"))
  {
    names(xs_qxt) <- name_gender
  } else
  {
    xs_qxt <- NULL
  }

  return(list(
    tab_xs = tab_xs,
    tab_Dxt_xs = tab_Dxt_xs,
    xs_mort_rate = xs_mort_rate,
    xs_mort_nb = xs_mort_nb,
    xs_ext = xs_ext,
    loss_ext = loss_ext,
    xs_qxt = xs_qxt
  ))
}
