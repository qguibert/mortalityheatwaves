plot_kt_sim <- function(proj_par, virt_proj_par, n_sim, annee_deb, end_year)
{

  # select trend
  name_trend <- c("K.t_M", "k.t_M", "k.t_F")

  # Transform data for the model without temp. effects
  tab_par <- lapply(c(name_trend), function(x)
  {
    tab <- as_tibble(t(proj_par[[x]])) %>%
      mutate(sim = 1:n_sim) %>%
      relocate(sim)
    names(tab) <- c("sim", annee_deb:end_year)
    tab <- tab %>% tidyr::pivot_longer(cols = -sim,
                                       names_to = "year",
                                       values_to = "trend") %>%
      mutate(trend_name = x, model = "Without temp. effects")
  })
  tab_par <- do.call("rbind", tab_par)

  # Transform data for the model with temp. effects
  tab_virt_par <- lapply(c(name_trend), function(x)
  {
    tab <- as_tibble(t(virt_proj_par[[x]])) %>%
      mutate(sim = 1:n_sim) %>%
      relocate(sim)
    names(tab) <- c("sim", annee_deb:end_year)
    tab <- tab %>% tidyr::pivot_longer(cols = -sim,
                                       names_to = "year",
                                       values_to = "trend") %>%
      mutate(trend_name = x, model = "With temp. effects")
  })
  tab_virt_par <- do.call("rbind", tab_virt_par)

  # Merge datesets
  tab <- rbind(tab_par, tab_virt_par)
  tab$trend_name <- as.factor(tab$trend_name)
  levels(tab$trend_name) <- c("kappa[t]^(f)", "kappa[t]^(m)", "K[t]")

  # fanchart arguments
  temp <- tab %>%
    group_by(year, trend_name, model) %>%
    summarise(tmedian = median(trend),
              tmax = quantile(trend, 0.95),
              tmin = quantile(trend, 0.05))


  # palette Brewer Set3
  palette_set1 <- brewer.pal(12, "Set1")
  # Select 3 couleurs and reverse
  sub_palette <- palette_set1[c(2,3)]


  g <- ggplot(temp, aes(x = as.numeric(year), y =  tmedian)) +
    geom_line(aes(colour = factor(model))) +
    geom_ribbon(aes(ymax = tmax, ymin = tmin, fill = factor(model)), alpha = 0.3) +
    facet_wrap(~ trend_name, scales = "free_y", labeller = label_parsed) +
    labs(x = "Years", y = "Trend", fill = "Calibration data", colour = "Calibration data") +
    scale_colour_manual(values = sub_palette) +
    scale_fill_manual(values = sub_palette) +
    theme(legend.position = c(.1, 0.2),
          legend.background = element_rect(fill = "white", color = "black"),
          strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          text = element_text(size = 12))
  return(g)
}
