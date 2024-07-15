###################################
#'summary_forecast_ev_effect
###################################

summary_forecast_ev_effect <- function(etab)
{
  temp <- etab %>%
    mutate(rcp = fct_recode(rcp,
                            "RCP 2.6" = "2.6",
                            "RCP 4.5" = "4.5",
                            "RCP 8.5" = "8.5")) %>%
    mutate(temp_effect = fct_recode(temp_effect,
                                    "All effects" = "all_effect",
                                    "Extreme hot" = "extr_hot_effect")) %>%
    group_by(rcp, gender, temp_effect, year, age) %>%
    filter(age == 0) %>%
    summarise(e = median(Ext), emax = quantile(Ext, 0.95), emin = quantile(Ext, 0.05),
              etemp = median(Ext_xs), etemp_max = quantile(Ext_xs, 0.95), etemp_min = quantile(Ext_xs, 0.05),
              gap = median(Ext_xs - Ext), gapmax = quantile(Ext_xs - Ext,0.95), gapmin = quantile(Ext_xs - Ext,0.05))

  # Plot EV with and without temperature effect
  p1 <- temp %>%
    filter(gender == "f") %>%
    filter(temp_effect == "All effects") %>%
    ggplot() +
    geom_line(aes(x = year, y = e), color = "blue") +
    geom_ribbon(aes(x = year, ymax = emax, ymin = emin), fill = "blue", alpha = 0.3) +
    geom_line(aes(x = year, y = etemp), color = "red") +
    geom_ribbon(aes(x = year, ymax = etemp_max, ymin = etemp_min), fill = "red", alpha = 0.3) +
    facet_wrap( ~ rcp) + ylim(c(75, 100)) +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill = "white", color = "black"),
          strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          text = element_text(size = 12)) +
    labs(x = "Years", y = "Life expenctancy") +
    ggtitle("Females")

  p2 <- temp %>%
    filter(gender == "m") %>%
    filter(temp_effect == "All effects") %>%
    ggplot() +
    geom_line(aes(x = year, y = e), color = "blue") +
    geom_ribbon(aes(x = year, ymax = emax, ymin = emin), fill = "blue", alpha = 0.3) +
    geom_line(aes(x = year, y = etemp), color = "red") +
    geom_ribbon(aes(x = year, ymax = etemp_max, ymin = etemp_min), fill = "red", alpha = 0.3) +
    facet_wrap( ~ rcp) + ylim(c(75, 100)) +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill = "white", color = "black"),
          strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          text = element_text(size = 12)) +
    labs(x = "Years", y = "Life expenctancy") +
    ggtitle("Males")

  p.ev <- ggarrange(plotlist = list(p1, p2),
                    ncol = 1, nrow = 2)

  # Plot EV Gap with and without temperature effect
  p3 <- temp %>%
    ungroup() %>%
    filter(temp_effect == "All effects") %>%
    mutate(gender = fct_recode(gender,
                               "Male" = "m",
                               "Female" = "f")) %>%
    ggplot() +
    geom_line(aes(x = year, y = gap, color = gender)) +
    geom_ribbon(aes(x = year, ymax = gapmax, ymin = gapmin, fill = gender), alpha = 0.3) +
    facet_wrap( ~ rcp) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = c(0.95, 0.2),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title=element_blank()) +
    labs(x = "Years", y = "Life expenctancy loss") +
    ggtitle("All effects") +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1")

  p4 <- temp %>%
    ungroup() %>%
    filter(temp_effect == "Extreme hot") %>%
    mutate(gender = fct_recode(gender,
                               "Male" = "m",
                               "Female" = "f")) %>%
    ggplot() +
    geom_line(aes(x = year, y = gap, color = gender)) +
    geom_ribbon(aes(x = year, ymax = gapmax, ymin = gapmin, fill = gender), alpha = 0.3) +
    facet_wrap( ~ rcp) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none",
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title=element_blank()) +
    labs(x = "Years", y = "Life expenctancy loss") +
    ggtitle("Extreme hot") +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1")

  p.gap <- ggarrange(plotlist = list(p3, p4),
                     ncol = 1, nrow = 2)

  return(list(p.ev = p.ev, p.gap = p.gap))

}
