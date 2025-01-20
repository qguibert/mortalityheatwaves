########################################
#' summary_temp_effect : display the temperature-mortality attributable fractions
########################################
summary_temp_effect <- function(xs_data){

  sub_palette <- c("black", "blue", "red", "lightblue", "orange")

  # arrange data and display
  p <- xs_data %>%
    dplyr::filter(temp_effect %in% c("all_effect",
                                     "moderate_hot_effect",
                                     "moderate_cold_effect",
                                     "extr_hot_effect","extr_cold_effect")) %>%
    mutate(temp_effect = fct_recode(temp_effect,
                                    "All effects" = "all_effect",
                                    "Extreme cold" = "extr_cold_effect",
                                    "Extreme hot" = "extr_hot_effect",
                                    "Moderate cold" = "moderate_cold_effect",
                                    "Moderate hot" = "moderate_hot_effect")) %>%
    mutate(gender = fct_recode(gender, "Females" = "f" ,
                               "Males" = "m")) %>%
    group_by(year, sim, temp_effect, gender) %>%
    summarise(Dxt = sum(Dxt),
              an_sum_all_ages = sum(an_sum),
              cases_sum_all_ages = sum(cases_sum)) %>%
    mutate(xs_rate = an_sum_all_ages / cases_sum_all_ages * 100) %>%
    group_by(year, temp_effect, gender) %>%
    summarise(xsmean = mean(xs_rate),
              xsmax = quantile(xs_rate, 0.95),
              xsmin = quantile(xs_rate, 0.05)) %>%
    ggplot(aes(x = year, y = xsmean)) +
    geom_line(aes(colour = factor(temp_effect))) +
    geom_ribbon(aes(ymax = xsmax, ymin = xsmin, fill = factor(temp_effect)), alpha = 0.3) +
    facet_wrap(~ gender, scales = "free_x", ncol = 2) +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill = "white", color = "black"),
          strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          text = element_text(size = 12))+
    labs(x = "Years", y = "Excess mortality (%)", fill = "Temperature effect",
         colour = "Temperature effect") +
    scale_colour_manual(values = sub_palette) +
    scale_fill_manual(values = sub_palette)

  return(p)

}
