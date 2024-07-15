########################################
#' summary_forecast_temp_effect
########################################
summary_forecast_temp_effect <- function(afftab, dec_weight)
{
  sub_palette <- c("black", "blue", "red", "lightblue", "orange")

  # Aggregate results
    temp <- afftab %>%
      ungroup() %>%
      dplyr::filter(sim %in% 1:15) %>%
      left_join(dec_weight, by = c("age_bk", "gender")) %>%
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
      mutate(rcp = fct_recode(rcp,
                              "RCP 2.6" = "2.6",
                              "RCP 4.5" = "4.5",
                              "RCP 8.5" = "8.5")) %>%
      group_by(traj_clim, rcp, temp_effect, year, sim) %>%
      summarise(attrib_frac = sum(attrib_frac * w) * 100) %>%
      ungroup() %>%
      group_by(rcp, temp_effect, year) %>%
      summarise(xsmean = mean(attrib_frac),
                xsmax = quantile(attrib_frac, 0.95),
                xsmin = max(0,quantile(attrib_frac, 0.05)))

    # Plot aggregate results
    res_agg <- temp %>%
      ggplot(aes(x = year, y = xsmean)) +
      geom_line(aes(colour = factor(temp_effect))) +
      geom_ribbon(aes(ymax = xsmax, ymin = xsmin, fill = factor(temp_effect)), alpha = 0.3) +
      facet_wrap( ~ rcp, scales = "free_y", ncol = 3) +
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



  # Produce result for each age bucket
  res_bk <- lapply(age_labels, function(i){
    temp <- afftab %>%
      ungroup() %>%
      dplyr::filter(age_bk == i) %>%
      # dplyr::filter(sim %in% 1:15) %>%
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
      mutate(rcp = fct_recode(rcp,
                              "RCP 2.6" = "2.6",
                              "RCP 4.5" = "4.5",
                              "RCP 8.5" = "8.5")) %>%
      group_by(rcp, gender, temp_effect, year) %>%
      summarise(xsmean = mean(attrib_frac * 100),
                xsmax = quantile(attrib_frac * 100, 0.95),
                xsmin = max(0,quantile(attrib_frac * 100, 0.05)))

    p1 <- temp %>%
      filter(gender == "f") %>%
      ggplot(aes(x = year, y = xsmean)) +
      geom_line(aes(colour = factor(temp_effect))) +
      geom_ribbon(aes(ymax = xsmax, ymin = xsmin, fill = factor(temp_effect)), alpha = 0.3) +
      facet_wrap( ~ rcp, scales = "free_y", ncol = 3) +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(color = "black"),
            panel.border = element_rect(color = "black", fill = NA),
            text = element_text(size = 12))+
      labs(x = "Years", y = "Excess mortality (%)", fill = "Temperature effect",
           colour = "Temperature effect") +
      scale_colour_manual(values = sub_palette) +
      scale_fill_manual(values = sub_palette) +
      ggtitle(paste0("Females", " - ", i))


    p2 <- temp %>%
      filter(gender == "m") %>%
      ggplot(aes(x = year, y = xsmean)) +
      geom_line(aes(colour = factor(temp_effect))) +
      geom_ribbon(aes(ymax = xsmax, ymin = xsmin, fill = factor(temp_effect)), alpha = 0.3) +
      facet_wrap( ~ rcp, scales = "free_y", ncol = 3) +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = "white", color = "black"),
            strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(color = "black"),
            panel.border = element_rect(color = "black", fill = NA),
            text = element_text(size = 12))+
      labs(x = "Years", y = "Excess mortality (%)", fill = "Temperature effect",
           colour = "Temperature effect") +
      scale_colour_manual(values = sub_palette) +
      scale_fill_manual(values = sub_palette)  +
      ggtitle(paste0("Males", " - ", i))

    # add common legend
    common_legend <- get_legend(p1)

    plots_with_legend <- lapply(list(p1, p2), function(p) {
      p + theme(legend.position = "none")  # Suppress legends
    })

    p.comb <- ggarrange(plotlist = list(p1, p2),
                        ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
    return(p.comb)
  })
  return(list(res_agg, res_bk))
}
