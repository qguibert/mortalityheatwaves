#----------------------------------------------------------
#  summary_dlnm_all_sex
#----------------------------------------------------------
summary_dlnm_all_sex <- function(obj)
{
  if(! is(obj, "list"))
  {
    stop("'obj' should be list")
  }

  # Visualization
  # get prediction and ci intervals
  pred <- do.call("rbind",
                  lapply(names(obj), function(g)
                  {
                    do.call("rbind",
                            lapply(names(obj[[g]]), function(x)
                            {
                              res <- data.frame(
                                sex = g,
                                age_bk = x,
                                temp = obj[[g]][[x]]$pred$predvar,
                                rr = obj[[g]][[x]]$pred$allRRfit,
                                rrlow = obj[[g]][[x]]$pred$allRRlow,
                                rrhigh = obj[[g]][[x]]$pred$allRRhigh
                              )
                            }))
                  }))
  pred$sex <- as.factor(pred$sex)
  levels(pred$sex) <- c("Female", "Male")

  #plot RR
  RRplot <- ggplot(pred) +
    geom_line(aes(x = temp, y = rr, colour = sex), linewidth = 1.2) +
    geom_ribbon(aes(x = temp, ymin = rrlow, ymax = rrhigh, fill = sex), alpha = 0.3) +
    # geom_vline(
    #   xintercept = quantile(obj[[1]][[1]]$model$data$tavg, 0.99, na.rm = T),
    #   linetype = "dashed", color = "red", linewidth = 1
    # ) +
    # geom_vline(
    #   xintercept = quantile(obj[[1]][[1]]$model$data$tavg, 0.95, na.rm = T),
    #   linetype = "dashed", color = "orange",linewidth = 1
    # ) +
    # geom_vline(
    #   xintercept = quantile(obj[[1]][[1]]$model$data$tavg, 0.05, na.rm = T),
    #   linetype = "dashed", color = "cyan", linewidth = 1
    # ) +
    # geom_vline(
    #   xintercept = quantile(obj[[1]][[1]]$model$data$tavg, 0.01, na.rm = T),
    #   linetype = "dashed", color = "blue", linewidth = 1
    # ) +
    geom_hline(
      yintercept = 1,
      linetype = "solid", color = "grey40"
    ) +
    facet_wrap(~ age_bk, ncol = 2) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = c(0.05, 0.95),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.title=element_blank()) +
    ylim(c(0.9, 3))+
    labs(y = "Relative Risk", x = "Temperature [C]") +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1")

  return(RRplot)
}








