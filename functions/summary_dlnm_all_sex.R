#----------------------------------------------------------
#'  summary_dlnm_all_sex: This function takes as input a nested
#'  list object containing DLNM (Distributed Lag Non-Linear Model)
#'   results for different sexes and age groups. It extracts the estimated
#'   relative risks and their confidence intervals, then returns
#'   a ggplot object visualizing the exposure–response curves
#'   for each age group and sex.
#----------------------------------------------------------
summary_dlnm_all_sex <- function(obj)
{
  # Check that the input is a list
  if(! is(obj, "list"))
  {
    stop("'obj' should be list")
  }

  # Extract predictions and confidence intervals from all groups
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

  # Set sex variable as factor with labels
  pred$sex <- as.factor(pred$sex)
  levels(pred$sex) <- c("Female", "Male")

  # Create the RR plot
  RRplot <- ggplot(pred) +
    geom_line(aes(x = temp, y = rr, colour = sex), linewidth = 1.2) +
    geom_ribbon(aes(x = temp, ymin = rrlow, ymax = rrhigh, fill = sex), alpha = 0.3) +
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








