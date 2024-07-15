myplot_parameters_li_lee <- function(xv, yv, list_fit)
{

  # get model results
  fit_M <- list_fit$fit_M
  fit_F <- list_fit$fit_F
  virt_fit_M <- list_fit$virt_fit_M
  virt_fit_F <- list_fit$virt_fit_F

  if (length(fit_M$k.t) != length(yv))
    stop("The vector yv should have the same length as the length of the time series k.t.")

  if (length(fit_M$A.x) != length(xv))
    stop("The vector xv should have the same length as the length of the time series A.x, B.x, a.x and b.x.")

  param_names <- c("A.x", "B.x", "K.t",
                   "a.x", "b.x", "k.t",
                   "a.x", "b.x", "k.t")

  # k.t and K.t are possibly of different length
  yv.kt <- yv
  yv.Kt <- (tail(yv,1) - length(fit_M$K.t) + 1):tail(yv,1)

  xrange        <- list(xv, xv, yv.Kt, xv, xv, yv.kt, xv, xv, yv.kt)
  names(xrange) <- param_names

  smain <- list(bquote("Common":~A[x]), bquote("Common":~B[x]), bquote("Common":~K[t]),
                bquote("Female": ~alpha[x]^(f)),
                bquote("Female": ~beta[x]^(f)),
                bquote("Female": ~kappa[t]^(f)),
                bquote("Male": ~alpha[x]^(m)),
                bquote("Male": ~beta[x]^(m)),
                bquote("Male": ~kappa[t]^(m)))

  names(smain) <- param_names
  sxlab <- list("Age", "Age", "Year", "Age", "Age", "Year", "Age", "Age", "Year")

  # plot
  # g_legend <- function(a.gplot){
  #   tmp <- ggplot_gtable(ggplot_build(a.gplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)}

  plots <- list()
  # Loop for common and females
  for (t in 1:6){
    df <- data.frame("x" = xrange[[t]], "y" = fit_F[[param_names[[t]]]],
                     "var" = "Without temp. effects")
    df_virt <- data.frame("x" = xrange[[t]], "y" = virt_fit_F[[param_names[[t]]]],
                          "var" = "With temp. effects")
    df <- rbind(df, df_virt)
    plots[[t]] <- ggplot(df) +
      geom_line(mapping = aes(x = x, y = y, col = var, linetype = var)) +
      xlab(sxlab[t]) + ylab("") + theme_bw(base_size = 15) + ggtitle(smain[[t]]) +
      scale_color_manual(values = c("black", "blue")) +
      labs(color = "") + labs(linetype = "")
    }

  # Loop for males
  for (t in 7:9){
    df <- data.frame("x" = xrange[[t]], "y" = fit_M[[param_names[[t]]]],
                     "var" = "Without temp. effects")
    df_virt <- data.frame("x" = xrange[[t]], "y" = virt_fit_M[[param_names[[t]]]],
                          "var" = "With temp. effects")
    df <- rbind(df, df_virt)
    plots[[t]] <- ggplot(df) +
      geom_line(mapping = aes(x = x, y = y, col = var, linetype = var)) +
      xlab(sxlab[t]) + ylab("") + theme_bw(base_size = 15) + ggtitle(smain[[t]]) +
      scale_color_manual(values = c("black", "blue")) +
      labs(color = "") + labs(linetype = "")
  }

  # add common legend
  common_legend <- get_legend(plots[[1]])

  plots_with_legend <- lapply(plots, function(p) {
    p + theme(legend.position = "none")  # Suppress legends
  })

  ggarrange(plotlist = plots,
                 ncol = 3, nrow = 3, common.legend = TRUE, legend = "bottom")
}



