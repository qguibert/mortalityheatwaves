#####################################################################################################
#' summary_climate: Display projection of temperature per RCP and heatwave indicators.
#' This function uses DRIAS climate data to produce a four sub-figures with projected
#' annual average of daily temperature, heatwave duration per decade, cumulative
#' heatwave severity per decade, and heatwave intensity per decade.
#####################################################################################################

summary_climate <- function(data)
{
  # List of scenarios
  id <- unique(paste0(data$model, "-", data$rcp))
  list_model <- unique(data$model)
  list_rcp <- unique(data$rcp)

  # Figure 1 : number of hw days, severity and intensity per years
  df <- data %>%
    filter(YEARS >= start_year, YEARS < end_year) %>%
    #create year buckets
    mutate(year_bk = cut(YEARS, year_breaks, year_labels, include.lowest = T)) %>%
    group_by(model, rcp, year_bk, Num_vague) %>%
    summarise(Duration = sum(Ind_canicule),
              Severity = sum(Severite_canicule),
              Intensity = max(Severite_canicule))

  # palette Brewer Set3
  palette_set1 <- brewer.pal(12, "Set1")
  # Select 3 couleurs and reverse
  sub_palette <- palette_set1[c(2,3,1)]

  # agregage figure per decade
  fig_agg1 <- ggplot(df, aes(x = year_bk, y = Duration)) +
    geom_boxplot(aes(fill = rcp), alpha = 0.5) +
    theme(legend.position = c(0.1, 0.85),
          legend.background = element_rect(fill = "white", color = "black"))+
    labs(x = "Years", y = "Duration (number of days)",
         fill = "RCP") +
    scale_fill_manual(values = sub_palette)
  fig_agg2 <- ggplot(df, aes(x = year_bk, y = Severity)) +
    geom_boxplot(aes(fill = rcp), alpha = 0.5) +
    theme(legend.position = c(0.1, 0.85),
          legend.background = element_rect(fill = "white", color = "black"))+
    labs(x = "Years", y = "Cumulative Severity", fill = "RCP") +
    scale_fill_manual(values = sub_palette)
  fig_agg3 <- ggplot(df, aes(x = year_bk, y = Intensity)) +
    geom_boxplot(aes(fill = rcp), alpha = 0.5) +
    theme(legend.position = c(0.1, 0.85),
          legend.background = element_rect(fill = "white", color = "black"))+
    labs(x = "Years", y = "Intensity", fill = "RCP") +
    scale_fill_manual(values = sub_palette)
  fig_agg4 <- data %>%
    group_by(rcp, model, YEARS) %>%
    summarise(tavg = mean(TAVG)) %>%
    group_by(rcp, YEARS) %>%
    summarise(tmean = mean(tavg),
              tmax = quantile(tavg, 0.95),
              tmin = quantile(tavg, 0.05)) %>%
    ggplot(aes(x = YEARS, y = tmean)) +
    geom_line(aes(colour = factor(rcp))) +
    geom_ribbon(aes(ymax = tmax, ymin = tmin, fill = factor(rcp)), alpha = 0.3) +
    theme(legend.position = c(0.1, 0.85),
          legend.background = element_rect(fill = "white", color = "black"))+
    labs(x = "Years", y = "Avg. Temperature [C]", fill = "RCP", colour = "RCP") +
    scale_colour_manual(values = sub_palette) +
    scale_fill_manual(values = sub_palette)

  # Aggregate fig
  fig_agg <- plot_grid(fig_agg4, fig_agg1, fig_agg2, fig_agg3,
                       labels = c('(a)', '(b)', '(c)', '(d)'),
                       label_size = 12)

  # data frame for figure 2
  df2 <- data %>%
    group_by(model, rcp, YEARS, Num_vague) %>%
    summarise(Duration = sum(Ind_canicule),
              Severity = sum(Severite_canicule),
              Intensity = max(Severite_canicule))
  # bubble for each year
  fig1 <- lapply(list_model,
                 function(m)
                 {
                   lapply(list_rcp,
                          function(sc)
                          {
                            temp <- df2 %>%
                              filter(model == m , rcp == sc)
                            if(nrow(temp) == 0)
                            {
                              return(NULL)
                            } else
                            {
                              ggplot(data = temp) +
                                geom_circle(aes(x0 = Duration, y0 = Severity,
                                                r = Intensity/3,
                                                fill = Intensity,
                                                color = Intensity),
                                            alpha = 0.9, na.rm=FALSE, show.legend = TRUE) +
                                geom_text(aes(x = Duration, y = Severity, label = YEARS),
                                          color = "black",  size = 4) +
                                #coord_fixed(ratio = 1) +
                                scale_color_distiller(palette = "Spectral", limits = c(0,30)) +
                                scale_fill_distiller(palette = "Spectral", limits = c(0,30)) +
                                # Names of axes
                                labs(y = "Cumulative Severity", x = "Duration (number of days)") +
                                scale_y_continuous(trans = trans_div_quatre, limits = c(0,500)) +
                                scale_x_continuous(trans = trans_mult_cinq, limits = c(0,45))

                            }
                          })
                 })

  # # --- Figure 3 : number of hw days per years
  df3 <- df2 %>%
    filter(Num_vague > 0)%>%
    summarise(Duration = round(sum(Duration),1),
              Severity = round(sum(Severity),1),
              Intensity = round(mean(Intensity),1))

  fig2 <- lapply(list_model,
                    function(m){
                      lapply(list_rcp,
                             function(sc)
                             {
                               df3 %>%
                                 filter(model == m , rcp == sc) %>%
                                 ggplot() +
                                 geom_bar(aes(x = YEARS, y = Duration, fill = Duration),
                                          stat="identity", position=position_dodge(),
                                          alpha = 0.75) +
                                 labs(x = "Years",
                                      y = "Duration (number of days)") +
                                 scale_fill_distiller(palette = "Spectral",
                                                      limits = c(1,85)) +
                                 ylim(0,85)
                             })
                    })
  # --- Figure 4 : number of hw days per months x period
  df4 <- data %>%
    mutate(MONTHS = month(DATE)) %>%
    group_by(model, rcp, YEARS, Num_vague, MONTHS) %>%
    summarise(Duration = round(sum(Ind_canicule),1),
              Severity = round(sum(Severite_canicule),1),
              Intensity = round(max(Severite_canicule),1)) %>%
    mutate(Period = ifelse(
      YEARS < 2040,  "2020-2039", ifelse(
        YEARS < 2060 & YEARS >=2040,  "2040-2059",ifelse(
          YEARS < 2080 & YEARS >=2060, "2060-2079","2080-2100"
        ))
    )) %>% group_by(model, rcp, Period, MONTHS) %>%
    summarise(Duration = round(mean(Duration),1),
              Severity = round(mean(Severity),1),
              Intensity = round(mean(Intensity),1))

  fig3 <- lapply(list_model,
                 function(m)
                 {
                   lapply(list_rcp,
                          function(sc)
                          {
                            df4 %>%
                              filter(model == m , rcp == sc) %>%
                              ggplot(aes(x = MONTHS, y = Duration)) +
                              geom_bar(aes(fill = Period), stat="identity",
                                       position = position_dodge(),
                                       alpha = 0.5) +
                              scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                                                 labels= c( "Jan.","Feb.", "Mar.",
                                                            "Apr.", "May", "June",
                                                            "July", "Aug.", "Sept.",
                                                            "Oct.","Nov.", "Dec." ))+
                              scale_fill_manual(values = c("#56B4E9", "#009E73",
                                                           "#F0E442", "#0072B2"))+
                              ylim(0,30)+
                              labs(x = "Months",
                                   y = "Average Duration (number of days)")
                          })
                 })


  # --- Summary tables
  df5 <- df2 %>%
    group_by(model, rcp, YEARS, Num_vague) %>%
    filter(Num_vague > 0)%>%
    summarise(Duration = round(sum(Duration),1),
              Severity = round(sum(Severity),1),
              Intensity = round(mean(Intensity),1)) %>%
    mutate(Periode =  ifelse
           (YEARS < 2050,  "2020-2049", ifelse(
             YEARS > 2070,  "2070-2100",  "2050-2069")
             )) %>%
    group_by(model, rcp, Periode) %>%
    summarise(Duration = (sum(Duration)),
              Severity = (sum(Severity)),
              Intensity = (sum(Intensity))) %>%
    mutate(Duration =
             ifelse(
      Periode == "2020-2049",  round(Duration/(2049-2020),1), ifelse(
        Periode == "2070-2100",  round(Duration/(2100-2070),1),  round(Duration/(2069-2050),1))
      ),
      Severity =
        ifelse(
      Periode == "2020-2049",  round(Severity/(2049-2020),1), ifelse(
        Periode == "2070-2100",  round(Severity/(2100-2070),1),  round(Severity/(2069-2050),1))
      ),
    Intensity =
      ifelse(
      Periode == "2020-2049",  round(Intensity/(2049-2020),1), ifelse(
        Periode == "2070-2100",  round(Intensity/(2100-2070),1),  round(Intensity/(2069-2050),1))
      )
    )

  tab1 <- lapply(list_model,
                 function(m)
                 {lapply(list_rcp,
                         function(sc)
                         {
                           df5 %>%
                             filter(model == m , rcp == sc)
                         })
                 })


  return(
    list(
      fig_agg = fig_agg,
      figure1 = fig1,
      figure2 = fig2,
      figure3 = fig3,
      tab1 = tab1
    )
  )
}

