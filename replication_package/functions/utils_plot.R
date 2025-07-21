
# Load graphical packages
library(lattice)
library(grid)
library(ggplot2)
library(ggfan)
require(gridExtra)
library(locfit)
library(scales)
library(dplyr)
library(formattable)
library(RColorBrewer)

conf_plot <- qnorm(1 - 0.05 /2)
#----------------------------------------------------------
# Main theme
grid.newpage();grid.draw(roundrectGrob(gp = gpar(lwd = NA)))
theme_perso <-function (base_size = 12, base_family = "")
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(size = 8),
          axis.ticks = element_line(colour = "black"),
          axis.title=element_text(size = 8),
          legend.key = element_rect(colour = "white"),
          panel.background = element_rect(fill = "white",colour = NA),
          panel.border = element_rect(fill = NA,colour = "black"),
          panel.grid.major = element_line(colour = "white", size = 0.2),
          panel.grid.minor = element_line(colour = "white", size = 0.5),
          strip.background = element_rect(fill = "grey", colour = "black"),
          strip.text = element_text(size=rel(1)),
          legend.text=element_text(size=rel(1)),
          legend.position="bottom",
          legend.title = element_text(face = "bold"),
          plot.background=element_rect(fill = NA,colour = NULL)
    )
}
theme_set(theme_perso())
trellis.device(color = FALSE)
#trellis.par.set("axis.line",list(col="black",lty=0.5,lwd=0.5))
#----------------------------------------------------------
# Managing figures
#----------------------------------------------------------
sauveFigure=function(config, title = NULL, path = NULL){
  if(is.null(title)){
    numeroFigure <- length(list.files(dossier_fig)) + 1L
  } else
  {
    numeroFigure <- title
  }
  if(is.null(path)){
    path <- "figures/fig_"
  }

  name <- paste0(path, numeroFigure,".pdf")

  if(config == 1){
    pdf(file = name, width = 12, height = 8, pointsize = 12)
  }
  if(config == 2){
    pdf(file = name, width = 24, height = 8, pointsize = 12)
  }

  if(config == 3){
    pdf(file = name, width = 12, height = 10.5, pointsize = 12)
  }

  if(config == 4){
    pdf(file = name, width = 18, height = 23, pointsize = 12)
  }

  if(config == 5){
    pdf(file = name, width = 18, height = 12, pointsize = 12)
  }

  if(config == 6){
    pdf(file = name, width = 18, height = 8, pointsize = 12)
  }

  if(config == 7){
    pdf(file = name, width = 12, height = 12, pointsize = 12)
  }

  if(config == 8){
    pdf(file = name, width = 12, height = 6, pointsize = 12)
  }

  if(config == 9){
    pdf(file = name, width = 12, height = 4, pointsize = 12)
  }
  if(config == 10){
    name <- paste0(path, numeroFigure,".png")
    png(file = name, width = 480, height = 480, pointsize = 12)
  }
  if(config == 11){
    pdf(file = name, width = 18, height = 8, pointsize = 12)
  }
}

mult_trois <- function(x) x * 3
mtrois <- function(x) x / 3
trans_mult_trois <- trans_new(name = "mult trois",
                              transform = mult_trois,
                              inverse = mtrois)

mult_quatre <- function(x) x * 4
mquatre <- function(x) x / 4
trans_mult_quatre <- trans_new(name = "mult quatre",
                               transform = mult_quatre,
                               inverse = mquatre)

mult_cinq <- function(x) x * 5
mcinq <- function(x) x / 5
trans_mult_cinq <- trans_new(name = "mult cinq",
                             transform = mult_cinq,
                             inverse = mcinq)

div_deux <- function(x) x / 2
ddeux <- function(x) x * 2
trans_div_deux <- trans_new(name = "div deux",
                            transform = div_deux,
                            inverse = ddeux)

div_quatre <- function(x) x / 4
dquatre <- function(x) x * 4
trans_div_quatre <- trans_new(name = "div quatre",
                              transform = div_quatre,
                              inverse = dquatre)
