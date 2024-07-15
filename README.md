# Impacts of Climate Change on Mortality: An extrapolation of temperature effects based on time series data in France

This github repository illustrated applications presented in the paper 
**Impacts of Climate Change on Mortality: An extrapolation of temperature effects based on time series data in France**, written by Quentin Guibert, Gaëlle Pincemin and Frédéric Planchet.
In this paper, we present a multi-population mortality model integrating
temperature effects in France. This document includes example codes on
`R` to reproduce the process presented in this article.

The paper is available on Arxiv <https://arxiv.org/abs/2406.02054>.

# Models

Our mortality model is based on the Li and Lee (2005) mortality model.
It is coupled with a Distributed lag non-linear generalized model (DLNM)
for integrating the effect of future temperatures on mortality.

Our implementation of the Li and Lee (2005) is based on the [**MultiMoMo**](https://github.com/jensrobben/MultiMoMo)`R`-package. 
Our adjustment are available in the folder 
*\multimomo*.

The DLNM model is fitted using 
[**dlnm**](https://cran.r-project.org/web/packages/dlnm/index.html) `R`-package. 
# Data 

## Climate Data
The historical temperature records came from the [GHCN database](https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily).

## Climata Scenarios

We used climate simulations available through the [DRIAS portal](https://www.drias-climat.fr/).
Temperature data is available in France on a daily time step over the period 2006-2100.

## Daily mortality data 
Our daily mortality data for the Metropolitan France came from a specific
inquiry made to the Quetelet-Prodego Diffusion network (doi: <10.13144/PSM-0015> ;
État Civil - Décès, INSEE (producteur), PROGEDO-ADISP (diffuseur)). 
This data is not available on this repo.

## Annual Mortality Data

The annual mortality data was downloaded from the 
[Human Mortality Database](https://www.mortality.org/).





