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

Our implementation of the Li and Lee (2005) is based on the [**MultiMoMo**](https://github.com/jensrobben/MultiMoMo)`R`\-package.
Our adjustment are available in the folder
*\\multimomo*.

The DLNM model is fitted using
[**dlnm**](https://cran.r-project.org/web/packages/dlnm/index.html) `R`\-package.
