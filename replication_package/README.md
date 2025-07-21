---
contributors:
  - Quentin Guibert
  - Gaëlle Pincemin
  - Frédéric Planchet
---
# Replication package for the paper "Impacts of Climate Change on Mortality: An extrapolation of temperature effects based on time series data in France"

## Overview

This repository reproduced all figures and tables presented in the paper
**Impacts of Climate Change on Mortality: An extrapolation of temperature effects based on time series data in France**, 
written by Quentin Guibert, Gaëlle Pincemin and Frédéric Planchet, available
[here](https://arxiv.org/abs/2406.02054).

##  Data availability and provenance 

This paper is based on four datasets, as described in the table below.
The available data should be deposited in the `data_raw/` directory.

| Data name  | Data.Files | Location | Provided | Citation |
| -- | -- | -- | -- | -- | 
| Global Historical Climatology Network daily | `GHCN_1950-2020.xlsx` | `data_raw/GHCN` | FALSE | Menne et al. (2012) |
| DRIAS-climat  | `[Climate_Model_Name]_[RCP_Number].txt` | `data_raw/DRIAS/[Climate_Model_Name]/[RCP_Number]`  | FALSE | DRIAS (2020) |
| Human Mortality Database  | To download | <https://www.mortality.org>  | FALSE | HMD (2024) |
| Insee  | `tableau_2.csv` | `data_raw/PROGEDO`  | FALSE | INSEE (2020) |

## Temperature records

The historical temperature records originate from the GHCN database (Menne et al., 2012).
These data, produced by a U.S. federal agency (NOAA), are publicly available and
can be freely used and redistributed. They should be stored in the `data_raw/GHCN`
directory and can also be downloaded using the DOI <10.7289/V5D21VHZ> or
via the following link: [https://www.ncei.noaa.gov/cdo-web/search?datasetid=GHCND](https://www.ncei.noaa.gov/cdo-web/search?datasetid=GHCND).

If you wishe to reproduce the data reconstruction process, you must
follow the steps outlined below:

  1. Access the NOAA Climate Data Online website.
  2. Select the `Daily Summaries` dataset and specify the date range from January 1, 1950, to December 31, 2020.
  3. Manually search for each station by entering its name.
  4. Click on `Search`.
  5. Select the station on the map and click `ADD TO CART`.
  6. Proceed to the CART, select the CSV format, and click `CONTINUE`.
  7. Under options, select `Station Name`, choose the unit system (`Standard`
  for degrees Fahrenheit or `Metric` for degrees Celsius),
  and check the `Air Temperature` option to include the variables `TAVG`, `TMAX`, and `TMIN`.
  8. Submit the request, specifying your email address.
  9. Download the CSV files using the link received by email.

This process must be repeated for the 14 stations listed below:

  1. BORDEAUX MERIGNAC, FR
  2. BREST GUIPAVAS, FR
  3. CAEN CARPIQUET, FR
  4. CLERMONT FERRAND, FR
  5. DIJON LONGVIC, FR
  6. LILLE LESQUIN, FR
  7. LYON ST EXUPERY, FR
  8. MARSEILLE MARIGNANE, FR
  9. NANTES BOUGUENAIS, FR
  10. PARIS LE BOURGET, FR
  11. PERPIGNAN, FR
  12. STRASBOURG ENTZHEIM, FR
  13. TOULOUSE BLAGNAC, FR
  14. TOURS, FR

If the data are downloaded in Fahrenheit, they must be converted to Celsius prior
to any further processing. Once all files have been downloaded and prepared,
they should be merged into a single Excel file named `GHCN_1950-2020.xlsx`.

Based on these raw data, the full preprocessing workflow can be reproduced
using the script `1-Prep_Data_Temp`. The daily records include average (TAVG),
minimum (TMIN), and maximum (TMAX) temperatures for each station.

## Climata Scenarios

We use climate simulations available through the [DRIAS portal](https://www.drias-climat.fr/).
The data made available on the DRIAS-climat portals are covered by the 
[Etalab Open License](https://www.etalab.gouv.fr/wp-content/uploads/2017/04/ETALAB-Licence-Ouverte-v2.0.pdf),
which permits reproduction and distribution.
They should be stored in the `data_raw/DRIAS` directory. 
This data can also be retrieved directly from the DRIAS portal by creating a user account. 
Based on these raw data, processing the data can be reproduced using the script `2-Prep_Data_Clim`.

## Annual mortality data

We use annual mortality data for metropolitan France provided by the HMD (2024).
These data are licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/),
which permits reproduction and distribution.

Downloading and processing the data can be reproduced using the script `3-Prep_Data_HMD`,
after creating an account on the [HMD website](https://www.mortality.org/Account/Auth)
and entering your username and password in lines 69 and 70 of the script.

## Daily mortality data

Our daily mortality data for the Metropolitan France came from a specific
inquiry made to the Quetelet-Prodego Diffusion network (doi: <10.13144/PSM-0015> ;
État Civil - Décès, INSEE (producteur), PROGEDO-ADISP (diffuseur)).

INSEE and Quetelet-Progedo do not allow for redistribution. Quetelet-Progedo
ask that you submit a formal request via <https://data.progedo.fr/> 
using your academic affiliation and email address.

You must follow the process below to obtain the data:

  - Request the extraction of the dataset [psm-0015] PSM État Civil – Deaths – 1968–2023
  - Ask Quetelet-Progedo to perform the following SAS requests

The name of the filed sended by Quetelet-Progedo should be `tableau_2.csv`. 

```
proc summary data=deces missing nway ;
class DOM CS GROUPAGE ADEC MDEC JDEC SEXE;
where DOM = METRO; /*Sélection France métropolitaine*/
output out = tableau_1 (drop=_type_ ) ;
run ;
```

Once this process is completed, processing the data can be reproduced using the script `4-Prep_Data_INSEE`.

## Computational requirements 

### Software Requirements

This replication package is based on the four data sources using `R (version 4.4.1)`.
To start, install `R`with  the correct version of the required packages. 
The program `0-Setup.R` installs all dependencies locally, and should be run once.
Remember to specify the correct path to the local version of the repository. For
that, replicator can change the line `folder <- getwd()` a the beginning of each
`R`-script.

### Memory and Runtime Requirements

The scripts are executed on:

  - a laptop (called PC) used Windows 11 (64-bit), equipped with an Intel Core Intel(R) Core(TM)
  i7-8665U CPU @ 1.90GHz 2.11 GHz, 16 GB of RAM.
  - a computer (called CLUST) running Debian GNU/Linux 10, equipped with an Intel(R)
  Xeon(R) CPU E5-2697A v4 @ 2.60G, 64 GB of RAM.

## Description of programs/code and instructions to replicators

The main contents of the replication package are the following:

   - `data_raw/`: folder of raw data files
   - `data/`: folder of processed data files
   - `multimomo/`: folder with the a custom version of the `R` MultiMoMo package
   - `functions/`: folder with our `R`-functions to produce and display our results
   - `figures/`: folder of generated plots as PDF or png files
   - `tables/`: folder with our `R`-functions to process table

The main scripts used to prepare the data and produce the results (46 figures and 4 tables)
are listed in the table below. After gathering the external data as described
in the section `Data availability and provenance`, the scripts should be
run following the script numbering. Each script contains short descriptions of 
the other scripts they execute. If necessary, please set your working directory
at the beginning of each script.

The expected runtimes are provided in the 'Estimated runtime' column,
distinguishing between execution times on the laptop PC and
on the computer CLUST. 

| Script files           | Short description                                                                 | Estimated runtime | Note                                                            |
|------------------------|------------------------------------------------------------------------------------|-------------------|-----------------------------------------------------------------|
| `0-Setup.R`   | Installs all dependencies locally, and should be run once. | 1 minute (PC)    | |
| `1-Prep_Data_Temp.R`   | Processes raw GHCN files to compute daily temperature indicators | 1 minute (PC)    | |
| `2-Prep_Data_Clim.R`   | Processes raw DRIAS files to compute daily heatwave indicators per model and scenario | 20–25 minutes (PC)   | |
| `3-Prep_Data_HMD.R`   | Loads from the HMD website and processes raw HMD files to get yearly mortality data | 1 minute (PC)  | |
| `4-Prep_Data_INSEE.R`   | Processes raw INSEE files to get daily death counts | 1 minute (PC)  | |
| `5-Main_Section_6.1.Rmd` | Produce results presented in Section 6.1  and in Appendix B | 1 minute  (PC)  |                         |
| `6-Main_Section_6.2.Rmd` | Produce results presented in Section 6.2, Appendix C.1 and Appendix C.2     | 12-13 hours for 1,000 simulations (PC) | To reduce computation time, your results could be generated with 100 simulations (approximately 75 minutes on the PC), see `nsim` variable in row 110. They are very similar to those presented in the paper. |
| `7-Main_Generate_Sensi`  | Conduct the sensitivity analysis presented in Appendix C.3.1.| 90 minutes (PC)  |                         |
| `8-Main_Appendix_C.3`    | Produce results presented in Appendix C.3.| 12 minutes (PC)      |                         |
| `9-Main_Section_6.3`    | Produce results presented in Section 6.3, Appendix D.1 and Appendix D.2.| 130 minutes (PC) |
| `10-Main_Project_Rates_France`    | Produce results presented in Section 6.3, Appendix D.1 and Appendix D.2.| 36 hours for 1,000 simulations (CLUST)  | To reduce computation time, your results could be generated with 20 simulations (approximately 360 minutes on the PC), see `nsim` variable in row 102. They are very similar to those presented in the paper.
| `11-Main_Project_Rates_City`    | Produce results presented in Section 6.3, Appendix D.1 and Appendix D.2.| 180 hours for 1,000 simulations (CLUST)  | To reduce computation time, your results could be with 20 simulations (approximately 30 hours on the PC), see `nsim` variable in row 103. They are very similar to those presented in the paper.
| `12-Main_Section_6.4`    | Produce results presented in Sections 6.4, 6.5 and in Appendix E and F| 2 minutes (PC)  | |

**Remark**: 

  - Note that all Rmd files include a section specifying the R session information
(version numbers and packages) used to produce the results
  - Our code is licensed under a Creative Commons Legal Code CC0 1.0 Universal.
See [LICENSE](LICENSE) for details.

## Results

To facilitate the replication of results, the table below describes all the
scripts used and the corresponding output files. All figures and tables presented
in the paper are reproduced with their respective numbering.
These results are stored in the `\figures` and `\tables` directories. 
They were generated through the execution of `RMarkdown` documents, 
allowing the figures and the tables to be integrated directly into `html` files
of the same name.

### List of Figures

| Figure # | Program        | Line Number | Output File       | Note           |
|----------|----------------|-------------|--------------------|----------------|
| Figure 1 | `5-Main_Section_6.1.Rmd`    | Row 148       | `fig1.pdf` and  5-Main_Section_6.1.html |             |
| Figure 2 | `5-Main_Section_6.1.Rmd`    | Row 191       | `fig2.pdf` and  5-Main_Section_6.1.html |             |
| Figure 3 | `5-Main_Section_6.1.Rmd`    | Row 243       | `fig3.pdf` and  5-Main_Section_6.1.html |             |
| Figure 4 | `5-Main_Section_6.1.Rmd`    | Row 295       | `fig4.pdf` and  5-Main_Section_6.1.html  |             |
| Figure 5 | `6-Main_Section_6.2.Rmd`    | Row 247       | `fig5.pdf` and  6-Main_Section_6.2.html |            |
| Figure 6 | `6-Main_Section_6.2.Rmd`    | Row 312       | `fig6.pdf` and  6-Main_Section_6.2.html |            |
| Figure 7 | `9-Main_Section_6.3.Rmd`    | Row 376       | `fig7.pdf` and  9-Main_Section_6.3.html  |             |
| Figure 8 | `9-Main_Section_6.3.Rmd`    | Row 396       | `fig8.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 9 | `9-Main_Section_6.3.Rmd`    | Row 511       | `fig8.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 10| `12-Main_Section_6.4.Rmd`    | Row 157       | `fig10.pdf` and  12-Main_Section_6.4.html |            |
| Figure 11| `12-Main_Section_6.4.Rmd`    | Row 178       | `fig11.pdf` and  12-Main_Section_6.4.html |            |
| Figure 12| `5-Main_Section_6.1.Rmd`    | Row 479       | `fig12.pdf` and  5-Main_Section_6.1.html |             |
| Figure 13| `6-Main_Section_6.2.Rmd`    | Row 335       | `fig13.pdf` and  6-Main_Section_6.2.html |            |
| Figure 14| `6-Main_Section_6.2.Rmd`    | Row 351       | `fig14.pdf` and  6-Main_Section_6.2.html |            
| Figure 15| `6-Main_Section_6.2.Rmd`    | Row 367       | `fig15.pdf` and  6-Main_Section_6.2.html |            
| Figure 16| `6-Main_Section_6.2.Rmd`    | Row 384       | `fig16.pdf` and  6-Main_Section_6.2.html |            
| Figure 17| `6-Main_Section_6.2.Rmd`    | Row 410       | `fig17.pdf` and  6-Main_Section_6.2.html |            
| Figure 18| `6-Main_Section_6.2.Rmd`    | Row 431       | `fig18.pdf` and  6-Main_Section_6.2.html |
| Figure 19| `6-Main_Section_6.2.Rmd`    | Row 447       | `fig19.pdf` and  6-Main_Section_6.2.html |
| Figure 20| `6-Main_Section_6.2.Rmd`    | Row 463       | `fig20.pdf` and  6-Main_Section_6.2.html |
| Figure 21| `6-Main_Section_6.2.Rmd`    | Row 480       | `fig21.pdf` and  6-Main_Section_6.2.html |
| Figure 22| `8-Main_Appendix_C.3.Rmd`    | Row 360       | `fig22.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 23| `8-Main_Appendix_C.3.Rmd`    | Row 375       | `fig23.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 24| `8-Main_Appendix_C.3.Rmd`    | Row 498       | `fig24.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 25| `8-Main_Appendix_C.3.Rmd`    | Row 514       | `fig25.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 26| `8-Main_Appendix_C.3.Rmd`    | Row 637       | `fig26.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 27| `8-Main_Appendix_C.3.Rmd`    | Row 652       | `fig27.pdf` and  8-Main_Appendix_C.3.html |             |
| Figure 28| `9-Main_Section_6.3.Rmd`    | Row 699       | `fig28.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 29| `9-Main_Section_6.3.Rmd`    | Row 730       | `fig29.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 30| `9-Main_Section_6.3.Rmd`    | Row 1132       | `fig30.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 31| `9-Main_Section_6.3.Rmd`    | Row 1232       | `fig31.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 32| `9-Main_Section_6.3.Rmd`    | Row 1268       | `fig32.pdf` and  9-Main_Section_6.3.html  |            |
| Figure 33| `12-Main_Section_6.4.Rmd`    | Row 197       | `fig33.pdf` and  12-Main_Section_6.4.html |            |
| Figure 34| `12-Main_Section_6.4.Rmd`    | Row 207       | `fig34.pdf` and  12-Main_Section_6.4.html |            |
| Figure 35| `12-Main_Section_6.4.Rmd`    | Row 217       | `fig35.pdf` and  12-Main_Section_6.4.html |            |
| Figure 36| `12-Main_Section_6.4.Rmd`    | Row 227       | `fig36.pdf` and  12-Main_Section_6.4.html |            |
| Figure 37| `12-Main_Section_6.4.Rmd`    | Row 256       | `fig37.pdf` and  12-Main_Section_6.4.html |            |
| Figure 38| `12-Main_Section_6.4.Rmd`    | Row 278       | `fig38.pdf` and  12-Main_Section_6.4.html |            |
| Figure 39| `12-Main_Section_6.4.Rmd`    | Row 300       | `fig39.pdf` and  12-Main_Section_6.4.html |            |
| Figure 40| `12-Main_Section_6.4.Rmd`    | Row 322       | `fig40.pdf` and  12-Main_Section_6.4.html |            |
| Figure 41| `12-Main_Section_6.4.Rmd`    | Row 344       | `fig41.pdf` and  12-Main_Section_6.4.html |            |
| Figure 42| `12-Main_Section_6.4.Rmd`    | Row 369       | `fig42.pdf` and  12-Main_Section_6.4.html |            |
| Figure 43| `12-Main_Section_6.4.Rmd`    | Row 391       | `fig43.pdf` and  12-Main_Section_6.4.html |            |
| Figure 44| `12-Main_Section_6.4.Rmd`    | Row 412       | `fig44.pdf` and  12-Main_Section_6.4.html |            |
| Figure 45| `12-Main_Section_6.4.Rmd`    | Row 433       | `fig45.pdf` and  12-Main_Section_6.4.html |            |
| Figure 46| `12-Main_Section_6.4.Rmd`    | Row 454       | `fig46.pdf` and  12-Main_Section_6.4.html |            |


### List of Tables

| Table # | Program        | Line Number | Output File       | Note           |
|----------|----------------|-------------|--------------------|----------------|
| Table 1 | -    | -       | - | This table contains only qualitative information             |
| Table 2 | `9-Main_Section_6.3.Rmd`    | Row 433       | `tab2.tex` and  9-Main_Section_6.3.html |             |
| Table 3 | `9-Main_Section_6.3.Rmd`    | Row 483       | `tab3.tex` and  9-Main_Section_6.3.html |             |
| Table 4 | -    | -       | - | This table contains only descriptive information             |
| Table 5 |`8-Main_Appendix_C.3.Rmd`    | Row 250       | `tab5.tex` and  8-Main_Appendix_C.3.html |             |
| Table 6 |`9-Main_Section_6.3.Rmd`    | Row 1294       | `tab3.tex` and  9-Main_Section_6.3.html |             |


# References

   -  DRIAS (2020). DRIAS, Les futurs du climat. https://www.drias-climat.fr.
   -  INSEE (2020). Etat civil - Fichier d´etail Décès, INSEE (producteur), PROGEDO-ADISP (diffuseur). doi: 10.13144/psm-0015.
   -  HMD (2024). Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org (data downloaded on 2024-01-19).
   -  Menne, M. J. et al. (2012). Global Historical Climatology Network - Daily (GHCN-Daily), Version 3. doi: 10.7289/V5D21VHZ.  
