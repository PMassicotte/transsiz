# Transsiz

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3364144.svg)](https://doi.org/10.5281/zenodo.3364144)

This is the repository for the research paper **Sensitivity of phytoplankton primary production estimates to available irradiance under heterogeneous sea-ice condition** which can be found here: https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JC015007

Please cite the paper as: 

Massicotte, P., Peeken, I., Katlein, C., Flores, H., Huot, Y., Castellani, G., et al. (2019). Sensitivity of phytoplankton primary production estimates to available irradiance under heterogeneous sea ice conditions. Journal of Geophysical Research: Oceans, 124. https://doi.org/ 10.1029/2019JC015007

## Data

The data is available on [Pangaea web site](https://www.pangaea.de/):

- ROV data (https://doi.pangaea.de/10.1594/PANGAEA.861048)

- SUIT data (submitted to Pangaea)

- Incident radiation (https://doi.pangaea.de/10.1594/PANGAEA.849663)

- Station list (https://doi.pangaea.de/10.1594/PANGAEA.848841)

- Photosynthetic parameters (https://doi.org/10.1594/PANGAEA.899842)

- Sea-ice/snow thickness (https://doi.pangaea.de/10.1594/PANGAEA.897958)

## Source code

R 3.6.0 has been used to for this project. The script file `R/main.R` contains the recipes to reproduce all the analyses and figures of the article.

## SessionInfo

Print version information about R, the OS and attached or loaded packages.

<details>

```r
> sessionInfo()
R version 3.6.0 (2019-04-26)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Linux Mint 19.1

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so

locale:
 [1] LC_CTYPE=en_CA.UTF-8       LC_NUMERIC=C               LC_TIME=en_CA.UTF-8        LC_COLLATE=en_CA.UTF-8     LC_MONETARY=en_CA.UTF-8   
 [6] LC_MESSAGES=en_CA.UTF-8    LC_PAPER=en_CA.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_CA.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] xtable_1.8-2          pbmcapply_1.4.1       data.table_1.12.2     furrr_0.1.0.9002      future_1.12.0         readxl_1.3.1          multidplyr_0.0.0.9000
 [8] sf_0.7-4              extrafont_0.17        feather_0.3.3         forcats_0.4.0         stringr_1.4.0         dplyr_0.8.0.1         purrr_0.3.2          
[15] readr_1.3.1           tidyr_0.8.3.9000      tibble_2.1.1          ggplot2_3.1.1         tidyverse_1.2.1       MASS_7.3-51.1        

loaded via a namespace (and not attached):
 [1] nlme_3.1-139       fs_1.3.1           usethis_1.5.0      lubridate_1.7.4    devtools_2.0.2     httr_1.4.0         rprojroot_1.3-2    tools_3.6.0       
 [9] backports_1.1.4    R6_2.4.0           KernSmooth_2.23-15 DBI_1.0.0          lazyeval_0.2.2     colorspace_1.4-1   withr_2.1.2        tidyselect_0.2.5  
[17] prettyunits_1.0.2  processx_3.3.1     curl_3.3           compiler_3.6.0     git2r_0.24.0       extrafontdb_1.0    cli_1.1.0          rvest_0.3.3       
[25] xml2_1.2.0         desc_1.2.0         scales_1.0.0       classInt_0.3-3     callr_3.2.0        digest_0.6.18      rmarkdown_1.12     pkgconfig_2.0.2   
[33] htmltools_0.3.6    sessioninfo_1.1.1  styler_1.1.1       rlang_0.3.4.9003   rstudioapi_0.10    generics_0.0.2     jsonlite_1.6       magrittr_1.5      
[41] Rcpp_1.0.1         munsell_0.5.0      clipr_0.6.0        stringi_1.4.3      yaml_2.2.0         pkgbuild_1.0.2     plyr_1.8.4         grid_3.6.0        
[49] listenv_0.7.0      crayon_1.3.4       lattice_0.20-38    haven_2.1.0        hms_0.4.2          zeallot_0.1.0      knitr_1.22         ps_1.3.0          
[57] pillar_1.3.1       datapasta_3.0.0    pkgload_1.0.2      codetools_0.2-16   clisymbols_1.2.0   glue_1.3.1         packrat_0.5.0      evaluate_0.13     
[65] remotes_2.0.2      modelr_0.1.4       vctrs_0.1.0.9003   testthat_2.1.1     Rttf2pt1_1.3.7     cellranger_1.1.0   gtable_0.3.0       rematch2_2.0.1    
[73] assertthat_0.2.1   xfun_0.6           broom_0.5.2        e1071_1.7-1        class_7.3-15       tinytex_0.12       memoise_1.1.0      units_0.6-3       
[81] globals_0.12.4     here_0.1    
```
</details>

## Code of conduct

Please note that the 'transsiz' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
