# Introduction 
This repository contains the data and scripts used for the analysis presented in the paper Priß, D., Prell, C., Lawrence, D., Wainwright, J., Turnbull, L. "The social behind the physical - Assessing tie formation processes of ancient route systems", published in the Journal of Archaeological Science. 

# Content
The repository contains one folder with Data, one with Plots and one with Scripts. 
  *Data: Input for the scripts
  *Plots: Plots of network graphs and gof grpahs created in the script "3. MTERGM_hybrid.R"
  *Scripts: 
    *"1.DNA.R" - Script for a descriptive network analysis of the networks. This scirpt is meant to help the reader get accustomed with the data. 
    *"2.ERGM_periods_hybrid.R" - Script to implement ERGMs for the individual time periods (waves) analysed in the paper.
    *"3. MTERGM_hybrid.R" - Script to implement the MTERGM. 

# Software
For the Scripts, the following software was used:
  *"1.DNA.R"
    
    R version 4.1.0 (2021-05-18)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19045)

    Matrix products: default
 
    locale:
    [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
    [5] LC_TIME=German_Germany.1252    

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] network_1.18.0    xts_0.12.2        zoo_1.8-11        purrr_0.3.4       R.utils_2.12.2    R.oo_1.25.0       R.methodsS3_1.8.2 sf_1.0-7         
    [9] dplyr_1.0.9       igraph_1.3.5     

    loaded via a namespace (and not attached):
    [1] Rcpp_1.0.10          pillar_1.9.0         compiler_4.1.0       class_7.3-19         tools_4.1.0          lifecycle_1.0.3      tibble_3.1.7        
    [8] lattice_0.20-44      pkgconfig_2.0.3      rlang_1.0.6          DBI_1.1.3            cli_3.4.1            rstudioapi_0.14      coda_0.19-4         
    [15] e1071_1.7-13         generics_0.1.3       vctrs_0.5.2          classInt_0.4-7       grid_4.1.0           tidyselect_1.2.0     glue_1.6.2          
    [22] R6_2.5.1             fansi_1.0.3          magrittr_2.0.3       ellipsis_0.3.2       units_0.8-0          assertthat_0.2.1     utf8_1.2.2          
    [29] KernSmooth_2.23-20   proxy_0.4-27         statnet.common_4.7.0
