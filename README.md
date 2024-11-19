# Introduction 
This repository contains the data and scripts used for the analysis presented in the paper Priß, D., Prell, C., Lawrence, D., Wainwright, J., Turnbull, L. "The social behind the physical - Assessing tie formation processes of ancient route systems", published in the Journal of Archaeological Science. 

# Content
The repository contains one folder with Data, one with Plots and one with Scripts. 

  * Data: Input for the scripts
  
  * Plots: Plots of network graphs and gof grpahs created in the script "3. MTERGM_hybrid.R"
  
  * Scripts: 
    + 1.DNA.R - Script for a descriptive network analysis of the networks. This scirpt is meant to help the reader get accustomed with the data. 
    + 2.ERGM_periods_hybrid.R - Script to implement ERGMs for the individual time periods (waves) analysed in the paper.
    + 3.MTERGM_hybrid.R - Script to implement the MTERGM. 

# Software
For the Scripts, the following software was used:

1.DNA.R
    
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


 2.ERGM_periods_hybrid.R

    R version 4.1.0 (2021-05-18)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19045)

    Matrix products: default

    locale:
    [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
    [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] openxlsx_4.2.5     viridis_0.6.2      viridisLite_0.4.1  cowplot_1.1.1      arsenal_3.6.3      ggplot2_3.4.0     
     [7] RColorBrewer_1.1-3 readxl_1.4.1       network_1.18.0     xts_0.12.2         zoo_1.8-11         purrr_0.3.4       
    [13] R.utils_2.12.2     R.oo_1.25.0        R.methodsS3_1.8.2  sf_1.0-7           dplyr_1.0.9        plyr_1.8.7        
    [19] igraph_1.3.5      

    loaded via a namespace (and not attached):
     [1] Rdpack_2.4              assertthat_0.2.1        cellranger_1.1.0        yaml_2.3.5              robustbase_0.95-0      
     [6] pillar_1.9.0            lattice_0.20-44         glue_1.6.2              digest_0.6.29           rbibutils_2.2.13       
    [11] colorspace_2.0-3        htmltools_0.5.2         Matrix_1.4-1            pkgconfig_2.0.3         ergm_4.4.0             
    [16] scales_1.2.1            intergraph_2.0-2        tibble_3.1.7            proxy_0.4-27            generics_0.1.3         
    [21] ellipsis_0.3.2          withr_2.5.0             cachem_1.0.6            cli_3.4.1               magrittr_2.0.3         
    [26] statnet.common_4.8.0    memoise_2.0.1           evaluate_0.21           fansi_1.0.3             MASS_7.3-54            
    [31] class_7.3-19            tools_4.1.0             lifecycle_1.0.3         munsell_0.5.0           trust_0.1-8            
    [36] zip_2.2.1               compiler_4.1.0          e1071_1.7-13            rlang_1.0.6             classInt_0.4-7         
    [41] units_0.8-0             grid_4.1.0              rstudioapi_0.14         rmarkdown_2.17          gtable_0.3.1           
    [46] DBI_1.1.3               R6_2.5.1                gridExtra_2.3           lpSolveAPI_5.5.2.0-17.8 rle_0.9.2              
    [51] knitr_1.42              fastmap_1.1.0           utf8_1.2.2              KernSmooth_2.23-20      stringi_1.7.12         
    [56] parallel_4.1.0          Rcpp_1.0.10             vctrs_0.5.2             DEoptimR_1.0-13         tidyselect_1.2.0       
    [61] xfun_0.39               coda_0.19-4 

3.MTERGM_hybrid.R

    R version 4.1.0 (2021-05-18)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows 10 x64 (build 19045)
    
    Matrix products: default
    
    locale:
    [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
    [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
    
    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     
    
    other attached packages:
     [1] texreg_1.39.3         intergraph_2.0-2      tidygraph_1.2.2       ggnetwork_0.5.12      plotly_4.10.1        
     [6] statnet_2019.6        tsna_0.3.5            sna_2.7-1             statnet.common_4.8.0  ergm.count_4.1.1     
    [11] tergm_4.1.1           networkDynamic_0.11.3 ergm_4.4.0            openxlsx_4.2.5        viridis_0.6.2        
    [16] viridisLite_0.4.1     cowplot_1.1.1         arsenal_3.6.3         ggplot2_3.4.0         RColorBrewer_1.1-3   
    [21] readxl_1.4.1          network_1.18.0        xts_0.12.2            zoo_1.8-11            purrr_0.3.4          
    [26] R.utils_2.12.2        R.oo_1.25.0           R.methodsS3_1.8.2     sf_1.0-7              dplyr_1.0.9          
    [31] plyr_1.8.7            igraph_1.3.5         
    
    loaded via a namespace (and not attached):
     [1] nlme_3.1-152            httr_1.4.4              tools_4.1.0             utf8_1.2.2              R6_2.5.1               
     [6] KernSmooth_2.23-20      lazyeval_0.2.2          DBI_1.1.3               colorspace_2.0-3        withr_2.5.0            
    [11] tidyselect_1.2.0        gridExtra_2.3           compiler_4.1.0          cli_3.4.1               scales_1.2.1           
    [16] DEoptimR_1.0-13         classInt_0.4-7          robustbase_0.95-0       proxy_0.4-27            digest_0.6.29          
    [21] rmarkdown_2.17          pkgconfig_2.0.3         htmltools_0.5.2         fastmap_1.1.0           htmlwidgets_1.5.4      
    [26] rlang_1.0.6             rstudioapi_0.14         generics_0.1.3          jsonlite_1.8.0          zip_2.2.1              
    [31] magrittr_2.0.3          Matrix_1.4-1            Rcpp_1.0.10             munsell_0.5.0           fansi_1.0.3            
    [36] lifecycle_1.0.3         stringi_1.7.12          yaml_2.3.5              networkLite_1.0.5       MASS_7.3-54            
    [41] grid_4.1.0              parallel_4.1.0          lattice_0.20-44         lpSolveAPI_5.5.2.0-17.8 knitr_1.42             
    [46] pillar_1.9.0            rle_0.9.2               glue_1.6.2              evaluate_0.21           trust_0.1-8            
    [51] data.table_1.14.2       vctrs_0.5.2             Rdpack_2.4              cellranger_1.1.0        tidyr_1.2.0            
    [56] gtable_0.3.1            assertthat_0.2.1        cachem_1.0.6            xfun_0.39               rbibutils_2.2.13       
    [61] e1071_1.7-13            coda_0.19-4             class_7.3-19            tibble_3.1.7            memoise_2.0.1          
    [66] units_0.8-0             ellipsis_0.3.2 

# How to use 

We advise to work throught the script 1.DNA.R first as it provides the code for an exploratory analysis. We first import the files which can found in the Data folder before calculating selected network metrics. Those metrics are not mentioned in the paper but instead should just help the reader to understand the networks. We then proceed by creating some plots of the networks with different metrics such as degree and site size to compare and visuslise them. The basic plots of the networks are presented in Figure 3 while the other plots again just serve as a means of orientation. 

The other two scripts can be run in any order, depending on the reader's preferences. The script 2.ERGM_periods_hybrid.R contains the code for the ERGMs for the cross-sectional networks, i.e. for the six periods. Every network is analysed independently. We present the results of the models and the gof plots in the SI of the paper (Table 1 and Figure 5 - 10) because those models are just additional information for the interested reader and are not explained in detail in the paper itself. They were added on a reviewer's request and we think that they supplement the paper very well.

Script 3.MTERGM_hybrid.R contains the main analysis presented in the paper, i.e. the MTERMG. We first provide the simple models with and without temporal dependencies, i.e. the models that just include the hypotheses mentioned in the paper. We then present the fitted models, i.e. the models that include additonal ERGM terms that improve the gof. The results of the fitted models are given in Table 3 while the gof plots are shown in the SI, Figure 1 - 4. 

