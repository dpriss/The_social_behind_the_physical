# Introduction 
This repository contains the data and scripts used for the analysis presented in the paper Priß, D., Prell, C., Lawrence, D., Wainwright, J., Turnbull, L. "The social behind the physical - Assessing tie formation processes of ancient route systems", published in the Journal of Archaeological Science. 

Analysing and understanding connectivity of human social networks of (ancient) societies offers new perspectives on their functioning. However, social network approaches in archaeology rarely utilise formal statistical models to test established theories or develop new hypotheses. In this paper, we present the implementation of MCMC-MLE Temporal Exponential Random Graph Models (MTERGMs) to investigate the hollow way network between settlements of the Bronze and Iron Age Khabur Valley, Mesopotamia. Using MTERGMs, we evaluate eight hypotheses to assess which network patterns explain the formation of the hollow ways. Our results show that in the cross-sectional networks, preferential attachment, transitivity, distance and site size are important factors for tie formation while the longitudinal analysis reveals tie persistence over time with distance and transitivity being significant for tie formation. We reflect on these findings as well as the limitations of our dataset and conclude that TERGMs are useful tools to formally evaluate archaeological theories pertaining to network structures and processes, if the available data are sufficiently complete.  

# Content
The repository contains one folder with Data, one with Plots and one with Scripts. 
  
  * Plots: Plots of network graphs and goodness-of-fit (gof) graphs created in the script "3. MTERGM_hybrid.R"
  
  * Data & Scripts:
    + Input data for the scripts
    + 1.DNA.R - Script for a descriptive network analysis of the networks. This script is meant to help the reader get accustomed with the data.
    + 2.ERGM_periods_hybrid.R - Script to implement ERGMs for the individual time periods (waves) analysed in the paper.
    + 3.MTERGM_hybrid.R - Script to implement the MTERGM.  

# Software
For the Scripts, the following software was used:
    
    R version 4.5.1 (2025-06-13 ucrt)
	Platform: x86_64-w64-mingw32/x64
	Running under: Windows 11 x64 (build 26100)

	Matrix products: default
  	LAPACK version 3.12.1

	locale:
	[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
	[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8    

	time zone: Europe/London
	tzcode source: internal

	attached base packages:
	[1] stats     graphics  grDevices utils     datasets  methods   base     

	other attached packages:
 	[1] openxlsx_4.2.8     viridis_0.6.5      viridisLite_0.4.2  cowplot_1.2.0      arsenal_3.6.3      ggplot2_3.5.2      RColorBrewer_1.1-3
 	[8] readxl_1.4.5       network_1.19.0     xts_0.14.1         zoo_1.8-14         purrr_1.1.0        R.utils_2.13.0     R.oo_1.27.1       
	[15] R.methodsS3_1.8.2  sf_1.0-21          dplyr_1.1.4        plyr_1.8.9         igraph_2.1.4      

	loaded via a namespace (and not attached):
 	[1] generics_0.1.4        class_7.3-23          KernSmooth_2.23-26    stringi_1.8.7         lattice_0.22-7        intergraph_2.0-4     
 	[7] digest_0.6.37         magrittr_2.0.3        statnet.common_4.12.0 evaluate_1.0.4        grid_4.5.1            fastmap_1.2.0        
	[13] cellranger_1.1.0      zip_2.3.3             e1071_1.7-16          DBI_1.2.3             gridExtra_2.3         scales_1.4.0         
	[19] cli_3.6.5             rlang_1.1.6           units_0.8-7           withr_3.0.2           yaml_2.3.10           tools_4.5.1          
	[25] coda_0.19-4.1         vctrs_0.6.5           R6_2.6.1              proxy_0.4-27          lifecycle_1.0.4       classInt_0.4-11      
	[31] pkgconfig_2.0.3       pillar_1.11.0         gtable_0.3.6          glue_1.8.0            Rcpp_1.1.0            xfun_0.52            
	[37] tibble_3.3.0          tidyselect_1.2.1      rstudioapi_0.17.1     knitr_1.50            farver_2.1.2          htmltools_0.5.8.1    
	[43] rmarkdown_2.29        compiler_4.5.1


# How to use 

We advise working through the script 1.DNA.R first as it provides the code for an exploratory analysis. We first import the data files which can be found in the Data & Scripts folder before creating some plots of the networks with different metrics such as degree and site size to compare and visualise them. The basic plots of the networks are presented in Figure 3 while the other plots just serve as a means of orientation. We then proceed by calculating selected network metrics. Those metrics are not mentioned in the paper but instead should just help the reader to understand the networks.  

The other two scripts can be run in any order, depending on the reader's preferences. The script 2.ERGM_periods_hybrid.R contains the code for the ERGMs for the cross-sectional networks, i.e. for the six time periods analysed in the paper. Every network is analysed independently. We present the results of the models and the gof plots in the SI of the paper (Table 1 and Figure 5 - 10) because those models are just additional information for the interested reader and are not explained in detail in the paper itself. They were added on a reviewer's request and we think that they supplement the paper very well.

Script 3.MTERGM_hybrid.R contains the main analysis presented in the paper, i.e. the MTERMG. We first provide the simple models with and without temporal dependencies, i.e. the models that just include the hypotheses mentioned in the paper. We then present the fitted models, i.e. the models that include additional ERGM terms that improve the gof. The results of the fitted models are given in Table 3 while the gof plots are shown in the SI, Figure 1 - 4. 

Running script 1.DNA.R after the other two scripts will produce an error (because of conflicts in package function names).  To re-run this script, we recommend restarting the R session to clear the conflicting loaded libraries. 


## Running the scripts 

There are two options to run the scripts, either download them and run them locally or use the provided Dockerfile:

If you run the scripts in your local RStudio or R console, please use `setwd` before running the script to change the working directory to the folder that contains the scripts.  We do not recommend using R in command line mode to run the scripts. 

Please note that to use the Dockerfile, you will need to set the current directory before building the project (i.e. by typing `cd /path` into the Docker terminal, where the path is the folder containing the Dockerfile, scripts and data folder) before following the instructions in the Dockerfile. 
