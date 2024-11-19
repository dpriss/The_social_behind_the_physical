# This script provides the ERGMs for the individual periods as presented in the SI of the paper. The summary of the models is given in Table 1 of the SI and the gof plots are presented in Figure 5 - 10.

library(ergm)
library(network)
library(readxl)
library(statnet)
library(igraph)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggnetwork)
library(tidygraph)
library(intergraph)
library(purrr)
library(texreg)

# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] openxlsx_4.2.5     viridis_0.6.2      viridisLite_0.4.1  cowplot_1.1.1      arsenal_3.6.3      ggplot2_3.4.0     
# [7] RColorBrewer_1.1-3 readxl_1.4.1       network_1.18.0     xts_0.12.2         zoo_1.8-11         purrr_0.3.4       
# [13] R.utils_2.12.2     R.oo_1.25.0        R.methodsS3_1.8.2  sf_1.0-7           dplyr_1.0.9        plyr_1.8.7        
# [19] igraph_1.3.5      
# 
# loaded via a namespace (and not attached):
#   [1] Rdpack_2.4              assertthat_0.2.1        cellranger_1.1.0        yaml_2.3.5              robustbase_0.95-0      
# [6] pillar_1.9.0            lattice_0.20-44         glue_1.6.2              digest_0.6.29           rbibutils_2.2.13       
# [11] colorspace_2.0-3        htmltools_0.5.2         Matrix_1.4-1            pkgconfig_2.0.3         ergm_4.4.0             
# [16] scales_1.2.1            intergraph_2.0-2        tibble_3.1.7            proxy_0.4-27            generics_0.1.3         
# [21] ellipsis_0.3.2          withr_2.5.0             cachem_1.0.6            cli_3.4.1               magrittr_2.0.3         
# [26] statnet.common_4.8.0    memoise_2.0.1           evaluate_0.21           fansi_1.0.3             MASS_7.3-54            
# [31] class_7.3-19            tools_4.1.0             lifecycle_1.0.3         munsell_0.5.0           trust_0.1-8            
# [36] zip_2.2.1               compiler_4.1.0          e1071_1.7-13            rlang_1.0.6             classInt_0.4-7         
# [41] units_0.8-0             grid_4.1.0              rstudioapi_0.14         rmarkdown_2.17          gtable_0.3.1           
# [46] DBI_1.1.3               R6_2.5.1                gridExtra_2.3           lpSolveAPI_5.5.2.0-17.8 rle_0.9.2              
# [51] knitr_1.42              fastmap_1.1.0           utf8_1.2.2              KernSmooth_2.23-20      stringi_1.7.12         
# [56] parallel_4.1.0          Rcpp_1.0.10             vctrs_0.5.2             DEoptimR_1.0-13         tidyselect_1.2.0       
# [61] xfun_0.39               coda_0.19-4 

#function to remove the X from the column names of imported .csv files
destroyX = function(es) {
  f = es
  for (col in c(1:ncol(f))) {
    #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  {
      #if starts with 'X' ..
      colnames(f)[col] <-
        substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}

#Data import----
# EDGELISTS 
# define source file for edgelists 
path <- "edgelists_hybrid.xlsx"

# import sheets into list
sheets <- path %>% 
  ## get the names of the excel sheets
  excel_sheets() %>%
  ## set the names
  set_names() %>% 
  ## read sheets into list
  map(read_excel, path = path)

# unlist the list of edgelists into the environment
list2env(sheets, envir = .GlobalEnv)

# put only the relevant files into a new list
edgelists <- list(EBA1, EBA2, MBA, LBA, IA1, IA2)

# set the names of the files
names(edgelists) <- list("EBA1", "EBA2", "MBA", "LBA", "IA1", "IA2")



#ATTRIBUTES
# find the attribute files for the periods
all_attr <- list.files(pattern = "sites_\\w+_hybrid.csv", full.names = TRUE)

# read csv into list 
list_all_attr <- lapply(all_attr, read.csv)

# set the names of the files 
names(list_all_attr) <- gsub(".csv", "", gsub("_hybrid", "", list.files(pattern = "sites_\\w+_hybrid.csv", full.names = F)))

# create new ID column to match the edgelists
list_all_attr <- 
  map(list_all_attr, ~ .x %>%
        relocate(id))
names(list_all_attr) <- list("EBA1", "EBA2", "IA1", "IA2", "LBA", "MBA")


#NETWORKS
x <- 1
for (elist in edgelists) {
  # extract the period from the edgelist
  period <- names(edgelists[x])
  # find the attribute for the period
  vert.attr <- list_all_attr[grepl(period, names(list_all_attr))]
  # #subset attribute table according to edgelist - edgelist/matrix and
  # attribute table need to have the same dimension
  #vert.attr <- as.data.frame(subset(vert.attr[[1]], vert.attr[[1]]$id %in% rownames(edges)))
  vert.attr <- as.data.frame(vert.attr[[1]])
  # change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
  size_bins <- subset(vert.attr, vert.attr$sizeCat == 1 | vert.attr$sizeCat == 2 | 
                        vert.attr$sizeCat == 3)
  vert.attr <- vert.attr%>% 
    mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                            TRUE ~ Size))
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))

  # create network
  nw <- intergraph::asNetwork(ig)
  # save vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  va <- paste("vert.attr", period, sep = "_")
  data_va <- vert.attr
  assign(va, data_va)
    
  lon <- paste("lo", period, sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
    
  nwn <- paste("nw", period, sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)

  x <- x + 1
}

list_nw <- list(nw_EBA1, nw_EBA2, nw_MBA, nw_LBA, nw_IA1, nw_IA2)


#DISTANCE MATRICES
# period-specific distance matrices, i.e. subset to the nodes in the respective period. Used for ERGMs because a distance matrix for all nodes would lead to wrong assignments of distances.
DistMats_periods <- list.files(pattern = "\\w+_DistMat_hybrid.csv$", full.names = TRUE)
# create list of csv
list_DistMats <- lapply(DistMats_periods, read.csv, row.names = 1)
# row and column names are imported with an X because they are numbers. This function removes them.
list_DistMats <- lapply(list_DistMats, destroyX)
# add names to the csvs
names(list_DistMats) <- gsub("_hybrid.csv", "", list.files(pattern = "\\w+_DistMat_hybrid.csv$", full.names = F))
# add to environment as matrices
list2env(lapply(list_DistMats, as.matrix),envir = .GlobalEnv)


# EBA1
# interaction term nodematch("Datst_1"):absdiff("Size") excluded because it throws an error: Error in T2nullity && verbose: invalid 'x' type in 'x && y'
ergm_multi.EBA1 <- ergm(nw_EBA1 ~ edges + degree(2) + degree(6) + gwnsp(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + gwdegree(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(EBA1_DistMat) + edgecov(EBA1_DistMat):smalldiff("Size", cutoff = 5), 
                        control = control.ergm(MCMC.samplesize = 30000, seed = 123))
summary(ergm_multi.EBA1)
ergm_multi.EBA1_gof <- ergm::gof(ergm_multi.EBA1)

png("Interaction_EBA1_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.EBA1_gof)
dev.off()

# EBA2
ergm_multi.EBA2 <- ergm(nw_EBA2 ~ edges + degree1.5 + gwnsp(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + gwdegree(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(EBA2_DistMat) + edgecov(EBA2_DistMat):absdiff("Size") + edgecov(EBA2_DistMat):smalldiff("Size", cutoff = 5),
                        control = control.ergm(MCMC.samplesize = 30000, seed = 12345))
summary(ergm_multi.EBA2)
ergm_multi.EBA2_gof <- gof(ergm_multi.EBA2)

png("Interaction_EBA2_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.EBA2_gof)
dev.off()

# MBA
ergm_multi.MBA <- ergm(nw_MBA ~ edges + degree1.5 + degrange(4, to = 7) + gwnsp(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + gwdegree(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(MBA_DistMat) + edgecov(MBA_DistMat):absdiff("Size") + edgecov(MBA_DistMat):smalldiff("Size", cutoff = 5),
    control = control.ergm(MCMC.samplesize = 30000, seed = 12345))
summary(ergm_multi.MBA)
ergm_multi.MBA_gof <- gof(ergm_multi.MBA)

png("Interaction_MBA_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.MBA_gof)
dev.off()


# LBA
ergm_multi.LBA <- ergm(nw_LBA ~ edges + isolates + degree(4) + gwesp(decay = 0.95, fixed = T) + gwdegree(decay = 0.95, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(LBA_DistMat) + edgecov(LBA_DistMat):absdiff("Size") + edgecov(LBA_DistMat):smalldiff("Size", cutoff = 5),
    control = control.ergm(MCMC.samplesize = 30000, seed = 12345))
summary(ergm_multi.LBA)
ergm_multi.LBA_gof <- gof(ergm_multi.LBA)

png("Interaction_LBA_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.LBA_gof)
dev.off()


# IA1
ergm_multi.IA1 <- ergm(nw_IA1 ~ edges + degree(6) + gwnsp(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + gwdegree(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(IA1_DistMat) + edgecov(IA1_DistMat):absdiff("Size") + edgecov(IA1_DistMat):smalldiff("Size", cutoff = 5),
    control = control.ergm(MCMC.samplesize = 30000, seed = 12345))
summary(ergm_multi.IA1)
ergm_multi.IA1_gof <- gof(ergm_multi.IA1)

png("Interaction_IA1_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.IA1_gof)
dev.off()

# IA2
ergm_multi.IA2 <- ergm(nw_IA2 ~ edges + concurrent + degrange(4, to = 11) + gwesp(decay = 0.5, fixed = T) + gwdegree(decay = 0.5, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(IA2_DistMat) + edgecov(IA2_DistMat):absdiff("Size") + edgecov(IA2_DistMat):smalldiff("Size", cutoff = 5),
    control = control.ergm(MCMC.samplesize = 50000, seed = 12345))
summary(ergm_multi.IA2)
ergm_multi.IA2_gof <- gof(ergm_multi.IA2)

png("Interaction_IA2_hybrid.png", width = 600, height = 600, res = 100)
par(mfrow = c(2, 2))
plot(ergm_multi.IA2_gof)
dev.off()

texreg(list(ergm_multi.EBA1, ergm_multi.EBA2, ergm_multi.MBA, ergm_multi.LBA, ergm_multi.IA1, ergm_multi.IA2), digits = 6, file = "Multi_ERGM_hybrid_all.txt")
wordreg(list(ergm_multi.EBA1, ergm_multi.EBA2, ergm_multi.MBA, ergm_multi.LBA, ergm_multi.IA1, ergm_multi.IA2), digits = 6, file = "Multi_ERGM_hybrid_all.doc")
