# This script provides the MTERGM analysis for the paper. The model results are given in Table 3 and the gof plots can be found in the SI, Figure 1 - 4.

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

library(btergm)
library(tergm)

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
#   [1] texreg_1.39.3         intergraph_2.0-2      tidygraph_1.2.2       ggnetwork_0.5.12      plotly_4.10.1        
# [6] statnet_2019.6        tsna_0.3.5            sna_2.7-1             statnet.common_4.8.0  ergm.count_4.1.1     
# [11] tergm_4.1.1           networkDynamic_0.11.3 ergm_4.4.0            openxlsx_4.2.5        viridis_0.6.2        
# [16] viridisLite_0.4.1     cowplot_1.1.1         arsenal_3.6.3         ggplot2_3.4.0         RColorBrewer_1.1-3   
# [21] readxl_1.4.1          network_1.18.0        xts_0.12.2            zoo_1.8-11            purrr_0.3.4          
# [26] R.utils_2.12.2        R.oo_1.25.0           R.methodsS3_1.8.2     sf_1.0-7              dplyr_1.0.9          
# [31] plyr_1.8.7            igraph_1.3.5         
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-152            httr_1.4.4              tools_4.1.0             utf8_1.2.2              R6_2.5.1               
# [6] KernSmooth_2.23-20      lazyeval_0.2.2          DBI_1.1.3               colorspace_2.0-3        withr_2.5.0            
# [11] tidyselect_1.2.0        gridExtra_2.3           compiler_4.1.0          cli_3.4.1               scales_1.2.1           
# [16] DEoptimR_1.0-13         classInt_0.4-7          robustbase_0.95-0       proxy_0.4-27            digest_0.6.29          
# [21] rmarkdown_2.17          pkgconfig_2.0.3         htmltools_0.5.2         fastmap_1.1.0           htmlwidgets_1.5.4      
# [26] rlang_1.0.6             rstudioapi_0.14         generics_0.1.3          jsonlite_1.8.0          zip_2.2.1              
# [31] magrittr_2.0.3          Matrix_1.4-1            Rcpp_1.0.10             munsell_0.5.0           fansi_1.0.3            
# [36] lifecycle_1.0.3         stringi_1.7.12          yaml_2.3.5              networkLite_1.0.5       MASS_7.3-54            
# [41] grid_4.1.0              parallel_4.1.0          lattice_0.20-44         lpSolveAPI_5.5.2.0-17.8 knitr_1.42             
# [46] pillar_1.9.0            rle_0.9.2               glue_1.6.2              evaluate_0.21           trust_0.1-8            
# [51] data.table_1.14.2       vctrs_0.5.2             Rdpack_2.4              cellranger_1.1.0        tidyr_1.2.0            
# [56] gtable_0.3.1            assertthat_0.2.1        cachem_1.0.6            xfun_0.39               rbibutils_2.2.13       
# [61] e1071_1.7-13            coda_0.19-4             class_7.3-19            tibble_3.1.7            memoise_2.0.1          
# [66] units_0.8-0             ellipsis_0.3.2 

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
  # survey networks
  if (period != "IA2") {
    LLN <- induced.subgraph(ig, which(V(ig)$Datst_1 == "LLN"))
    vert.attr_LLN <- as.data.frame(vertex_attr(LLN))
    lo_LLN <- layout.norm(as.matrix(vert.attr_LLN[, c("POINT_X", "POINT_Y")]))
    
    ### create new attribute to include average shortest paths for every node
    #### calculate number of nodes in the graph
    LLN_N <- gorder(LLN)
    #### use lapply to calculate the average shortest paths (results in a list)
    LLN_sp <- lapply(V(LLN),function(v){
      q <- shortest.paths(LLN, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/LLN_N})
    #### add as graph attribute
    LLN <- set_vertex_attr(LLN, "av_shortest_path", value = as.numeric(LLN_sp))
    # calculate shortest paths matrix
    d_LLN <- distances(LLN, mode = "all", algorithm = "automatic")
    
    NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
    vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
    lo_NJS <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
    
    NJS_N <- gorder(NJS)
    NJS_sp <- lapply(V(NJS),function(v){
      q <- shortest.paths(NJS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/NJS_N})
    NJS <- set_vertex_attr(NJS, "av_shortest_path", value = as.numeric(NJS_sp))
    d_NJS <- distances(NJS, mode = "all", algorithm = "automatic")
    
    TBS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "TBS"))
    vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
    lo_TBS <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
    
    TBS_N <- gorder(TBS)
    TBS_sp <- lapply(V(TBS),function(v){
      q <- shortest.paths(TBS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/TBS_N})
    TBS <- set_vertex_attr(TBS, "av_shortest_path", value = as.numeric(TBS_sp))
    d_TBS <- distances(TBS, mode = "all", algorithm = "automatic")
    
    THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
    vert.attr_THS <- as.data.frame(vertex_attr(THS))
    lo_THS <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
    
    THS_N <- gorder(THS)
    THS_sp <- lapply(V(THS),function(v){
      q <- shortest.paths(THS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/THS_N})
    THS <- set_vertex_attr(THS, "av_shortest_path", value = as.numeric(THS_sp))
    d_THS <- distances(THS, mode = "all", algorithm = "automatic")

    # create network
    nw <- intergraph::asNetwork(ig)
    nw_LLN <- intergraph::asNetwork(LLN)
    nw_NJS <- intergraph::asNetwork(NJS)
    nw_TBS <- intergraph::asNetwork(TBS)
    nw_THS <- intergraph::asNetwork(THS)
    # save vert.attr, ig, lo and nw to the environment with the respective period
    # in the object name
    va <- paste("vert.attr", period, sep = "_")
    data_va <- vert.attr
    assign(va, data_va)
    
    va <- paste("vert.attr_LLN", period, sep = "_")
    data_va <- vert.attr_LLN
    assign(va, data_va)
    
    va <- paste("vert.attr_THS", period, sep = "_")
    data_va <- vert.attr_THS
    assign(va, data_va)
    
    va <- paste("vert.attr_TBS", period, sep = "_")
    data_va <- vert.attr_TBS
    assign(va, data_va)
    
    va <- paste("vert.attr_NJS", period, sep = "_")
    data_va <- vert.attr_NJS
    assign(va, data_va)
    
    # gn <- paste("ig", period, sep = "_")
    # data_ig <- ig
    # assign(gn, data_ig)
    
    lon <- paste("lo", period, sep = "_")
    data_lo <- lo
    assign(lon, data_lo)
    
    lon <- paste("lo_LLN", period, sep = "_")
    data_lo <- lo_LLN
    assign(lon, data_lo)
    
    lon <- paste("lo_NJS", period, sep = "_")
    data_lo <- lo_NJS
    assign(lon, data_lo)
    
    lon <- paste("lo_TBS", period, sep = "_")
    data_lo <- lo_TBS
    assign(lon, data_lo)
    
    lon <- paste("lo_THS", period, sep = "_")
    data_lo <- lo_THS
    assign(lon, data_lo)
    
    nwn <- paste("nw", period, sep = "_")
    data_nw <- nw
    assign(nwn, data_nw)
    
    nwn <- paste("nw_LLN", period, sep = "_")
    data_nw <- nw_LLN
    assign(nwn, data_nw)
    
    nwn <- paste("nw_THS", period, sep = "_")
    data_nw <- nw_THS
    assign(nwn, data_nw)
    
    nwn <- paste("nw_TBS", period, sep = "_")
    data_nw <- nw_TBS
    assign(nwn, data_nw)
    
    nwn <- paste("nw_NJS", period, sep = "_")
    data_nw <- nw_NJS
    assign(nwn, data_nw)
    
    dn <- paste("d_LLN", period, sep = "_")
    data_d <- d_LLN
    assign(dn, data_d)
    
    dn <- paste("d_NJS", period, sep = "_")
    data_d <- d_NJS
    assign(dn, data_d)
    
    dn <- paste("d_TBS", period, sep = "_")
    data_d <- d_TBS
    assign(dn, data_d)
    
    dn <- paste("d_THS", period, sep = "_")
    data_d <- d_THS
    assign(dn, data_d)
  } else
  {
    NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
    vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
    lo_NJS <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
    
    NJS_N <- gorder(NJS)
    NJS_sp <- lapply(V(NJS),function(v){
      q <- shortest.paths(NJS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/NJS_N})
    NJS <- set_vertex_attr(NJS, "av_shortest_path", value = as.numeric(NJS_sp))
    d_NJS <- distances(NJS, mode = "all", algorithm = "automatic")
    
    TBS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "TBS"))
    vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
    lo_TBS <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
    
    TBS_N <- gorder(TBS)
    TBS_sp <- lapply(V(TBS),function(v){
      q <- shortest.paths(TBS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/TBS_N})
    TBS <- set_vertex_attr(TBS, "av_shortest_path", value = as.numeric(TBS_sp))
    d_TBS <- distances(TBS, mode = "all", algorithm = "automatic")
    
    THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
    vert.attr_THS <- as.data.frame(vertex_attr(THS))
    lo_THS <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
    
    THS_N <- gorder(THS)
    THS_sp <- lapply(V(THS),function(v){
      q <- shortest.paths(THS, v )
      rowSums(q*is.finite(q),na.rm = TRUE)/THS_N})
    THS <- set_vertex_attr(THS, "av_shortest_path", value = as.numeric(THS_sp))
    d_THS <- distances(THS, mode = "all", algorithm = "automatic")

    # create network
    nw <- intergraph::asNetwork(ig)
    nw_NJS <- intergraph::asNetwork(NJS)
    nw_TBS <- intergraph::asNetwork(TBS)
    nw_THS <- intergraph::asNetwork(THS)
    # savw vert.attr, ig, lo and nw to the environment with the respective period
    # in the object name
    va <- paste("vert.attr", period, sep = "_")
    data_va <- vert.attr
    assign(va, data_va)
    
    va <- paste("vert.attr_THS", period, sep = "_")
    data_va <- vert.attr_THS
    assign(va, data_va)
    
    va <- paste("vert.attr_TBS", period, sep = "_")
    data_va <- vert.attr_TBS
    assign(va, data_va)
    
    va <- paste("vert.attr_NJS", period, sep = "_")
    data_va <- vert.attr_NJS
    assign(va, data_va)

    lon <- paste("lo", period, sep = "_")
    data_lo <- lo
    assign(lon, data_lo)
    
    lon <- paste("lo_NJS", period, sep = "_")
    data_lo <- lo_NJS
    assign(lon, data_lo)
    
    lon <- paste("lo_TBS", period, sep = "_")
    data_lo <- lo_TBS
    assign(lon, data_lo)
    
    lon <- paste("lo_THS", period, sep = "_")
    data_lo <- lo_THS
    assign(lon, data_lo)
    
    nwn <- paste("nw", period, sep = "_")
    data_nw <- nw
    assign(nwn, data_nw)
    
    nwn <- paste("nw_THS", period, sep = "_")
    data_nw <- nw_THS
    assign(nwn, data_nw)
    
    nwn <- paste("nw_TBS", period, sep = "_")
    data_nw <- nw_TBS
    assign(nwn, data_nw)
    
    nwn <- paste("nw_NJS", period, sep = "_")
    data_nw <- nw_NJS
    assign(nwn, data_nw)
    
    dn <- paste("d_NJS", period, sep = "_")
    data_d <- d_NJS
    assign(dn, data_d)
    
    dn <- paste("d_TBS", period, sep = "_")
    data_d <- d_TBS
    assign(dn, data_d)
    
    dn <- paste("d_THS", period, sep = "_")
    data_d <- d_THS
    assign(dn, data_d)
  }
  
  x <- x + 1
}


# creating lists of network waves to include the temporal depencency (see Table 2)
## Dependent network variable - LHS:
survey_waves <- list(nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_NJS_IA2, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_TBS_IA2, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_THS_IA2, nw_LLN_EBA2, nw_LLN_MBA, nw_LLN_LBA, nw_LLN_IA1)

## MEmeory term:
survey_temp <- list(nw_NJS_EBA1, nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_EBA1, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_EBA1, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_LLN_EBA1, nw_LLN_EBA2, nw_LLN_MBA, nw_LLN_LBA)

## network list for the NULL model:
list_nw <- list(nw_EBA1, nw_EBA2, nw_MBA, nw_LBA, nw_IA1, nw_IA2)


#DISTANCE MATRICES
# distance matrix with all sites included, used for TERGM
DistMat <- as.matrix(destroyX(read.csv("DistMatAll_man.csv", row.names = 1)))
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


#SMALLDIFF-DIST MATRICES
# matrix with all sites included, used for TERGM
smalldiff_Dist <- as.matrix(destroyX(read.csv("smalldiff_Dist.csv", row.names = 1)))

smalldiffMats_Dist_periods <- list.files(pattern = "smalldiff_Dist_\\w+_hybrid.csv$", full.names = TRUE)
# create list of csv
list_smalldiffMats_Dist <- lapply(smalldiffMats_Dist_periods, read.csv, row.names = 1)
# row and column names are imported with an X because they are numbers. This function removes them.
list_smalldiffMats_Dist <- lapply(list_smalldiffMats_Dist, destroyX)
# add names to the csvs
names(list_smalldiffMats_Dist) <- gsub("_hybrid.csv", "", list.files(pattern = "smalldiff_Dist_\\w+_hybrid.csv$", full.names = F))
# add to environment as matrices
list2env(lapply(list_smalldiffMats_Dist, as.matrix),envir = .GlobalEnv)


#ABSDIFF-DIST MATRICES
# matrix with all sites included, used for TERGM
absdiff_Dist <- as.matrix(destroyX(read.csv("absdiff_Dist.csv", row.names = 1)))

AbsdiffMats_Dist_periods <- list.files(pattern = "absdiff_Dist_\\w+_hybrid.csv$", full.names = TRUE)
# create list of csv
list_AbsdiffMats_Dist <- lapply(AbsdiffMats_Dist_periods, read.csv, row.names = 1)
# row and column names are imported with an X because they are numbers. This function removes them.
list_AbsdiffMats_Dist <- lapply(list_AbsdiffMats_Dist, destroyX)
# add names to the csvs
names(list_AbsdiffMats_Dist) <- gsub("_hybrid.csv", "", list.files(pattern = "absdiff_Dist\\w+_hybrid.csv$", full.names = F))
# add to environment as matrices
list2env(lapply(list_AbsdiffMats_Dist, as.matrix),envir = .GlobalEnv)


# NULL model ----
mtergm.null <- mtergm(list_nw ~ edges)
summary(mtergm.null)
mtergm.null_gof <- gof(mtergm.null)
mtergm.null_gof
par(mfrow = c(2,2))
plot(mtergm.null_gof)
plot(mtergm.null_gof, plotlogodds = T)
mcmc.diagnostics(mtergm.null)

# Fitted Models
## Simple MTERGM without temporal dependencies 
mtergm <- mtergm(survey_waves ~ edges + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist))
summary(mtergm)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)

# fitted MTERGM without temporal dependencies (Table 3, Model 1)
mtergm.3 <- mtergm(survey_waves ~ edges + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 30000, seed = 123))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)

## simple MTERGM with temporal dependencies
mtergm <- mtergm(survey_waves ~ edges + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
sum_mtergm <- summary(mtergm)
mtergm_gof <- gof(mtergm)
mtergm_gof
par(mfrow = c(2,2))
plot(mtergm_gof)
plot(mtergm_gof, plotlogodds = T)

## fitted MTERGM with temporal dependencies (Table 3, Model 2)
mtergm.2.9.6 <- mtergm(survey_waves ~ edges + gwnsp(decay = 0.9, fixed = T) + meandeg + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp), control = control.ergm(MCMC.samplesize = 30000, seed = 12245))
summary(mtergm.2.9.6)
mtergm.2.9.6_gof <- gof(mtergm.2.9.6)
# mtergm.2.9.6_gof
par(mfrow = c(2,2))
plot(mtergm.2.9.6_gof)
plot(mtergm.2.9.6_gof, plotlogodds = T)
texreg(mtergm.2.9.6, "mtergm.2.9.6.txt", digits = 4)

wordreg(list(mtergm.3, mtergm.2.9.6), file = "Multi_ERGM_hybrid.txt")










