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

survey_waves <- list(nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_NJS_IA2, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_TBS_IA2, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_THS_IA2, nw_LLN_EBA2, nw_LLN_MBA, nw_LLN_LBA, nw_LLN_IA1)

survey_temp <- list(nw_NJS_EBA1, nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_EBA1, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_EBA1, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_LLN_EBA1, nw_LLN_EBA2, nw_LLN_MBA, nw_LLN_LBA)

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


# Step 1: NULL model ----
mtergm.null <- mtergm(list_nw ~ edges)
summary(mtergm.null)
mtergm.null_gof <- gof(mtergm.null)
mtergm.null_gof
par(mfrow = c(2,2))
plot(mtergm.null_gof)
plot(mtergm.null_gof, plotlogodds = T)
mcmc.diagnostics(mtergm.null)
# Error in mcmc.diagnostics.default(mtergm.null): An ergm object must be given as an argument

# Step 2: adding and adjusting terms----
mtergm.1 <- mtergm(list_nw ~ edges + gwesp(decay = 0.05, fixed = T) + nodecov("survey_log") + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + nodematch("Datst_1"))
summary(mtergm.1)
mtergm.1_gof <- gof(mtergm.1)
mtergm.1_gof
par(mfrow = c(2,2))
plot(mtergm.1_gof)
plot(mtergm.1_gof, plotlogodds = T)
# 1.1 simple model as above 
# esp and dsp good, distance bad, degree and ROC/PR ok
## ROC 0.903662 (0.5520477) / with survey_log: 0.9135517 (0.5543662)
## PR 0.2170562 (0.01938903) / with survey_log: 0.243401 (0.01914224)
## Error in mcmc.diagnostics.default(mtergm.1): An ergm object must be given as an argument

mtergm.1 <- mtergm(list_nw_all ~ edges + gwdegree(decay = 0.1, fixed = T) + gwesp(decay = 0.1, fixed = T) + nodecov("survey_log") + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + nodematch("Datst_1"))
summary(mtergm.1)
mtergm.1_gof <- gof(mtergm.1)
mtergm.1_gof
par(mfrow = c(2,2))
plot(mtergm.1_gof)
plot(mtergm.1_gof, plotlogodds = T)
mcmc.diagnostics(mtergm.1)
# 1.2 included gwdegree and adjust gwdegree/gwesp
# slightly improved degree and distance 
## ROC 0.9252751 (0.5376591)
## PR 0.3408323 (0.01823868)
## Error in mcmc.diagnostics.default(mtergm.1): An ergm object must be given as an argument

mtergm.1 <- mtergm(list_nw ~ edges + isolates + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + nodematch("Datst_1"))
summary(mtergm.1)
mtergm.1_gof <- gof(mtergm.1)
mtergm.1_gof
par(mfrow = c(2,2))
plot(mtergm.1_gof)
plot(mtergm.1_gof, plotlogodds = T)
mcmc.diagnostics(mtergm.1)
# 1.3 included isolates
# slightly improved degree and distance 
## ROC 0.9267527 (0.5459774)
## PR 0.3424956 (0.01870266)
## Error in mcmc.diagnostics.default(mtergm.1): An ergm object must be given as an argument

# LLN -----
## Step 1: NULL model 
mtergm.LLN_null <- mtergm(list_LLN ~ edges)
summary(mtergm.LLN_null)
mtergm.LLN_null_gof <- gof(mtergm.LLN_null)
mtergm.LLN_null_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_null_gof)
plot(mtergm.LLN_null_gof, plotlogodds = T)

## Step 2: adding and adjusting terms
mtergm.LLN <- mtergm(list_LLN ~ edges + gwesp(decay = 0, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.1 same as above
# reasonable fit for esp and degree, bad fit for distance 


mtergm.LLN <- mtergm(list_LLN ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.2 added gwdegree and set gwdegree and gwesp to 0.9
# better fit for esp and degree, worse for distance 
# ROC 0.9122485 (0.6128387)
# PR 0.225129 (0.02803626)

mtergm.LLN <- mtergm(list_LLN ~ edges + degree(1) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.3 added degree(1)
# better fit for esp and degree, no change for distance 
# ROC 0.9090048 (0.6021063)
# PR 0.237054 (0.03226336)

mtergm.LLN <- mtergm(list_LLN ~ edges + degree(1) + gwdegree(decay = 0.2, fixed = T) + gwesp(decay = 0.2, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.4 adjusted gwdegree and gwesp
# esp and degree slighlty worse, distance slightly better 
# ROC 0.9008204 (0.6251308)
# PR 0.2029295 (0.02894871)

mtergm.LLN <- mtergm(list_LLN ~ edges + degrange(4, to = 5) + degree(1) + gwdegree(decay = 0.2, fixed = T) + gwesp(decay = 0.2, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.5 added degrange
# slightly better than 1.4 
# ROC 0.8981288 (0.6071489)
# PR 0.2428825 (0.0279466)

mtergm.LLN <- mtergm(list_LLN ~ edges + degrange(3, to = 5) + degree(1) + gwdegree(decay = 0.2, fixed = T) + gwesp(decay = 0.2, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.LLN)
mtergm.LLN_gof <- gof(mtergm.LLN)
mtergm.LLN_gof
par(mfrow = c(2,2))
plot(mtergm.LLN_gof)
plot(mtergm.LLN_gof, plotlogodds = T)
# 1.6 changed degrange
# degree slightly better
# ROC 0.9081468 (0.6084623)
# PR 0.2696731 (0.02745375)


# NJS ----

## Step 1: NULL model 
mtergm.NJS_null <- mtergm(list_NJS ~ edges)
summary(mtergm.NJS_null)
mtergm.NJS_null_gof <- gof(mtergm.NJS_null)
mtergm.NJS_null_gof
par(mfrow = c(2,2))
plot(mtergm.NJS_null_gof)
plot(mtergm.NJS_null_gof, plotlogodds = T)

## Step 2: adding and adjusting terms
mtergm.NJS <- mtergm(list_NJS ~ edges + gwesp(decay = 0, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.NJS)
mtergm.NJS_gof <- gof(mtergm.NJS)
mtergm.NJS_gof
par(mfrow = c(2,2))
plot(mtergm.NJS_gof)
plot(mtergm.NJS_gof, plotlogodds = T)
# 1.1 same as above
# degree and distance ok, esp good 
# ROC 0.9304371(0.5937875)
# PR 0.6090274 (0.09534336)

mtergm.NJS <- mtergm(list_NJS ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.NJS)
mtergm.NJS_gof <- gof(mtergm.NJS)
mtergm.NJS_gof
par(mfrow = c(2,2))
plot(mtergm.NJS_gof)
plot(mtergm.NJS_gof, plotlogodds = T)
# 1.2 added gwdegree and set gwdegree and gwesp to 0.9
# good for degree and esp, ok for distance  
# ROC 0.9407095 (0.5755199)
# PR 0.6461646 (0.08987698)

# TBS ----

## Step 1: NULL model 
mtergm.TBS_null <- mtergm(list_TBS ~ edges)
summary(mtergm.TBS_null)
mtergm.TBS_null_gof <- gof(mtergm.TBS_null)
mtergm.TBS_null_gof
par(mfrow = c(2,2))
plot(mtergm.TBS_null_gof)
plot(mtergm.TBS_null_gof, plotlogodds = T)

## Step 2: adding and adjusting terms
mtergm.TBS <- mtergm(list_TBS ~ edges + gwesp(decay = 0, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.TBS)
mtergm.TBS_gof <- gof(mtergm.TBS)
mtergm.TBS_gof
par(mfrow = c(2,2))
plot(mtergm.TBS_gof)
plot(mtergm.TBS_gof, plotlogodds = T)
# 1.1 same as above
# degree and distance ok, esp good 
# ROC 0.9304371(0.5937875)
# PR 0.6090274 (0.09534336)

mtergm.TBS <- mtergm(list_TBS ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.TBS)
mtergm.TBS_gof <- gof(mtergm.TBS)
mtergm.TBS_gof
par(mfrow = c(2,2))
plot(mtergm.TBS_gof)
plot(mtergm.TBS_gof, plotlogodds = T)
# 1.2 added gwdegree and set gwdegree and gwesp to 0.9
# good for esp, ok for distance and degree 
# ROC 0.9267969 (0.58967)
# PR 0.6513753 (0.2002203)

mtergm.TBS <- mtergm(list_TBS ~ edges + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.TBS)
mtergm.TBS_gof <- gof(mtergm.TBS)
mtergm.TBS_gof
par(mfrow = c(2,2))
plot(mtergm.TBS_gof)
plot(mtergm.TBS_gof, plotlogodds = T)
# 1.3 added altkstar
# esp and degree slightly worse, distance slightly better
# ROC 0.9358818 (0.5918325)
# PR 0.689940 (0.1851428)

mtergm.TBS <- mtergm(list_TBS ~ edges + degrange(3, to = 6) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.TBS)
mtergm.TBS_gof <- gof(mtergm.TBS)
mtergm.TBS_gof
par(mfrow = c(2,2))
plot(mtergm.TBS_gof)
plot(mtergm.TBS_gof, plotlogodds = T)
# 1.4 removed altkstar, added degrange
# slightly better
# ROC 0.938798 (0.5796915)
# PR 0.6713691 (0.1755235)

#THS ----

## Step 1: NULL model 
mtergm.THS_null <- mtergm(list_THS ~ edges)
summary(mtergm.THS_null)
mtergm.THS_null_gof <- gof(mtergm.THS_null)
mtergm.THS_null_gof
par(mfrow = c(2,2))
plot(mtergm.THS_null_gof)
plot(mtergm.THS_null_gof, plotlogodds = T)

## Step 2: adding and adjusting terms
mtergm.THS <- mtergm(list_THS ~ edges + gwesp(decay = 0, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.1 same as above
# reasonable fit 
# ROC 0.8732105 (05358076)
# PR 0.6869439 (0.2824497)

mtergm.THS <- mtergm(list_THS ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.2 added gwdegree and set gwdegree and gwesp to 0.9
# good for esp, ok for distance and degree 
# ROC 0.8991406 (0.5398458)
# PR 0.6814833 (0.2676693)

mtergm.THS <- mtergm(list_THS ~ edges + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.3 added altkstar
# similar to 1.2
# ROC 0.9076996 (0.5567871
# PR 0.7051576 (0.284807)

mtergm.THS <- mtergm(list_THS ~ edges + degrange(2, to = 4) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.4 removed altkstar, added degrange
# no improvement
# ROC 0.9035849 (0.5341064)
# PR 0.6987689 (0.2761628)

mtergm.THS <- mtergm(list_THS ~ edges + concurrentties  + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.5 removed degrange, added concurrentties
# no improvement
# ROC 0.908971 (0.5528079)
# PR 0.6858735 (0.2703834)

mtergm.THS <- mtergm(list_THS ~ edges + degrange(2, to = 4) + concurrentties  + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + timecov(transform = function(t) t))
summary(mtergm.THS)
mtergm.THS_gof <- gof(mtergm.THS)
mtergm.THS_gof
par(mfrow = c(2,2))
plot(mtergm.THS_gof)
plot(mtergm.THS_gof, plotlogodds = T)
# 1.6 added degrange
# no improvement
# ROC 0.9073405 (0.5701789)
# PR 0.6964685 (0.3035429

# All periods ----
## Step 1: NULL model 
mtergm.2.null <- mtergm(survey_waves ~ edges)
summary(mtergm.null)
mtergm.null_gof <- gof(mtergm.null)
mtergm.null_gof
par(mfrow = c(2,2))
plot(mtergm.null_gof)
plot(mtergm.null_gof, plotlogodds = T)

## Step 2: adding and adjusting terms
mtergm.2 <- mtergm(survey_waves ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.1 included gwdegree and adjust gwdegree/gwesp
# everything ok but could be improved
## ROC 0.9488561 (0.6944685)
## PR 0.562414 (0.1581132)


## fit MTERGM with temporal dependencies
mtergm.2 <- mtergm(survey_waves ~ edges + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.2 included altkstar
# no significant change 
## ROC 0.9489262 (0.696476)
## PR 0.5754612 (0.1576878)

mtergm.2 <- mtergm(survey_waves ~ edges + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.3 included memory term edgecov and interaction effects edgecov
# slightly improved  
## ROC 0.9672353 (0.6967233)
## PR 0.8398528 (0.1746886)

mtergm.2 <- mtergm(survey_waves ~ edges + degrange(2, to = 4) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.4 included degrange
# degree improved 
## ROC 0.9675305 (0.6969)
## PR 0.8386977 (0.1762473)

mtergm.2 <- mtergm(survey_waves ~ edges + degrange(1, to = 4) + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.5 added altkstar
# not much difference 
## ROC 0.9742076 (0.6976835)
## PR 0.8489854 (0.1836806)

mtergm.2 <- mtergm(survey_waves ~ edges + degrange(2, to = 4) + altkstar(lambda = 2, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.6 adjusted lamda for altkstar
# slightly improved 
## ROC 0.9683013 (0.7023345)
## PR 0.843 (0.1811424)

mtergm.2 <- mtergm(survey_waves ~ edges + twopath + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.81, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.7 included twopath - had to adjust gwdegree and gwesp
# not much difference 
## ROC 0.970514 (0.6986593)
## PR 0.8501409 (0.1814553)

mtergm.2 <- mtergm(survey_waves ~ edges + threetrail + degrange(2, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.5, fixed = T) + gwesp(decay = 0.5, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
#Error in ergm.MCMLE(init, nw, model, initialfit = (initialfit <- NULL): MCMLE estimation stuck. There may be excessive correlation between model terms, suggesting a poor model for the observed data. If target.stats are specified, try increasing SAN parameters.
# 2.8 replaced twopath with threetrail
# not much difference 
## ROC 0.9513725 (0.6986771)
## PR 0.7377734 (0.123768)

mtergm.2 <- mtergm(survey_waves ~ edges + degrange(2, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwnsp(decay = 0.9) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9 added gwnsp
# not much difference 
## ROC 0.970509 (0.7025734)
## PR 0.8637621 (0.1754847)

mtergm.2 <- mtergm(survey_waves ~ edges + nodecov("av_shortest_path") + degrange(2, to = 4) + gwnsp(decay = 0.8) + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.1 added nodecov for average shortest paths
# not much difference 
## ROC 0.9694479 (0.7118933)
## PR 0.8410635 (0.1869135)

mtergm.2 <- mtergm(survey_waves ~ edges + nodecov("av_shortest_path") + degrange(2, to = 4) + gwnsp(decay = 0.8) + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.1 added edgecov for average shortest paths
# not much difference 
## ROC 0.9694479 (0.7118933)
## PR 0.8410635 (0.1869135)

mtergm.2 <- mtergm(survey_waves ~ edges + concurrent + nodecov("av_shortest_path") + degrange(2, to = 4) + gwnsp(decay = 0.8) + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.2 added concurrent
# not much difference 
## ROC 0.9712867 (0.6994065)
## PR 0.8483928 (0.1766454)

mtergm.2 <- mtergm(survey_waves ~ edges + degree1.5 + nodecov("av_shortest_path") + gwnsp(decay = 0.8) + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.3 degree1.5, removed degrange - two degree controls lead to model degeneracy
# not much difference 
## ROC 0.9757375 (0.700066)
## PR 0.8448791 (0.1864882)

mtergm.2 <- mtergm(survey_waves ~ edges + degcor + nodecov("av_shortest_path") + gwdegree(decay = 0.5, fixed = T) + gwesp(decay = 0.5, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.4 included degcor, removed degree1.5
# does not converge (also not when degree1.5 is included)

mtergm.2 <- mtergm(survey_waves ~ edges + degcrossprod + nodecov("av_shortest_path") + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.5 included degcrossprod, removed degree1.5
# does not converge  (also not when degree1.5 is included)

mtergm.2.9.7 <- mtergm(survey_waves ~ edges + meandeg + degree1.5 + nodecov("av_shortest_path") + gwnsp + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp), control = control.ergm(MCMC.samplesize = 20000, seed = 123))
summary(mtergm.2.9.7)
mtergm.2.9.7_gof <- gof(mtergm.2.9.7)
mtergm.2.9.7_gof
par(mar = c(1, 1, 1, 1), mfrow = c(2,2))
plot(mtergm.2.9.7_gof)
plot(mtergm.2.9.7_gof, plotlogodds = T)
# 2.9.7 
# meandeg improves
# degrange 1 - 3 makes it worse 
# degree1.5 - makes it much worse

mtergm.2.9.8 <- mtergm(survey_waves ~ edges + meandeg + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp))
summary(mtergm.2.9.8)
mtergm.2_gof <- gof(mtergm.2.9.8)
mtergm.2.9.8_gof
par(mfrow = c(2,2))
plot(mtergm.2.9.8)
plot(mtergm.2.9.8, plotlogodds = T)
# 2.9.8 removed gwnsp 
# significantly improved gof 

mtergm.2 <- mtergm(survey_waves ~ edges + meandeg + gwdegree(decay = 0.9, fixed = T) + gwnsp + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp), control = control.ergm(MCMC.samplesize = 20000, seed = 123))
summary(mtergm.2)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)
# 2.9.9 removed gwnsp and included edgecov shortest path
# significantly improved gof 


# fit mtergm without temporal dependencies

mtergm.3 <- mtergm(survey_waves ~ edges + altkstar(lambda = 4, fixed = TRUE) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 30000, seed = 123))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
texreg(mtergm.3, "mtergm_3.1.txt", digits = 4)
# 3.1 included altkstar - sehr gut!
## ROC 0.9498005 (0.6875715)
## PR 0.5851478 (0.1553525)

mtergm.3.2 <- mtergm(survey_waves ~ edges + degree1.5 + degrange(5, to = 7) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 30000, seed = 123))
summary(mtergm.3.2)
mtergm.3.2_gof <- gof(mtergm.3.2)
mtergm.3.2_gof
par(mfrow = c(2,2))
plot(mtergm.3.2_gof)
plot(mtergm.3.2_gof, plotlogodds = T)
texreg(mtergm.3.2, "mtergm.3.2.txt", digits = 4)
# 3.2 included degrange
## ROC 0.9495477 (0.6949209)
## PR 0.5791886 (0.157887)

mtergm.3.3 <- mtergm(survey_waves ~ edges + degree1.5 + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 30000, seed = 123))
summary(mtergm.3.3)
mtergm.3.3_gof <- gof(mtergm.3.3)
mtergm.3.3_gof
par(mfrow = c(2,2))
plot(mtergm.3.3_gof)
plot(mtergm.3.3_gof, plotlogodds = T)
texreg(mtergm.3.3, "mtergm.3.3.txt", digits = 4)
# 3.3 removed altkstar as it is the same as gwdegree (https://cran.r-project.org/web/packages/ergm/ergm.pdf, S.14)
# good fit
## ROC 0.9522096 (0.6885872)
## PR 0.576743 (0.1581012)

mtergm.3.4 <- mtergm(survey_waves ~ edges + degree1.5 + degrange(3, to = 7) + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 30000, seed = 123))

summary(mtergm.3.4)
mtergm.3.4_gof <- gof(mtergm.3.4)
mtergm.3.4_gof
par(mfrow = c(2,2))
plot(mtergm.3.4_gof)
plot(mtergm.3.4_gof, plotlogodds = T)
texreg(mtergm.3.4, "mtergm.3.4.txt", digits = 4)
# 3.4 changed degrange
## ROC 0.9492651 (0.6931596)
## PR 0.5693014 (0.1598426)

# not converged after more than a day
mtergm.3.5 <- mtergm(survey_waves ~ edges + meandeg + degree1.5 + gwdegree(decay = 0.9, fixed = T) + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist), control = control.ergm(MCMC.samplesize = 20000, seed = 123))

summary(mtergm.3.5)
mtergm.3.5_gof <- gof(mtergm.3.5)
mtergm.3.5_gof
par(mfrow = c(2,2))
plot(mtergm.3.5_gof)
plot(mtergm.3.5_gof, plotlogodds = T)
texreg(mtergm.3.5, "mtergm.3.5.txt", digits = 4)
# 3.5 added meandeg
## ROC 0.9492651 (0.6931596)
## PR 0.5693014 (0.1598426)


# Paper 2 outputs ----
## Simple MTERMG without temporal dependencies
mtergm <- mtergm(survey_waves ~ edges + gwesp(decay = 0.9, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist))
summary(mtergm)
mtergm.2_gof <- gof(mtergm.2)
mtergm.2_gof
par(mfrow = c(2,2))
plot(mtergm.2_gof)
plot(mtergm.2_gof, plotlogodds = T)

# fitted MTERGM without temporal dependencies
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

## fitted MTERGM with temporal dependencies
mtergm.2.9.6 <- mtergm(survey_waves ~ edges + gwnsp(decay = 0.9, fixed = T) + meandeg + gwdegree(decay = 0.8, fixed = T) + gwesp(decay = 0.8, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp), control = control.ergm(MCMC.samplesize = 30000, seed = 12245))
summary(mtergm.2.9.6)
mtergm.2.9.6_gof <- gof(mtergm.2.9.6)
# mtergm.2.9.6_gof
par(mfrow = c(2,2))
plot(mtergm.2.9.6_gof)
plot(mtergm.2.9.6_gof, plotlogodds = T)
texreg(mtergm.2.9.6, "mtergm.2.9.6.txt", digits = 4)

wordreg(list(mtergm.2.9.6, mtergm.2.9.6), file = "Multi_ERGM_hybrid.txt")




#All surveys with different period constellation ----
# Remove wave 2 (EBA2)
survey_waves_wo2 <- list(nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_NJS_IA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_TBS_IA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_THS_IA2, nw_LLN_MBA, nw_LLN_LBA, nw_LLN_IA1)

survey_temp_wo2 <- list(nw_NJS_EBA1, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_EBA1, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_EBA1, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_LLN_EBA1, nw_LLN_MBA, nw_LLN_LBA)

mtergm.3 <- mtergm(survey_waves_wo2 ~ edges + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.7, fixed = T) + gwesp(decay = 0.7, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + nodecov("survey_log") + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp_wo2))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
# 3.1 survey lists without EBA2
# ESP and degree similar, distance improved significantly 
## ROC 0.9483426 (0.6620683)
## PR 0.7499378 (0.129493)

# Remove wave 2 (EBA2) and wave 6 (IA2)
survey_waves_wo2_wo6 <- list(nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_LLN_MBA, nw_LLN_LBA)

survey_temp_wo2_wo6 <- list(nw_NJS_EBA1, nw_NJS_MBA, nw_NJS_LBA, nw_TBS_EBA1, nw_TBS_MBA, nw_TBS_LBA, nw_THS_EBA1, nw_THS_MBA, nw_THS_LBA, nw_LLN_EBA1, nw_LLN_MBA)

mtergm.3 <- mtergm(survey_waves_wo2_wo6 ~ edges + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.6, fixed = T) + gwesp(decay = 0.6, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + nodecov("survey_log") + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp_wo2_wo6))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
# 3.2 survey lists without EBA2 and IA2
# ESP and degree similar, distance slightly improved
## ROC 0.942403 (0.6608796)
## PR 0.6745683 (0.1169712)


# Remove LLN surveys
survey_waves_woLLN <- list(nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_NJS_IA2, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_TBS_IA2, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_THS_IA2)

survey_temp_woLLN <- list(nw_NJS_EBA1, nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_EBA1, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_EBA1, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1)

mtergm.3 <- mtergm(survey_waves_woLLN ~ edges + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.6, fixed = T) + gwesp(decay = 0.6, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + nodecov("survey_log") + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp_woLLN))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
# 3.3 survey lists without LLN
# ESP and degree similar, distance worse 
## ROC 0.9565232 (0.6017546)
## PR 0.8197829 (0.1628505)

# Remove wave 6 (IA2)
survey_waves_wo6 <- list(nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_NJS_IA1, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_TBS_IA1, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_THS_IA1, nw_LLN_EBA2, nw_LLN_MBA, nw_LLN_LBA)

survey_temp_wo6 <- list(nw_NJS_EBA1, nw_NJS_EBA2, nw_NJS_MBA, nw_NJS_LBA, nw_TBS_EBA1, nw_TBS_EBA2, nw_TBS_MBA, nw_TBS_LBA, nw_THS_EBA1, nw_THS_EBA2, nw_THS_MBA, nw_THS_LBA, nw_LLN_EBA1, nw_LLN_EBA2, nw_LLN_MBA)

mtergm.3 <- mtergm(survey_waves_wo6 ~ edges + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.6, fixed = T) + gwesp(decay = 0.6, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + nodecov("survey_log") + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp_wo6))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
# 3.4 survey lists without IA2
# ESP and distance similar, degree slightly better, dsp worse 
## ROC 0.947124 (0.7147599)
## 0.6548496 (0.1339179)


# Model only 2 + 6
survey_waves_2_6 <- list(nw_NJS_IA2, nw_TBS_IA2, nw_THS_IA2, nw_LLN_IA1)

survey_temp_2_6 <- list(nw_NJS_EBA2, nw_TBS_EBA2, nw_THS_EBA2, nw_LLN_EBA2)

mtergm.3 <- mtergm(survey_waves_2_6 ~ edges + degrange(1, to = 4) + altkstar(lambda = 3, fixed = TRUE) + gwdegree(decay = 0.5, fixed = T) + gwesp(decay = 0.5, fixed = T) + nodecov("Size") + absdiff("Size") + smalldiff("Size", cutoff = 5) + nodecov("survey_log") + edgecov(DistMat) + edgecov(absdiff_Dist) + edgecov(smalldiff_Dist) + edgecov(survey_temp_2_6))
summary(mtergm.3)
mtergm.3_gof <- gof(mtergm.3)
mtergm.3_gof
par(mfrow = c(2,2))
plot(mtergm.3_gof)
plot(mtergm.3_gof, plotlogodds = T)
# 3.5 survey lists with only waves 2 and 6
# distance bad, everything else good 
## ROC 0.947124 (0.7147599)
## 0.6548496 (0.1339179)




