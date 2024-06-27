library(igraph)
library(plyr)
library(dplyr)
library(sf)
library(R.utils)
library(purrr)
library(xts)
library(network)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(arsenal)
library(cowplot)
library(viridis)
library(viridisLite)
library(patchwork)
library(gridGraphics)
library(openxlsx)

#function to remove the X from the column names of imported .csv files 
destroyX = function(es) {
  f = es
  for (col in c(1:ncol(f))){ #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}

# 
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] network_1.18.0    xts_0.12.2        zoo_1.8-11        purrr_0.3.4       R.utils_2.12.2    R.oo_1.25.0       R.methodsS3_1.8.2 sf_1.0-7         
# [9] dplyr_1.0.9       igraph_1.3.5     
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.10          pillar_1.9.0         compiler_4.1.0       class_7.3-19         tools_4.1.0          lifecycle_1.0.3      tibble_3.1.7        
# [8] lattice_0.20-44      pkgconfig_2.0.3      rlang_1.0.6          DBI_1.1.3            cli_3.4.1            rstudioapi_0.14      coda_0.19-4         
# [15] e1071_1.7-13         generics_0.1.3       vctrs_0.5.2          classInt_0.4-7       grid_4.1.0           tidyselect_1.2.0     glue_1.6.2          
# [22] R6_2.5.1             fansi_1.0.3          magrittr_2.0.3       ellipsis_0.3.2       units_0.8-0          assertthat_0.2.1     utf8_1.2.2          
# [29] KernSmooth_2.23-20   proxy_0.4-27         statnet.common_4.7.0

# Import data ----
## manual networks
edgelist_EBA1 <- read_xlsx("edgelists_manual_new.xlsx", sheet = "EBA1")
edgelist_IA2 <- read_xlsx("edgelists_manual_new.xlsx", sheet = "IA2")
edgelist_EBA2 <- read_xlsx("edgelists_manual_new.xlsx", sheet = "EBA2")
edgelist_IA1 <- read_xlsx("edgelists_manual_new.xlsx", sheet = "IA1")
edgelist_LBA <- read_xlsx("edgelists_manual_new.xlsx", sheet = "LBA")
edgelist_MBA <- read_xlsx("edgelists_manual_new.xlsx", sheet = "MBA")

sites_EBA1 <- read.csv("EBA1_sites_id_log.csv")
sites_IA2 <- read.csv("IA2_sites_id_log.csv")
sites_EBA2 <- read.csv("EBA2_sites_id_log.csv")
sites_IA1 <- read.csv("IA1_sites_id_log.csv")
sites_LBA <- read.csv("LBA_sites_id_log.csv")
sites_MBA <- read.csv("MBA_sites_id_log.csv")

## computational networks
edgelist_EBA1 <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "EBA1")
edgelist_IA2 <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "IA2")
edgelist_EBA2 <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "EBA2")
edgelist_IA1 <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "IA1")
edgelist_LBA <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "LBA")
edgelist_MBA <- read_xlsx("edgelists_comp_5500_new.xlsx", sheet = "MBA")

sites_EBA1 <- read.csv("EBA1_sites_5500_id.csv")
sites_EBA1 = sites_EBA1[!duplicated(sites_EBA1$id),]
sites_IA2 <- read.csv("IA2_sites_5500_id.csv")
sites_IA2 = sites_IA2[!duplicated(sites_IA2$id),]
sites_EBA2 <- read.csv("EBA2_sites_5500_id.csv")
sites_EBA2 = sites_EBA2[!duplicated(sites_EBA2$id),]
sites_IA1 <- read.csv("IA1_sites_5500_id.csv")
sites_IA1 = sites_IA1[!duplicated(sites_IA1$id),]
sites_LBA <- read.csv("LBA_sites_5500_id.csv")
sites_LBA = sites_LBA[!duplicated(sites_LBA$id),]
sites_MBA <- read.csv("MBA_sites_5500_id.csv")
sites_MBA = sites_MBA[!duplicated(sites_MBA$id),]

edgelist_EBA1 <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "EBA1")
edgelist_IA2 <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "IA2")
edgelist_EBA2 <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "EBA2")
edgelist_IA1 <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "IA1")
edgelist_LBA <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "LBA")
edgelist_MBA <- read_xlsx("edgelists_comp_3000_new.xlsx", sheet = "MBA")

sites_EBA1 <- read.csv("sites_EBA1_node_comp_3000.csv")
sites_EBA1 = sites_EBA1[!duplicated(sites_EBA1$id),]
sites_IA2 <- read.csv("sites_IA2_node_comp_3000.csv")
sites_IA2 = sites_IA2[!duplicated(sites_IA2$id),]
sites_EBA2 <- read.csv("sites_EBA2_node_comp_3000.csv")
sites_EBA2 = sites_EBA2[!duplicated(sites_EBA2$id),]
sites_IA1 <- read.csv("sites_IA1_node_comp_3000.csv")
sites_IA1 = sites_IA1[!duplicated(sites_IA1$id),]
sites_LBA <- read.csv("sites_LBA_node_comp_3000.csv")
sites_LBA = sites_LBA[!duplicated(sites_LBA$id),]
sites_MBA <- read.csv("sites_MBA_node_comp_3000.csv")
sites_MBA = sites_MBA[!duplicated(sites_MBA$id),]

## hybrid network
edgelist_EBA1 <- read_xlsx("edgelists_hybrid.xlsx", sheet = "EBA1")
edgelist_IA2 <- read_xlsx("edgelists_hybrid.xlsx", sheet = "IA2")
edgelist_EBA2 <- read_xlsx("edgelists_hybrid.xlsx", sheet = "EBA2")
edgelist_IA1 <- read_xlsx("edgelists_hybrid.xlsx", sheet = "IA1")
edgelist_LBA <- read_xlsx("edgelists_hybrid.xlsx", sheet = "LBA")
edgelist_MBA <- read_xlsx("edgelists_hybrid.xlsx", sheet = "MBA")

sites_EBA1 <- read.csv("sites_EBA1_hybrid.csv")
sites_IA2 <- read.csv("sites_IA2_hybrid.csv")
sites_EBA2 <- read.csv("sites_EBA2_hybrid.csv")
sites_IA1 <- read.csv("sites_IA1_hybrid.csv")
sites_LBA <- read.csv("sites_LBA_hybrid.csv")
sites_MBA <- read.csv("sites_MBA_hybrid.csv")



# Descriptives ----
## degree metrics
deg <- degree(ig)
hist(deg, main = "Histogram of node degree")
mean(deg)   
table(deg)   #table of degree frequency
deg.dist <- degree_distribution(ig, cumulative = T, mode="all")
plot(x = 0:max(deg), y = 1 - deg.dist, pch = 19, cex = 1.2, col = "orange", xlab  ="Degree", ylab = "Cumulative Frequency")

## centrality metrics
centr_degree(ig, mode = "all", normalized = T)
centr_betw(ig, directed = F, normalized = T)
max(harmonic_centrality(ig, normalized = T, mode = "all"))
mean(harmonic_centrality(ig, normalized = T, mode = "all"))

## other metrics
edge_density(ig, loops=F)
length(cluster_edge_betweenness(ig)) # number of communities


transitivity(ig, type="global") 
diameter(ig, directed = F, weights = NA) #longest shortest path between two nodes
#diam <- get_diameter(g, directed = F) #nodes along the diameter
mean_distance(ig, directed = F)
largest_cliques(ig)
modularity(cluster_edge_betweenness(ig))

## node level metrics
hub_score(g, weights = NA)$vector
authority_score(g, weights = NA)$vector
transitivity(g, type="local")
betweenness(g, directed = F, weights = NA)
edge_betweenness(g, directed = F, weights = NA)
closeness(g, mode = "all", weights = NA)
distances(g)
cliques(g)
sapply(cliques(g), length) # clique sizes
cluster_edge_betweenness(g)
clp <- cluster_label_prop(g)
plot(clp, g)
coreness(g, mode = "all")

# EBA1 ----
## convert edgelist to adjaceny matrix ----

## IDs for manual network
vert.attr_EBA1 <- sites_EBA1 %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_EBA1, vert.attr_EBA1$sizeCat == 1 | vert.attr_EBA1$sizeCat == 2 | 
                      vert.attr_EBA1$sizeCat == 3)
vert.attr_EBA1 <- vert.attr_EBA1 %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig <- graph_from_data_frame(edgelist_EBA1, directed = FALSE, vertices = vert.attr_EBA1)
ig <- igraph::simplify(ig)
## define layout to display the nodes with their real-world coordinates
lo <- layout.norm(as.matrix(vert.attr_EBA1[, c("POINT_X", "POINT_Y")]))

nw_EBA1 <- intergraph::asNetwork(ig)


## plot network----
plot(nw_EBA1,
     main = "Hollow way network EBA1",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_EBA1$Site_Nm)

# create a color map
col <- data.frame(Datst_1 = unique(vert.attr_EBA1$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_EBA1$color <- col$color[match(vert.attr_EBA1$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_EBA1, directed = FALSE, vertices = vert.attr_EBA1)
ig <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_EBA1[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(ig2, layout = lo, rescale = F, vertex.label = vert.attr_EBA1$Site_Nm, vertex.size = degree(ig)/3, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA1$Site_Nm, vertex.size = betweenness(ig)/10, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA1$Site_Nm, vertex.size = log(vert.attr_EBA1$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree 
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_EBA1$Site_Nm, vertex.size = log(vert.attr_EBA1$Size + 1, 2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys EBA1 ----
## LLN survey ----
LLN <- induced.subgraph(ig, which(V(ig)$Datst_1 == "LLN"))
vert.attr_LLN <- as.data.frame(vertex_attr(LLN))
lo2 <- layout.norm(as.matrix(vert.attr_LLN[, c("POINT_X", "POINT_Y")]))
plot.igraph(LLN, layout = lo2, vertex.label = vertex_attr(LLN, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(LLN, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(LLN, "Site_Nm"), vertex.size = degree(LLN)*1.5, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(LLN, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(LLN, "Site_Nm"), vertex.size = betweenness(LLN)*0.15, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(LLN, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(LLN, "Site_Nm"), vertex.size = log(vertex_attr(LLN, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(LLN)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(LLN, layout = lo2, vertex.color=cols[as.character(degree(LLN))], vertex.label = vertex_attr(LLN, "Site_Nm"), vertex.size = log(vertex_attr(LLN, "Size") + 1, 1.3), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS + THS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS" | V(ig)$Datst_1 == "THS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS), vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.08, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*2, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*3, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)


# IA2 ----
## convert edgelist to adjaceny matrix ----
vert.attr_IA2 <- sites_IA2 %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_IA2, vert.attr_IA2$sizeCat == 1 | vert.attr_IA2$sizeCat == 2 | 
                      vert.attr_IA2$sizeCat == 3)
vert.attr_IA2 <- vert.attr_IA2 %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig5 <- graph_from_data_frame(edgelist_IA2, directed = FALSE, vertices = vert.attr_IA2)
ig5 <- igraph::simplify(ig5)
## define layout to display the nodes with their real-world coordinates
lo5 <- layout.norm(as.matrix(vert.attr_IA2[, c("POINT_X", "POINT_Y")]))

nw_IA2 <- intergraph::asNetwork(ig5)


## plot network----
plot(nw_IA2,
     main = "Hollow way network IA2",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_IA2$Site_Nm)

ig5 <- intergraph::asIgraph(nw_IA2)

# create a color map for surveys
col <- data.frame(Datst_1 = unique(vert.attr_IA2$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_IA2$color <- col$color[match(vert.attr_IA2$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig5 <- graph_from_data_frame(edgelist_IA2, directed = FALSE, vertices = vert.attr_IA2)
ig5 <- igraph::simplify(ig5)
lo5 <- layout.norm(as.matrix(vert.attr_IA2[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = degree(ig)/3, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = betweenness(ig)*0.4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = log(vert.attr_IA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_IA2$Site_Nm, vertex.size = log(vert.attr_IA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys IA2 ----
## LLN survey ----
THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
vert.attr_THS <- as.data.frame(vertex_attr(THS))
lo2 <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
plot.igraph(THS, layout = lo2, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = degree(THS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = betweenness(THS)*0.7, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(THS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, rescale = F, vertex.color=cols[as.character(degree(THS))], vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS)*1.7, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.03, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, rescale = F, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.15), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, rescale = F, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)


# EBA2 ----
## convert edgelist to adjaceny matrix ----
vert.attr_EBA2 <- sites_EBA2 %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_EBA2, vert.attr_EBA2$sizeCat == 1 | vert.attr_EBA2$sizeCat == 2 | 
                      vert.attr_EBA2$sizeCat == 3)
vert.attr_EBA2 <- vert.attr_EBA2 %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig1 <- graph_from_data_frame(edgelist_EBA2, directed = FALSE, vertices = vert.attr_EBA2)
ig1 <- igraph::simplify(ig1)
## define layout to display the nodes with their real-world coordinates
lo1 <- layout.norm(as.matrix(vert.attr_EBA2[, c("POINT_X", "POINT_Y")]))

nw_EBA2 <- intergraph::asNetwork(ig)

## plot network----
plot(nw_EBA2,
     main = "Hollow way network EBA2",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_EBA2$Site_Nm)

# create a color map for surveys
col <- data.frame(Datst_1 = unique(vert.attr_EBA2$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_EBA2$color <- col$color[match(vert.attr_EBA2$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_EBA2, directed = FALSE, vertices = vert.attr_EBA2)
ig  <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_EBA2[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = degree(ig)/2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = betweenness(ig)/10, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = log(vert.attr_EBA2$Size + 1, 2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = log(vert.attr_EBA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys EBA2 ----
## LLN survey ----
THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
vert.attr_THS <- as.data.frame(vertex_attr(THS))
lo2 <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
plot.igraph(THS, layout = lo2, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = degree(THS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = betweenness(THS)*0.7, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(THS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, rescale = F, vertex.color=cols[as.character(degree(THS))], vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS)*1.7, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.03, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, rescale = F, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.15), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, rescale = F, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)



# MBA ----
## convert edgelist to adjaceny matrix ----
vert.attr_MBA <- sites_MBA %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_MBA, vert.attr_MBA$sizeCat == 1 | vert.attr_MBA$sizeCat == 2 | 
                      vert.attr_MBA$sizeCat == 3)
vert.attr_MBA <- vert.attr_MBA %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig2 <- graph_from_data_frame(edgelist_MBA, directed = FALSE, vertices = vert.attr_MBA)
ig2 <- igraph::simplify(ig2)
## define layout to display the nodes with their real-world coordinates
lo2 <- layout.norm(as.matrix(vert.attr_MBA[, c("POINT_X", "POINT_Y")]))

nw_MBA <- intergraph::asNetwork(ig)


## plot network----
plot(nw_MBA,
     main = "Hollow way network MBA",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_MBA$Site_Nm)

# create a color map for surveys
col <- data.frame(Datst_1 = unique(vert.attr_MBA$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_MBA$color <- col$color[match(vert.attr_MBA$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_MBA, directed = FALSE, vertices = vert.attr_MBA)
ig <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_MBA[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(degree(ig))*1.5, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = betweenness(ig)*0.4, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(vert.attr_MBA$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(vert.attr_MBA$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys MBA ----
## LLN survey ----
THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
vert.attr_THS <- as.data.frame(vertex_attr(THS))
lo2 <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
plot.igraph(THS, layout = lo2, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = degree(THS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = betweenness(THS)*0.7, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(THS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, rescale = F, vertex.color=cols[as.character(degree(THS))], vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS)*1.7, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.03, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, rescale = F, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.15), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, rescale = F, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)


# LBA ----
## convert edgelist to adjaceny matrix ----
vert.attr_LBA <- sites_LBA %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_LBA, vert.attr_LBA$sizeCat == 1 | vert.attr_LBA$sizeCat == 2 | 
                      vert.attr_LBA$sizeCat == 3)
vert.attr_LBA <- vert.attr_LBA %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig3 <- graph_from_data_frame(edgelist_LBA, directed = FALSE, vertices = vert.attr_LBA)
ig3 <- igraph::simplify(ig3)
## define layout to display the nodes with their real-world coordinates
lo3 <- layout.norm(as.matrix(vert.attr_LBA[, c("POINT_X", "POINT_Y")]))

nw_LBA <- intergraph::asNetwork(ig)


## plot network----
plot(nw_LBA,
     main = "Hollow way network LBA",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_LBA$Site_Nm)

# create a color map for surveys
col <- data.frame(Datst_1 = unique(vert.attr_LBA$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_LBA$color <- col$color[match(vert.attr_LBA$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_LBA, directed = FALSE, vertices = vert.attr_LBA)
ig <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_LBA[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = degree(ig)/2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = betweenness(ig)*1.2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = log(vert.attr_LBA$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_LBA$Site_Nm, vertex.size = log(vert.attr_LBA$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys LBA ----
## LLN survey ----
THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
vert.attr_THS <- as.data.frame(vertex_attr(THS))
lo2 <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
plot.igraph(THS, layout = lo2, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = degree(THS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = betweenness(THS)*0.7, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(THS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, rescale = F, vertex.color=cols[as.character(degree(THS))], vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS)*1.7, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.03, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, rescale = F, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.15), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, rescale = F, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)



# IA1 ----
## convert edgelist to adjaceny matrix ----
vert.attr_IA1 <- sites_IA1 %>%
  relocate(id) 

# change size column to avoid screwed results for size hypotheses: if it's 0, replace it by the median value of the three smallest bins
size_bins <- subset(vert.attr_IA1, vert.attr_IA1$sizeCat == 1 | vert.attr_IA1$sizeCat == 2 | 
                      vert.attr_IA1$sizeCat == 3)
vert.attr_IA1 <- vert.attr_IA1 %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#create igraph
ig4 <- graph_from_data_frame(edgelist_IA1, directed = FALSE, vertices = vert.attr_IA1)
ig4 <- igraph::simplify(ig4)
## define layout to display the nodes with their real-world coordinates
lo4 <- layout.norm(as.matrix(vert.attr_IA1[, c("POINT_X", "POINT_Y")]))

nw_IA1 <- intergraph::asNetwork(ig)


## plot network----
plot(nw_IA1,
     main = "Hollow way network IA1",
     cex.main  =0.8,
     vertex.cex = 1,
     label.cex = 0.5,
     label = vert.attr_IA1$Site_Nm)

# create a color map for surveys
col <- data.frame(Datst_1 = unique(vert.attr_IA1$Datst_1), stringsAsFactors = F)
#col$color <- brewer.pal(nrow(col), "YlOrRd")
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_IA1$color <- col$color[match(vert.attr_IA1$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_IA1, directed = FALSE, vertices = vert.attr_IA1)
ig <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_IA1[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = degree(ig)/1.5, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = betweenness(ig)*1.2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = log(vert.attr_IA1$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig))], vertex.label = vert.attr_IA1$Site_Nm, vertex.size = log(vert.attr_IA1$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys IA1 ----
## LLN survey ----
THS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "THS"))
vert.attr_THS <- as.data.frame(vertex_attr(THS))
lo2 <- layout.norm(as.matrix(vert.attr_THS[, c("POINT_X", "POINT_Y")]))
plot.igraph(THS, layout = lo2, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = degree(THS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = betweenness(THS)*0.7, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(THS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(THS, layout = lo2, rescale = F, vertex.color=cols[as.character(degree(THS))], vertex.label = vertex_attr(THS, "Site_Nm"), vertex.size = log(vertex_attr(THS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## NJS survey----
NJS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "NJS"))
vert.attr_NJS <- as.data.frame(vertex_attr(NJS))
lo3 <- layout.norm(as.matrix(vert.attr_NJS[, c("POINT_X", "POINT_Y")]))
plot.igraph(NJS, layout = lo3, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = degree(NJS)*1.7, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = betweenness(NJS)*0.03, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(NJS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(NJS, layout = lo3, rescale = F, vertex.color=cols[as.character(degree(NJS))], vertex.label = vertex_attr(NJS, "Site_Nm"), vertex.size = log(vertex_attr(NJS, "Size") + 1, 1.15), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

## TBS survey----
TBS <- induced.subgraph(TBS, which(V(TBS)$Datst_1 == "TBS"))
vert.attr_TBS <- as.data.frame(vertex_attr(TBS))
lo4 <- layout.norm(as.matrix(vert.attr_TBS[, c("POINT_X", "POINT_Y")]))
plot.igraph(TBS, layout = lo4, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, size = 0.5)

### create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = degree(TBS)*3, vertex.label.cex = 0.8, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = betweeness centrality 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = betweenness(TBS)*4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2)

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, layout = lo4, vertex.label.dist = 1, rescale = F, vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)

# create color map for degree 
d = degree(TBS)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(TBS, rescale = F, layout = lo4, vertex.color=cols[as.character(degree(TBS))], vertex.label = vertex_attr(TBS, "Site_Nm"), vertex.size = log(vertex_attr(TBS, "Size") + 1, 1.1), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2)




# Network metrics over time ----
metrics_man <- read_xlsx("Network_metrics.xlsx", sheet = "Manual network")
metrics_man <- subset(metrics_man[1:6,])

metrics_comp3000 <- read_xlsx("Network_metrics.xlsx", sheet = "Computational network 3000")
metrics_comp3000 <- subset(metrics_comp3000 [1:6,])

metrics_comp5500 <- read_xlsx("Network_metrics.xlsx", sheet = "Computational network 5500")
metrics_comp5500 <- subset(metrics_comp5500 [1:6,])

metrics_man <- metrics_man %>% 
  mutate_at(c('transitivity', 'closeness_centrality'), as.numeric)

metrics_comp3000 <- metrics_comp3000 %>% 
  mutate_at(c('transitivity', 'betweenness_centrality'), as.numeric)

metrics_comp5500 <- metrics_comp5500 %>% 
  mutate_at(c('transitivity', 'closeness_centrality', 'betweenness_centrality'), as.numeric)

## plot metrics over time 
metrics_man$period <- factor(metrics_man$period, levels = metrics_man$period)
metrics_comp3000$period <- factor(metrics_comp3000$period, levels = metrics_comp3000$period)
metrics_comp5500$period <- factor(metrics_comp5500$period, levels = metrics_comp5500$period)

ggplot(metrics_man, aes(y = period, x = edge_density)) +
  geom_point() +
  scale_y_discrete(limits = rev)

p1 <- ggplot(NULL, aes(y = period, x = edge_density)) +
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685"))

p2 <- ggplot(NULL, aes(y = period, x = mean_degree)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685")) 


p3 <- ggplot(NULL, aes(y = period, x = transitivity)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685"))

p4 <- ggplot(NULL, aes(y = period, x = degree_centrality)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685"))
  #scale_color_manual(values = sample(plasma_colors, 3))

# p5 <- ggplot(NULL, aes(y = period, x = closeness_centrality)) + 
#   geom_point(data = metrics_man) +
#   #geom_point(data = metrics_man, aes(color = "darkblue")) +
#   #geom_point(data = metrics_comp3000,aes(color = "darkred")) +
#   scale_y_discrete(limits = rev) #+
#   labs(color = "Network") +
#   scale_color_hue(labels=c("Manual","Computational")) 

# p6 <- ggplot(NULL, aes(y = period, x = eigen_centrality)) + 
#   geom_point(data = metrics_man) +
#   #geom_point(data = metrics_man, aes(color = "darkblue")) +
#   #geom_point(data = metrics_comp3000,aes(color = "darkred")) +
#   scale_y_discrete(limits = rev) #+
#   labs(color = "Network") +
#   scale_color_hue(labels=c("Manual","Computational")) 

p7 <- ggplot(NULL, aes(y = period, x = betweenness_centrality)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685"))

p8 <- ggplot(NULL, aes(y = period, x = mean_distance)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685")) 

p9 <- ggplot(NULL, aes(y = period, x = largest_clique)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685")) 

p10 <- ggplot(NULL, aes(y = period, x = communities)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685")) 

p11 <- ggplot(NULL, aes(y = period, x = modularity)) + 
  geom_point(data = metrics_man, aes(color = "Manual")) +
  geom_point(data = metrics_comp5500, aes(color = "Computational 5500")) +
  geom_point(data = metrics_comp3000, aes(color = "Computational 3000")) +
  scale_y_discrete(limits = rev) +
  labs(color = "Network") +
  scale_color_manual(values = c("#20114b", "#e76f5a", "#21a685"))


plot_grid(p1 + theme(legend.position="none"), 
          p2 + theme(axis.title.y = element_blank(), legend.position="none"), 
          p3 + theme(axis.title.y = element_blank()), 
          p4 + theme(legend.position="none"), 
          p7 + theme(axis.title.y = element_blank(), legend.position="none"),
          p8 + theme(axis.title.y = element_blank()), 
          p9 + theme(legend.position="none"),
          p10 + theme(axis.title.y = element_blank(), legend.position="none"),
          p11 + theme(axis.title.y = element_blank()),
          labels = "AUTO", align = "hv", axis = "btlr")


# Period plots ----
# arrange layout for plots
vert.attr_MBA <- sites_MBA %>%
  relocate(id) 

## create and plot igraph with spatial distribution----
ig4 <- graph_from_data_frame(edgelist_MBA, directed = FALSE, vertices = vert.attr_MBA)
ig4 <- igraph::simplify(ig4)
lo4 <- layout.norm(as.matrix(vert.attr_MBA[, c("POINT_X", "POINT_Y")]))

# https://stackoverflow.com/questions/10389967/common-legend-for-multiple-plots-in-r

png('hybrid_graphs_coloured_surveys.png', width = 20, height = 20, units = 'in', res=300)
#dev.new(width = 30, height = 30, unit = "cm")
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE)) # figure 1 in row 1, figure 2 and 3 in row 2

plot.igraph(ig, layout = lo, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_EBA1$color, asp = 0.5:1)
title("Early Bronze Age 1", cex.main=5)
plot.igraph(ig1, layout = lo1, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_EBA2$color, asp = 0.5:1)
title("Early Bronze Age 2", cex.main = 5)
plot.igraph(ig2, layout = lo2, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_MBA$color, asp = 0.5:1)
title("Middle Bronze Age", cex.main=5)
plot.igraph(ig3, layout = lo3, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_LBA$color, asp = 0.5:1)
title("Late Bronze Age", cex.main=5)
plot.igraph(ig4, layout = lo4, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_IA1$color, asp = 0.5:1)
title("Iron Age 1", cex.main=5)
plot.igraph(ig5, layout = lo5, rescale = F, vertex.label = NA, vertex.size = 2, edge.color = "grey39", edge.width = 3, vertex.color = vert.attr_IA2$color, asp = 0.5:1)
title("Iron Age 2", cex.main=5)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
#legend('bottom',legend = c("Tell Beydar Survey", "Leilan Regional Survey", "Tell Hammoukar Survey", "North Jazira Survey", "Additional sites"), col = c("#FD8D3C","#FFFFB2", "#F03B20", "#FECC5C", "#BD0026"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 2.5)
legend('bottom',legend = c("Tell Beydar Survey", "Leilan Regional Survey", "Tell Hammoukar Survey", "North Jazira Survey", "Additional sites"), col = c("darkgreen","yellow", "orange", "darkred", "blue"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 2.5)

dev.off()

png('MBA_graphs_comp_man_hybrid.png', width = 50, height = 35, units = 'in', res=300)
#dev.new(width = 30, height = 30, unit = "cm")
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) # figure 1 in row 1, figure 2 and 3 in row 2

plot.igraph(ig_MBA_man, layout = lo_MBA_man, rescale = F, vertex.label = NA, vertex.size = 2, vertex.color = "darkred", edge.color = "grey39", edge.width = 5, asp = 0.5:1)
title("Manual", cex.main=5)
#plot.igraph(ig_EBA1_hybrid, layout = lo3, rescale = F, vertex.label = NA, vertex.size = 2, vertex.color = "darkred", edge.color = "grey39", edge.width = 5, asp = 0.5:1)
#title("Hybrid", cex.main = 5)
plot.igraph(ig4, layout = lo4, rescale = F, vertex.label = NA, vertex.size = 2, vertex.color = "darkred", edge.color = "grey39", edge.width = 5, asp = 0.5:1)
title("Computational 1", cex.main=5)
plot.igraph(ig_MBA_3000, layout = lo_MBA_3000, rescale = F, vertex.label = NA, vertex.size = 2, vertex.color = "darkred", edge.color = "grey39", edge.width = 5, asp = 0.5:1)
title("Computational 2", cex.main=5)

dev.off()

# Network comparison ----
## Manual and comp_3000----

# EDGELISTS 
# define source file for edgelists 
path <- "edgelists_manual_new.xlsx"

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

all_sites <- read.csv("Khabur_Ane_pred_Tuna_dates_log.csv")
all_sites <- all_sites %>% relocate("id")

#fill in missing data in Size column by proxy
size_bins <- subset(all_sites, sizeCat == 1 | sizeCat == 2 | sizeCat == 3)
all_sites <- all_sites %>% 
  mutate(Size = case_when(is.na(Size) ~ median(size_bins$Size, na.rm = T),
                          TRUE ~ Size))

#NETWORKS FOR TERGM
x <- 1
for (elist in edgelists) {
  # extract the period from the edgelist
  period <- names(edgelists[x])
  # find the attribute for the period
  vert.attr <- all_sites
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))
  # create network
  nw <- intergraph::asNetwork(ig)
  # savw vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  gn <- paste("ig_all", period, sep = "_")
  data_ig <- ig
  assign(gn, data_ig)
  
  lon <- paste("lo_all", period, sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
  
  nwn <- paste("nw_all", period, sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)
  x <- x + 1
}

# create list of networks
list_nw_all <- list(nw_all_EBA1, nw_all_EBA2, nw_all_MBA, nw_all_LBA, nw_all_IA1, nw_all_IA2)
names(list_nw_all) <- list("EBA1", "EBA2", "MBA", "LBA", "IA1", "IA2")

EBA1_man <- ig_all_EBA1
EBA2_man <- ig_all_EBA2
MBA_man <- ig_all_MBA
LBA_man <- ig_all_LBA
IA1_man <- ig_all_IA1
IA2_man <- ig_all_IA2

# create adjacency matrices 
m1_EBA1 <- as_adjacency_matrix(ig_EBA1_man)
m1_EBA2 <- as_adjacency_matrix(EBA2_man)
m1_MBA <- as_adjacency_matrix(MBA_man)
m1_LBA <- as_adjacency_matrix(LBA_man)
m1_IA1 <- as_adjacency_matrix(IA1_man)
m1_IA2 <- as_adjacency_matrix(IA2_man)

# EDGELISTS 
# define source file for edgelists 
path <- "edgelists_comp_3000_new.xlsx"

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

#NETWORKS FOR TERGM
x <- 1
for (elist in edgelists) {
  # extract the period from the edgelist
  period <- names(edgelists[x])
  # find the attribute for the period
  vert.attr <- all_sites
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))
  # create network
  nw <- intergraph::asNetwork(ig)
  # savw vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  gn <- paste("ig_all", period, sep = "_")
  data_ig <- ig
  assign(gn, data_ig)
  
  lon <- paste("lo_all", period, sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
  
  nwn <- paste("nw_all", period, sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)
  x <- x + 1
}

# create list of networks
list_nw_all <- list(nw_all_EBA1, nw_all_EBA2, nw_all_MBA, nw_all_LBA, nw_all_IA1, nw_all_IA2)
names(list_nw_all) <- list("EBA1", "EBA2", "MBA", "LBA", "IA1", "IA2")

EBA1_3000 <- ig_all_EBA1
EBA2_3000 <- ig_all_EBA2
MBA_3000 <- ig_all_MBA
LBA_3000 <- ig_all_LBA
IA1_3000 <- ig_all_IA1
IA2_3000 <- ig_all_IA2

m2_EBA1 <- as_adjacency_matrix(ig_EBA1_3000)
m2_EBA2 <- as_adjacency_matrix(EBA2_3000)
m2_MBA <- as_adjacency_matrix(MBA_3000)
m2_LBA <- as_adjacency_matrix(LBA_3000)
m2_IA1 <- as_adjacency_matrix(IA1_3000)
m2_IA2 <- as_adjacency_matrix(IA2_3000)

m_EBA1 <- m1_EBA1 + m2_EBA1
m_EBA1[m_EBA1 == 1] <- 0
m_EBA1[m_EBA1 == 2] <- 1

m_EBA2 <- m1_EBA2 + m2_EBA2
m_EBA2[m_EBA2 == 1] <- 0
m_EBA2[m_EBA2 == 2] <- 1

m_MBA <- m1_MBA + m2_MBA
m_MBA[m_MBA == 1] <- 0
m_MBA[m_MBA == 2] <- 1

m_LBA <- m1_LBA + m2_LBA
m_LBA[m_LBA == 1] <- 0
m_LBA[m_LBA == 2] <- 1

m_IA1 <- m1_IA1 + m2_IA1
m_IA1[m_IA1 == 1] <- 0
m_IA1[m_IA1 == 2] <- 1

m_IA2 <- m1_IA2 + m2_IA2
m_IA2[m_IA2 == 1] <- 0
m_IA2[m_IA2 == 2] <- 1

perc_man_EBA1 <- (100/sum(!!m1_EBA1)) * sum(!!m_EBA1)
perc_man_EBA2 <- (100/sum(!!m1_EBA2)) * sum(!!m_EBA2)
perc_man_MBA <- (100/sum(!!m1_MBA)) * sum(!!m_MBA)
perc_man_LBA <- (100/sum(!!m1_LBA)) * sum(!!m_LBA)
perc_man_IA1 <- (100/sum(!!m1_IA1)) * sum(!!m_IA1)
perc_man_IA2 <- (100/sum(!!m1_IA2)) * sum(!!m_IA2)

perc_comp_EBA1 <- (100/sum(!!m2_EBA1)) * sum(!!m_EBA1)
perc_comp_EBA2 <- (100/sum(!!m2_EBA2)) * sum(!!m_EBA2)
perc_comp_MBA <- (100/sum(!!m2_MBA)) * sum(!!m_MBA)
perc_comp_LBA <- (100/sum(!!m2_LBA)) * sum(!!m_LBA)
perc_comp_IA1 <- (100/sum(!!m2_IA1)) * sum(!!m_IA1)
perc_comp_IA2 <- (100/sum(!!m2_IA2)) * sum(!!m_IA2)


## Manual and comp_5500----
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


#NETWORKS FOR TERGM
x <- 1
for (elist in edgelists) {
  # extract the period from the edgelist
  period <- names(edgelists[x])
  # find the attribute for the period
  vert.attr <- all_sites
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))
  # create network
  nw <- intergraph::asNetwork(ig)
  # savw vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  gn <- paste("ig_all", period, sep = "_")
  data_ig <- ig
  assign(gn, data_ig)
  
  lon <- paste("lo_all", period, sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
  
  nwn <- paste("nw_all", period, sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)
  x <- x + 1
}

EBA1_5500 <- ig_all_EBA1
EBA2_5500 <- ig_all_EBA2
MBA_5500 <- ig_all_MBA
LBA_5500 <- ig_all_LBA
IA1_5500 <- ig_all_IA1
IA2_5500 <- ig_all_IA2

m2_EBA1 <- as_adjacency_matrix(EBA1_5500)
m2_EBA2 <- as_adjacency_matrix(EBA2_5500)
m2_MBA <- as_adjacency_matrix(MBA_5500)
m2_LBA <- as_adjacency_matrix(LBA_5500)
m2_IA1 <- as_adjacency_matrix(IA1_5500)
m2_IA2 <- as_adjacency_matrix(IA2_5500)

m_EBA1 <- m1_EBA1 + m2_EBA1
m_EBA1[m_EBA1 == 1] <- 0
m_EBA1[m_EBA1 == 2] <- 1

m_EBA2 <- m1_EBA2 + m2_EBA2
m_EBA2[m_EBA2 == 1] <- 0
m_EBA2[m_EBA2 == 2] <- 1

m_MBA <- m1_MBA + m2_MBA
m_MBA[m_MBA == 1] <- 0
m_MBA[m_MBA == 2] <- 1

m_LBA <- m1_LBA + m2_LBA
m_LBA[m_LBA == 1] <- 0
m_LBA[m_LBA == 2] <- 1

m_IA1 <- m1_IA1 + m2_IA1
m_IA1[m_IA1 == 1] <- 0
m_IA1[m_IA1 == 2] <- 1

m_IA2 <- m1_IA2 + m2_IA2
m_IA2[m_IA2 == 1] <- 0
m_IA2[m_IA2 == 2] <- 1

perc_man_EBA1 <- (100/sum(!!m1_EBA1)) * sum(!!m_EBA1)
perc_man_EBA2 <- (100/sum(!!m1_EBA2)) * sum(!!m_EBA2)
perc_man_MBA <- (100/sum(!!m1_MBA)) * sum(!!m_MBA)
perc_man_LBA <- (100/sum(!!m1_LBA)) * sum(!!m_LBA)
perc_man_IA1 <- (100/sum(!!m1_IA1)) * sum(!!m_IA1)
perc_man_IA2 <- (100/sum(!!m1_IA2)) * sum(!!m_IA2)

perc_comp_EBA1 <- (100/sum(!!m2_EBA1)) * sum(!!m_EBA1)
perc_comp_EBA2 <- (100/sum(!!m2_EBA2)) * sum(!!m_EBA2)
perc_comp_MBA <- (100/sum(!!m2_MBA)) * sum(!!m_MBA)
perc_comp_LBA <- (100/sum(!!m2_LBA)) * sum(!!m_LBA)
perc_comp_IA1 <- (100/sum(!!m2_IA1)) * sum(!!m_IA1)
perc_comp_IA2 <- (100/sum(!!m2_IA2)) * sum(!!m_IA2)

## number of edges: gsize()
## number of matching edges: sum(!!m_EBA1)/2

## comparing sites ----
# statistics: 
betweenness(ig)
median(betweenness(ig))
max(betweenness(ig))
sort(betweenness(ig))
quantile(betweenness(ig))

degree(ig)
max(degree(ig))
sort(degree(ig))
quantile(degree(ig))

# EBA1_man: median betweenness = 66,8; max betweenness = 1981,949 (Tell Talab), max degree 9 (Tell Khazna, Tell Kaferu, NJS_80, Tell el-Uwaynat, Qarassa)
# EBA2_man: median betweenness = 29; max betweenness = 1484,798 (Abtakh Fowqani), max degree 8 (Abtakh Fowqani)
# MBA_man: median betweenness = 8,7; max betweenness = 6819.167 (Qarassa), max degree 9 (Tell Warada or Hilawat)
# LBA_man: median betweenness = 18.57; max betweenness = 981.47 (Abtakh Fowqani), max degree 7 (Abtakh Fowqani, Tell Tamr)
# IA1_man: median betweenness = 23,71; max betweenness = 1463.317 (Abtakh Fowqani), max degree 7 (Abtakh Fowqani, TBS_21, NJS_29)
# IA2_man: median betweenness = 159; max betweenness = 5464.391 (Tell Hamidiya), max degree 7 (NJS_29)

# EBA1_5500: median betweenness = 0; max betweenness = 207.5741505 (NJS_17), max degree 18 (NJS_35)
# EBA2_5500: median betweenness = 0; max betweenness = 219,9291, max degree 15 (Wulayqi Thirthar, Aweinat el-Amrin)
# MBA_5500: median betweenness = 7,25; max betweenness = 999.6381425 (LLN_69), max degree 22 (Wulayqi Thirthar)
# LBA_5500: median betweenness = 0; max betweenness = 88 (LLN_69), max degree 13 (NJS_39, NJS_10)
# IA1_5500: median betweenness = 0; max betweenness = 770.913 (NJS_48), max degree 13 (NJS_39, NJS_37)
# IA2_5500: median betweenness = 19.91116; max betweenness = 890.765040 (NJS_160), max degree 20 (NJS_14)

# EBA1_3000: median betweenness = 0; max betweenness = 202.931 (Biyada Foqani), max degree 19 (Biyada Foqani)
# EBA2_3000: median betweenness = 0; max betweenness = 352.83 (Aweinat el-Amrin), max degree 10 (Taya)
# MBA_3000: median betweenness = 8.783802; max betweenness = 1591.831 (Aweinat el-Amrin); max degree 22 (LLN_46)
# LBA_3000: median betweenness = 0; max betweenness = 98.4238095 (Tell Qarayah al-Botha), max degree 10 (NJS_22)
# IA1_3000: median betweenness = 0; max betweenness = 98.4238095 (Tell Qarayah al-Botha), max degree 10 (NJS_22)
# IA2_3000: median betweenness = 0.3; max betweenness = 398.3529705 (NJS_147), max degree 21 (Biyada Foqani, NJS_79)

# EBA1_hybrid: median betweenness = 62.92359; max betweenness = 2301.2789727 (Tell al-Sara), max degree 20 (NJS_42)
# EBA2_hybrid: median betweenness = 23.7; max betweenness = 1222.7698196 (Na'am Hayar), max degree 10 (Gunduk)
# MBA_hybrid: median betweenness = 45.75; max betweenness = 7673 (Umm Adam); max degree 22 (LLN_46)
# LBA_hybrid: median betweenness = 34.83333; max betweenness = 2139.9036888 (Abtakh Fowqani), max degree 11 (NJS_22)
# IA1_hybrid: median betweenness = 47.53333; max betweenness = 1546.6197924 (Abtakh Fowqani), max degree 11 (NJS_22)
# IA2_hybrid: median betweenness = 132.319; max betweenness = 3370.860754 (NJS_132), max degree 21 (NJS_79)



### Tell Leilan ----
# Tell Farfara as main centre since MBA 
degree(ig, v = 1)
# EBA1_man = 6 (fourth quartile)
# EBA2_man = 4 (third quartile)
# MBA_man = 3 (third quartile)
# LBA_man = 3 (third quartile)
# IA1_man = 2 (first quartile)

# EBA1_5500 = 7 (third quartile)
# EBA2_5500 = 11 (fourth quartile)
# MBA_5500 = 13 (fourth quartile)
# LBA_5500 = 3 (third quartile)
# IA1_5500 = 2 (second quartile)

# EBA1_3000 = 2 (second quartile)
# EBA2_3000 = 4 (third quartile)
# MBA_3000 = 7 (third quartile)
# LBA_3000 = 0 (second quartile)
# IA1_3000 = 0 (second quartile)

# EBA1_hybrid = 6 
# EBA2_hybrid = 7 
# MBA_hybrid = 8 
# LBA_hybrid = 3
# IA1_hybrid = 2 

betweenness(ig, v = 1)
# EBA1_man = 296 (fourth quartile)
# EBA2_man = 140.6336 (second quartile)
# MBA_man = 1449.5 (fourth quartile)
# LBA_man = 8 (second quartile)
# IA1_man = 231 (fourth quartile)

# EBA1_5500 = 6.194434 (third quartile)
# EBA2_5500 = 20.99351 (fourth quartile)
# MBA_5500 = 74.72919 (fourth quartile)
# LBA_5500 = 4 (third quartile)
# IA1_5500 = 0 (third quartile)

# EBA1_3000 = 0 (third quartile)
# EBA2_3000 = 0 (third quartile)
# MBA_3000 = 29.53864 (third quartile)
# LBA_3000 = 0 (third quartile)
# IA1_3000 = 0 (third quartile)

# EBA1_hybrid = 125.9808 
# EBA2_hybrid = 304.7842
# MBA_hybrid = 7.557089 
# LBA_hybrid = 364 
# IA1_hybrid = 237 

### Tell Farfara ----
degree(ig, v = 135)
# MBA_man =  5 (fourth quartile)
# LBA_man =  4 (third quartile)
# IA1_man = 4 (fourth quartile)

# MBA_5500 = 5 (second quartile)
# LBA_5500 = 2 (second quartile)
# IA1_5500 = 2 (second quartile)

# MBA_3000 = 3 (second quartile)
# LBA_3000 = 1 (second quartile)
# IA1_3000 = 0 (second quartile)

# MBA_hybrid = 6 
# LBA_hybrid = 5 
# IA1_hybrid = 4 

betweenness(ig, v = 135)
# MBA_man =  1441 (fourth quartile)
# LBA_man = 215 (fourth quartile)
# IA1_man = 846 (fourth quartile)

# MBA_5500 = 8.629449 (third quartile)
# LBA_5500 = 0 (third quartile)
# IA1_5500 = 4 (third quartile)

# MBA_3000 = 36.6 (third quartile)
# LBA_3000 = 0 (third quartile)
# IA1_3000 = 0 (third quartile)

# MBA_hybrid = 1080.582 
# LBA_hybrid = 452.5 
# IA1_hybrid = 389.7

### Mohammed Diyab ----
degree(ig, v = 13)
# EBA1_man = 3 (fourth quartile)
# EBA2_man = 5 (fourth quartile)
# MBA_man = 6 (fourth quartile)
# LBA_man = 1 (first quartile)
# IA1_man = 1 (first quartile)

# EBA1_5500 = 7 (third quartile)
# EBA2_5500 = 5 (third quartile)
# MBA_5500 = 14 (fourth quartile)
# LBA_5500 = 2 (second quartile)
# IA1_5500 = 3 (third quartile)

# EBA1_3000 = 2 (second quartile)
# EBA2_3000 = 2 (second quartile)
# MBA_3000 = 3 (second quartile)
# LBA_3000 = 0 (second quartile)
# IA1_3000 = 0 (second quartile)

# EBA1_hybrid = 6 
# EBA2_hybrid = 5 
# MBA_hybrid = 3 
# LBA_hybrid = 1 
# IA1_hybrid = 1 

betweenness(ig, v = 13)
# EBA1_man = 27.46927 (fourth quartile)
# EBA2_man = 212.4929 (fourth quartile)
# MBA_man = 149.3154 (fourth quartile)
# LBA_man = 0 (second quartile)
# IA1_man = 0 (fourth quartile)

# EBA1_5500 = 8.724992 (third quartile)
# EBA2_5500 = 2.898998 (third quartile)
# MBA_5500 = 74.72919 (fourth quartile)
# LBA_5500 = 0 (third quartile)
# IA1_5500 = 611.3571 (fourth quartile)

# EBA1_3000 = 0 (third quartile)
# EBA2_3000 = 148 (fourth quartile)
# MBA_3000 = 252.3452 (third quartile)
# LBA_3000 = 0 (third quartile)
# IA1_3000 = 0 (third quartile)

# EBA1_hybrid = 200.5368 
# EBA2_hybrid = 295.2689
# MBA_hybrid = 721.9579 
# LBA_hybrid = 0 
# IA1_hybrid = 0


### Tell Beydar ----
# Tell Sekar Foqani should be a centre in MBA but is not in the network
degree(ig, v = 79)
# EBA1_man = 5 (third quartile)
# EBA2_man = 5 (fourth quartile)
# MBA_man = na
# LBA_man = 4 (fourth quartile)
# IA1_man = 5 (fourth quartile)
# IA2_man = 6 (fourth quartile)

# EBA1_5500 = 1 (first quartile)
# EBA2_5500 = 2 (second quartile)
# MBA_5500 = na
# LBA_5500 = 1 (first quartile)
# IA1_5500 = 0 (first quartile)
# IA2_5500 = 1 (first quartile)

# EBA1_3000 = 0 (second quartile)
# EBA2_3000 = 0 (second quartile)
# MBA_3000 = na
# LBA_3000 = 0 (second quartile)
# IA1_3000 = 6 (fourth quartile)
# IA2_3000 = 1 (second quartile)

# EBA1_hybrid = 5 
# EBA2_hybrid = 5 
# MBA_hybrid = 3 
# LBA_hybrid = 4 
# IA1_hybrid = 5 
# IA2_hybrid = 6

betweenness(ig, v = 79)
# EBA1_man = 58.6749 (second quartile)
# EBA2_man = 20.96667 (second quartile)
# MBA_man = na
# LBA_man = 24,4 (third quartile)
# IA1_man = 37.20357 (third quartile)
# IA2_man = 875.626 (fourth quartile)

# EBA1_5500 = 0 (third quartile)
# EBA2_5500 = 22 (fourth quartile)
# MBA_5500 = na
# LBA_5500 = 0 (third quartile)
# IA1_5500 = 0 (third quartile)
# IA2_5500 = 0 (second quartile)

# EBA1_3000 = 0 (third quartile)
# EBA2_3000 = 0 (third quartile)
# MBA_3000 = na
# LBA_3000 = 0 (third quartile)
# IA1_3000 = 47.25 (fourth quartile)
# IA2_3000 = 0 (second quartile)

# EBA1_hybrid = 59.11542 (third quartile)
# EBA2_hybrid = 20.96667 (second quartile)
# MBA_hybrid = na
# LBA_hybrid = 47.25
# IA1_hybrid = 47.53333
# IA2_hybrid = 1442.222



### Tell Hamoukar ----
# Hamoukar abandoned in MBA, Khirbat al-'Abd as successor 
# shift from Khirbat al-'Abd to Tell Tamr in IA
degree(ig, v = 99)
# EBA1_man = 3 (second quartile)
# EBA2_man = 5 (fourth quartile)

# EBA1_5500 = 0 (first quartile)
# EBA2_5500 = 1 (first quartile)

# EBA1_3000 = 0 (second quartile)
# EBA2_3000 = 0 (second quartile)

# EBA1_hybrid = 3 (second quartile)
# EBA2_hybrid = 5 (third quartile)

betweenness(ig, v = 99)
# EBA1_man = 0 (fourth quartile)
# EBA2_man = 210.5453 (fourth quartile)

# EBA1_5500 = 0 (third quartile)
# EBA2_5500 = 0 (first quartile)

# EBA1_3000 = 0 (third quartile)
# EBA2_3000 = 0 (third quartile)

# EBA1_hybrid = 0 (second quartile)
# EBA2_hybrid = 265.4747 (second quartile)


### Khirbet al-'Abd----
degree(ig, v = 135)
# MBA_man = 5 (fourth quartile)
# LBA_man = 4 (third quartile)
# IA1_man = 4 (fourth quartile)
# IA2_man = 5 (fourth quartile)

# MBA_5500 = na
# LBA_5500 = na
# IA1_5500 = na
# IA2_5500 = na

# MBA_3000 = 0 (first quartile)
# LBA_3000 = 4 (third quartile)
# IA1_3000 = 3 (third quartile)
# IA2_3000 = 2 (second quartile)

# MBA_hybrid = 6 
# LBA_hybrid = 5 
# IA1_hybrid = 7 
# IA2_hybrid = 8 

betweenness(ig, v = 135)
# MBA_man = 114.0406 (third quartile)
# LBA_man = 164.4643 (fourth quartile)
# IA1_man = 224.519 (fourth quartile)
# IA2_man = 502.8394 (third quartile)

# MBA_5500 = na
# LBA_5500 = na
# IA1_5500 = na
# IA2_5500 = na

# MBA_3000 = 0 (second quartile)
# LBA_3000 = 0.3 (third quartile)
# IA1_3000 = 0 (third quartile)
# IA2_hybrid = 17 (third quartile)

# MBA_hybrid = 58.69397 
# LBA_hybrid = 249.0962 
# IA1_hybrid = 148.5437
# IA2_hybrid = 428.1705 

### Tell Tamr ----
degree(ig_EBA2_man, v = 154)
# IA1_man = 3 (third quartile)
# IA2_man = 3 (third quartile)

# IA1_5500 = 3 (third quartile)
# IA2_5500 = 0 (first quartile)

# IA1_3000 = 0 (second quartile)
# IA2_3000 = 0 (first quartile)

# IA1_hybrid = 0 (second quartile)
# IA2_hybrid = 0 (first quartile)

betweenness(ig_EBA2_man, v = 187)
# IA1_man = 1034.683 (fourth quartile)
# IA2_man = 884.7636 (fourth quartile)

# IA1_5500 = 39.817 (third quartile)
# IA2_5500 = 0 (second quartile)

# IA1_3000 = 0 (third quartile)
# IA2_3000 = 0 (second quartile)

# IA1_hybrid = 0 (second quartile)
# IA2_hybrid = 0 (first quartile)



### Tell Hawa ----
degree(ig, v = 22)
# EBA1_man = na
# EBA2_man = 7 (fourth quartile)
# MBA_man = 6 (fourth quartile)
# LBA_man = 5 (fourth quartile)
# IA1_man = 5 (fourth quartile)
# IA2_man = na

# EBA1_5500 = na
# EBA2_5500 = 1 (first quartile)
# MBA_5500 = 1 (first quartile)
# LBA_5500 = 4 (fourth quartile)
# IA1_5500 = 4 (third quartile)
# IA2_5500 = na

# EBA1_3000 = na
# EBA2_3000 = 1 (second quartile)
# MBA_3000 = 5 (third quartile)
# LBA_3000 = 2 (third quartile)
# IA1_3000 = 2 (third quartile)
# IA2_3000 = na

# EBA1_hybrid = na
# EBA2_hybrid = 8 
# MBA_hybrid = 10 
# LBA_hybrid = 8 
# IA1_hybrid = 8 
# IA2_hybrid = na

betweenness(ig, v = 22)
# EBA1_man = na
# EBA2_man = 392.9482 (fourth quartile) 
# MBA_man = 337.5713 (fourth quartile)
# LBA_man = 502.2902 (fourth quartile)
# IA1_man = 813.9167 (fourth quartile)
# IA2_man = na

# EBA1_5500 = na
# EBA2_5500 = 0 (first quartile)
# MBA_5500 = 0 (second quartile)
# LBA_5500 = 0 (third quartile)
# IA1_5500 = 0 (third quartile)
# IA2_5500 = na

# EBA1_3000 = na
# EBA2_3000 = 0 (third quartile)
# MBA_3000 = 170 (fourth quartile)
# LBA_3000 = 0 (third quartile)
# IA1_3000 = 0 (third quartile)
# IA2_3000 = na

# EBA1_hybrid = na
# EBA2_hybrid = 231.0209 
# MBA_hybrid = 63.34676 
# LBA_hybrid = 381.4186 
# IA1_hybrid = 319.8122
# IA2_hybrid = na



# Systemic bias ----
## set degree as vertex attribute
ig <- set_vertex_attr(ig, name = "degree", value = degree(ig))
LBA_hybrid <- data.frame(vertex_attr(ig))
write.csv(LBA_hybrid, "sites_LBA_hybrid_degree.csv")

all_plots <- list.files(pattern = "sites_\\w+_degree.csv$", full.names = TRUE)

# read csv into list 
list_all_attr <- lapply(all_plots, read.csv)

# set the names of the files 
names(list_all_attr) <- gsub("_degree.csv", "", gsub("sites_", "", list.files(pattern = "sites_\\w+_degree.csv$", full.names = F)))

list2env(lapply(list_all_attr, as.data.frame),envir = .GlobalEnv)

## Relation degree and site size ----
ggplot() +
  geom_point(data = EBA1, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = EBA1_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = EBA1_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = EBA1_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = EBA1, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA1_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA1_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA1_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("EBA1") +
  #scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1))
  #scale_y_continuous(trans='log10')
  scale_y_sqrt()
  #scale_x_sqrt()
  #scale_y_continuous(limits = c(0, 45))

ggplot() +
  geom_point(data = EBA2, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = EBA2_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = EBA2_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = EBA2_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = EBA2, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA2_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA2_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = EBA2_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("EBA2")

ggplot() +
  geom_point(data = MBA, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = MBA_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = MBA_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = MBA_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = MBA, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = MBA_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = MBA_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = MBA_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("MBA")

ggplot() +
  geom_point(data = LBA, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = LBA_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = LBA_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = LBA_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = LBA, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = LBA_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = LBA_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = LBA_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("LBA")

ggplot() +
  geom_point(data = IA1, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = IA1_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = IA1_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = IA1_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = IA1, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = IA1_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = IA1_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = IA1_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("IA1")

ggplot() +
  geom_point(data = IA2, aes(y = Size, x = degree, color = "Manual")) +                                      
  geom_point(data = IA2_3000, aes(y = Size, x = degree, color = "Computational 3000")) +
  geom_point(data = IA2_5500, aes(y = Size, x = degree, color = "Computational 5500")) +
  geom_point(data = IA2_hybrid, aes(y = Size, x = degree, color = "Hybrid")) +
  geom_smooth(data = IA2, aes(y = Size, x = degree, color = "Manual"), method = "lm", se = FALSE) +
  geom_smooth(data = IA2_3000, aes(y = Size, x = degree, color = "Computational 3000"), method = "lm", se = FALSE) +
  geom_smooth(data = IA2_5500, aes(y = Size, x = degree, color = "Computational 5500"), method = "lm", se = FALSE) +
  geom_smooth(data = IA2_hybrid, aes(y = Size, x = degree, color = "Hybrid"), method = "lm", se = FALSE) +
  ggtitle("IA2")

# Relation number of nodes per size ----
ls <- list(EBA1, EBA1_3000, EBA1_5500, EBA1_hybrid, EBA2, EBA2_3000, EBA2_5500, EBA2_hybrid, MBA, MBA_3000, MBA_5500, 
           MBA_hybrid, LBA, LBA_3000, LBA_5500, LBA_hybrid, IA1, IA1_3000, IA1_5500, IA1_hybrid, IA2, IA2_3000, IA2_5500, IA2_hybrid)

ls <- lapply(ls, transform, Size_round = round(Size))
names(ls) <- c("EBA1", "EBA1_3000", "EBA1_5500", "EBA1_hybrid", "EBA2", "EBA2_3000", "EBA2_5500", "EBA2_hybrid","MBA",
               "MBA_3000", "MBA_5500","MBA_hybrid", "LBA", "LBA_3000", "LBA_5500", "LBA_hybrid", "IA1", "IA1_3000", 
               "IA1_5500", "IA1_hybrid", "IA2", "IA2_3000", "IA2_5500", "IA2_hybrid")
list2env(lapply(ls, as.data.frame),envir = .GlobalEnv)


ggplot(NULL, aes(x = Size_round)) +
  geom_jitter(data = EBA1, stat = "count", aes(x = Size_round, y = ..count.., color = "Manual")) +                                      
  geom_jitter(data = EBA1_3000, stat = "count", aes(x = Size_round, y = ..count.., color = "Computational 3000")) +
  geom_jitter(data = EBA1_5500, stat = "count", aes(x = Size_round, y = ..count.., color = "Computational 5500")) +
  geom_jitter(data = EBA1_hybrid, stat = "count", aes(x = Size_round, y = ..count.., color = "Hybrid")) +
  scale_x_continuous(breaks = round(seq(min(EBA1$Size_round), max(EBA1$Size_round), by = 10),1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  ggtitle("EBA1")


# Hybrid network ----

# EDGELISTS 
# define source file for edgelists 
path <- "edgelists_manual_new.xlsx"

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


all_attr <- list.files(pattern = "\\w+_sites_id_log.csv$", full.names = TRUE)

# read csv into list 
list_all_attr <- lapply(all_attr, read.csv, row.names = 1) 

# set the names of the files 
names(list_all_attr) <- gsub(".csv", "", gsub("_id_log", "", list.files(pattern = "\\w+_sites_id_log.csv$", full.names = F)))

# create new ID column to match the edgelists
list_all_attr <- 
  map(list_all_attr, ~ .x %>%
        relocate(id))
names(list_all_attr) <- list("EBA1", "EBA2", "IA1", "IA2", "LBA", "MBA")

#NETWORKS FOR TERGM
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
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))
  # create network
  nw <- intergraph::asNetwork(ig)
  # savw vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  gn <- paste("ig", period, "man", sep = "_")
  data_ig <- ig
  assign(gn, data_ig)
  
  lon <- paste("lo", period, "man",sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
  
  nwn <- paste("nw", period, "man",sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)
  x <- x + 1
}

# EDGELISTS 
# define source file for edgelists 
path <- "edgelists_comp_3000_new.xlsx"

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
all_attr <- list.files(pattern = "sites_\\w+_node_comp_3000.csv", full.names = TRUE)

# read csv into list 
list_all_attr <- lapply(all_attr, read.csv, row.names = 1)

# set the names of the files 
names(list_all_attr) <- gsub(".csv", "", gsub("_id_log", "", list.files(pattern = "sites_\\w+_node_comp_3000.csv", full.names = F)))

# create new ID column to match the edgelists
list_all_attr <- 
  map(list_all_attr, ~ .x %>%
        relocate(id))
names(list_all_attr) <- list("EBA1", "EBA2", "IA1", "IA2", "LBA", "MBA")

#NETWORKS FOR TERGM
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
  # create igraph
  ig <- graph_from_data_frame(elist, directed = FALSE, vertices = vert.attr)
  # simplify igraph
  ig <- igraph::simplify(ig)
  # define layout to display the nodes with their real-world coordinates
  lo <- layout.norm(as.matrix(vert.attr[, c("POINT_X", "POINT_Y")]))
  # create network
  nw <- intergraph::asNetwork(ig)
  # savw vert.attr, ig, lo and nw to the environment with the respective period
  # in the object name
  gn <- paste("ig", period, "3000", sep = "_")
  data_ig <- ig
  assign(gn, data_ig)
  
  lon <- paste("lo", period, "3000",sep = "_")
  data_lo <- lo
  assign(lon, data_lo)
  
  nwn <- paste("nw", period, "3000",sep = "_")
  data_nw <- nw
  assign(nwn, data_nw)
  x <- x + 1
}

# create excel file for edgelists 
wb <- createWorkbook()
addWorksheet(wb, "EBA1")
addWorksheet(wb, "EBA2")
addWorksheet(wb, "MBA")
addWorksheet(wb, "LBA")
addWorksheet(wb, "IA1")
addWorksheet(wb, "IA2")

## EBA1 ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig_EBA1_man), as_edgelist(ig_EBA1_3000))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_EBA1_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig_EBA1_man)), as_data_frame(vertex_attr(ig_EBA1_3000)))
# remove the columns that are unique to either network
sites_EBA1_hybrid <- sites_EBA1_hybrid[ , -which(names(sites_EBA1_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_EBA1_hybrid <- sites_EBA1_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig_EBA1_hybrid <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_EBA1_hybrid)
# simplify graph
ig_EBA1_hybrid <- simplify(ig_EBA1_hybrid)
# convert graph to network 
nw_EBA1_hybrid <- intergraph::asNetwork(ig_EBA1_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_EBA1_man)
plot(nw_EBA1_3000)
plot(nw_EBA1_hybrid)

writeData(wb, "EBA1", edgelist_hybrid)
write.csv(sites_EBA1_hybrid, "sites_EBA1_hybrid.csv")

## EBA2 ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig), as_edgelist(ig2))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_EBA2_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig)), as_data_frame(vertex_attr(ig2)))
# remove the columns that are unique to either network
sites_EBA2_hybrid <- sites_EBA2_hybrid[ , -which(names(sites_EBA2_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_EBA2_hybrid <- sites_EBA2_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig3 <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_EBA2_hybrid)
# simplify graph
ig3 <- simplify(ig3)
lo3 <- layout.norm(as.matrix(sites_EBA2_hybrid[, c("POINT_X", "POINT_Y")]))
# convert graph to network 
nw_EBA2_hybrid <- intergraph::asNetwork(ig_EBA2_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_EBA2_man)
plot(nw_EBA2_3000)
plot(nw_EBA2_hybrid)

writeData(wb, "EBA2", edgelist_hybrid)
write.csv(sites_EBA2_hybrid, "sites_EBA2_hybrid.csv")

## MBA ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig_MBA_man), as_edgelist(ig_MBA_3000))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_MBA_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig_MBA_man)), as_data_frame(vertex_attr(ig_MBA_3000)))
# remove the columns that are unique to either network
sites_MBA_hybrid <- sites_MBA_hybrid[ , -which(names(sites_MBA_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_MBA_hybrid <- sites_MBA_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig_MBA_hybrid <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_MBA_hybrid)
# simplify graph
ig_MBA_hybrid <- simplify(ig_MBA_hybrid)
# convert graph to network 
nw_MBA_hybrid <- intergraph::asNetwork(ig_MBA_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_MBA_man)
plot(nw_MBA_3000)
plot(nw_MBA_hybrid)

writeData(wb, "MBA", edgelist_hybrid)
write.csv(sites_MBA_hybrid, "sites_MBA_hybrid.csv")

## LBA ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig_LBA_man), as_edgelist(ig_LBA_3000))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_LBA_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig_LBA_man)), as_data_frame(vertex_attr(ig_LBA_3000)))
# remove the columns that are unique to either network
sites_LBA_hybrid <- sites_LBA_hybrid[ , -which(names(sites_LBA_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_LBA_hybrid <- sites_LBA_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig_LBA_hybrid <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_LBA_hybrid)
# simplify graph
ig_LBA_hybrid <- simplify(ig_LBA_hybrid)
# convert graph to network 
nw_LBA_hybrid <- intergraph::asNetwork(ig_LBA_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_LBA_man)
plot(nw_LBA_3000)
plot(nw_LBA_hybrid)

writeData(wb, "LBA", edgelist_hybrid)
write.csv(sites_LBA_hybrid, "sites_LBA_hybrid.csv")

## IA1 ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig_IA1_man), as_edgelist(ig_IA1_3000))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_IA1_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig_IA1_man)), as_data_frame(vertex_attr(ig_IA1_3000)))
# remove the columns that are unique to either network
sites_IA1_hybrid <- sites_IA1_hybrid[ , -which(names(sites_IA1_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_IA1_hybrid <- sites_IA1_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig_IA1_hybrid <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_IA1_hybrid)
# simplify graph
ig_IA1_hybrid <- simplify(ig_IA1_hybrid)
# convert graph to network 
nw_IA1_hybrid <- intergraph::asNetwork(ig_IA1_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_IA1_man)
plot(nw_IA1_3000)
plot(nw_IA1_hybrid)

writeData(wb, "IA1", edgelist_hybrid)
write.csv(sites_IA1_hybrid, "sites_IA1_hybrid.csv")

## IA2 ----
# merge the edgelists of the manual and computational networks
edgelist_hybrid <- rbind(as_edgelist(ig_IA2_man), as_edgelist(ig_IA2_3000))
# remove duplicate rows 
edgelist_hybrid <- unique(edgelist_hybrid)
# rename columns
colnames(edgelist_hybrid) <- c("from", "to")
# merge the vertex attributes of the networks
sites_IA2_hybrid <- rbind.fill(as_data_frame(vertex_attr(ig_IA2_man)), as_data_frame(vertex_attr(ig_IA2_3000)))
# remove the columns that are unique to either network
sites_IA2_hybrid <- sites_IA2_hybrid[ , -which(names(sites_IA2_hybrid) %in% c("survey_log", "number"))]
# remove duplicate rows - if there are NAs in the Size column, drop those rows as well
sites_IA2_hybrid <- sites_IA2_hybrid %>%
  group_by(name) %>%
  slice(which.max(!is.na(Size))) %>% 
  dplyr::rename(id = name)
# create hybrid graph from edgelist and sites attributes 
ig_IA2_hybrid <- graph_from_data_frame(edgelist_hybrid, directed = FALSE, vertices = sites_IA2_hybrid)
# simplify graph
ig_IA2_hybrid <- simplify(ig_IA2_hybrid)
# convert graph to network 
nw_IA2_hybrid <- intergraph::asNetwork(ig_IA2_hybrid)

par(mfrow = c(2,2), oma = c(1,1,1,1), mar = c(4,1,1,1))               
plot(nw_IA2_man)
plot(nw_IA2_3000)
plot(nw_IA2_hybrid)

writeData(wb, "IA2", edgelist_hybrid)
write.csv(sites_IA2_hybrid, "sites_IA2_hybrid.csv")

saveWorkbook(wb, file = "edgelists_hybrid.xlsx", overwrite = TRUE)

# New sites ----
New_sites_EBA2 <- subset(sites_EBA2, !(sites_EBA2$Mj_ID_1 %in% sites_EBA1$Mj_ID_1))
New_sites_MBA <- subset(sites_MBA, !(sites_MBA$Mj_ID_1 %in% sites_EBA2$Mj_ID_1))
New_sites_LBA <- subset(sites_LBA, !(sites_LBA$Mj_ID_1 %in% sites_MBA$Mj_ID_1))
New_sites_IA1 <- subset(sites_IA1, !(sites_IA1$Mj_ID_1 %in% sites_LBA$Mj_ID_1))
New_sites_IA2 <- subset(sites_IA2, !(sites_IA2$Mj_ID_1 %in% sites_IA1$Mj_ID_1))

# Continuous sites ----
Con_sites_EBA2 <- subset(sites_EBA2, (sites_EBA2$Mj_ID_1 %in% sites_EBA1$Mj_ID_1))
Con_sites_MBA <- subset(sites_MBA, (sites_MBA$Mj_ID_1 %in% sites_EBA2$Mj_ID_1))
Con_sites_LBA <- subset(sites_LBA, (sites_LBA$Mj_ID_1 %in% sites_MBA$Mj_ID_1))
Con_sites_IA1 <- subset(sites_IA1, (sites_IA1$Mj_ID_1 %in% sites_LBA$Mj_ID_1))
Con_sites_IA2 <- subset(sites_IA2, (sites_IA2$Mj_ID_1 %in% sites_IA1$Mj_ID_1))

