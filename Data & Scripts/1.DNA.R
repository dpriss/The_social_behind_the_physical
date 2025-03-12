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


# Import data ----
edgelist_EBA1 <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "EBA1")
edgelist_IA2 <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "IA2")
edgelist_EBA2 <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "EBA2")
edgelist_IA1 <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "IA1")
edgelist_LBA <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "LBA")
edgelist_MBA <- read_xlsx("Data/edgelists_hybrid.xlsx", sheet = "MBA")

sites_EBA1 <- read.csv("Data/sites_EBA1_hybrid.csv")
sites_IA2 <- read.csv("Data/sites_IA2_hybrid.csv")
sites_EBA2 <- read.csv("Data/sites_EBA2_hybrid.csv")
sites_IA1 <- read.csv("Data/sites_IA1_hybrid.csv")
sites_LBA <- read.csv("Data/sites_LBA_hybrid.csv")
sites_MBA <- read.csv("Data/sites_MBA_hybrid.csv")


# The following code was used to create the network graphs in Figure 3. We also provide the code for additional plots to help the reader with an initial overview of the data.

# EBA1 ----
## convert edgelist to adjacency matrix ----

## relocate IDs to align with requirements for graph_from_data_frame function
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
col$color <- c("yellow","darkred", "darkgreen", "orange", "blue")

# attach the colors to the nodes data.frame
vert.attr_EBA1$color <- col$color[match(vert.attr_EBA1$Datst_1, col$Datst_1)]

## create and plot igraph with spatial distribution----
ig <- graph_from_data_frame(edgelist_EBA1, directed = FALSE, vertices = vert.attr_EBA1)
ig <- igraph::simplify(ig)
lo <- layout.norm(as.matrix(vert.attr_EBA1[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----
## plot with color = survey and size = degree centrality 
plot.igraph(ig, layout = lo, rescale = F, vertex.label = vert.attr_EBA1$Site_Nm, vertex.size = degree(ig)/3, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
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
TBS <- induced.subgraph(ig, which(V(ig)$Datst_1 == "TBS"))
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
plot.igraph(ig5, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = degree(ig5)/3, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig5, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = betweenness(ig5)*0.4, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig5, layout = lo, rescale = F, vertex.label = vert.attr_IA2$Site_Nm, vertex.size = log(vert.attr_IA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig5)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig5, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig5))], vertex.label = vert.attr_IA2$Site_Nm, vertex.size = log(vert.attr_IA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys IA2 ----
## LLN survey ----
THS <- induced.subgraph(ig5, which(V(ig5)$Datst_1 == "THS"))
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
NJS <- induced.subgraph(ig5, which(V(ig5)$Datst_1 == "NJS"))
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
TBS <- induced.subgraph(ig5, which(V(ig5)$Datst_1 == "TBS"))
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

nw_EBA2 <- intergraph::asNetwork(ig1)

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
ig1 <- graph_from_data_frame(edgelist_EBA2, directed = FALSE, vertices = vert.attr_EBA2)
ig1  <- igraph::simplify(ig1)
lo <- layout.norm(as.matrix(vert.attr_EBA2[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig1, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = degree(ig1)/2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig1, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = betweenness(ig1)/10, vertex.label.cex = 1, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig1, layout = lo, rescale = F, vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = log(vert.attr_EBA2$Size + 1, 2), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig1)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig1, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig1))], vertex.label = vert.attr_EBA2$Site_Nm, vertex.size = log(vert.attr_EBA2$Size + 1, 1.5), vertex.label.cex = 0.7, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys EBA2 ----
## LLN survey ----
THS <- induced.subgraph(ig1, which(V(ig1)$Datst_1 == "THS"))
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
NJS <- induced.subgraph(ig1, which(V(ig1)$Datst_1 == "NJS"))
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
TBS <- induced.subgraph(ig1, which(V(ig1)$Datst_1 == "TBS"))
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

nw_MBA <- intergraph::asNetwork(ig2)


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
ig2 <- graph_from_data_frame(edgelist_MBA, directed = FALSE, vertices = vert.attr_MBA)
ig2 <- igraph::simplify(ig2)
lo <- layout.norm(as.matrix(vert.attr_MBA[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig2, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(degree(ig2))*1.5, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig2, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = betweenness(ig2)*0.4, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig2, layout = lo, rescale = F, vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(vert.attr_MBA$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig2)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig2, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig2))], vertex.label = vert.attr_MBA$Site_Nm, vertex.size = log(vert.attr_MBA$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys MBA ----
## LLN survey ----
THS <- induced.subgraph(ig2, which(V(ig2)$Datst_1 == "THS"))
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
NJS <- induced.subgraph(ig2, which(V(ig2)$Datst_1 == "NJS"))
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
TBS <- induced.subgraph(ig2, which(V(ig2)$Datst_1 == "TBS"))
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

nw_LBA <- intergraph::asNetwork(ig3)


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
ig3 <- graph_from_data_frame(edgelist_LBA, directed = FALSE, vertices = vert.attr_LBA)
ig3 <- igraph::simplify(ig3)
lo <- layout.norm(as.matrix(vert.attr_LBA[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig3, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = degree(ig3)/2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig3, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = betweenness(ig3)*1.2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig3, layout = lo, rescale = F, vertex.label = vert.attr_LBA$Site_Nm, vertex.size = log(vert.attr_LBA$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig3)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig3, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig3))], vertex.label = vert.attr_LBA$Site_Nm, vertex.size = log(vert.attr_LBA$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys LBA ----
## LLN survey ----
THS <- induced.subgraph(ig3, which(V(ig3)$Datst_1 == "THS"))
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
NJS <- induced.subgraph(ig3, which(V(ig3)$Datst_1 == "NJS"))
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
TBS <- induced.subgraph(ig3, which(V(ig3)$Datst_1 == "TBS"))
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

nw_IA1 <- intergraph::asNetwork(ig4)


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
ig4 <- graph_from_data_frame(edgelist_IA1, directed = FALSE, vertices = vert.attr_IA1)
ig4 <- igraph::simplify(ig4)
lo <- layout.norm(as.matrix(vert.attr_IA1[, c("POINT_X", "POINT_Y")]))

## create plots with different metrics ----

## plot with color = survey and size = degree centrality 
plot.igraph(ig4, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = degree(ig4)/1.5, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")
##asp sets the aspect ratio, i.e. displays the plot in the correct dimensions

## plot with color = survey and size = betweeness centrality
plot.igraph(ig4, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = betweenness(ig4)*1.2, vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

## plot with color = survey and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig4, layout = lo, rescale = F, vertex.label = vert.attr_IA1$Site_Nm, vertex.size = log(vert.attr_IA1$Size + 1, 2), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)
legend("topright", legend = col$Datst_1, pt.bg = col$color, bty = "n",
       pch = 21, col = "#777777")

# create color map for degree
d = degree(ig4)
cols = setNames(colorRampPalette(c("yellow", "red"))(length(unique(d))), sort(unique(d)))

## plot with color = degree and size = logarithmic size (with +1 to avoid negatives and base 2 to scale) 
plot.igraph(ig4, layout = lo, rescale = F, vertex.color=cols[as.character(degree(ig4))], vertex.label = vert.attr_IA1$Site_Nm, vertex.size = log(vert.attr_IA1$Size + 1, 1.5), vertex.label.cex = 0.5, vertex.label.color = "black", edge.width = 2, asp = 0.5:1)

# Surveys IA1 ----
## LLN survey ----
THS <- induced.subgraph(ig4, which(V(ig4)$Datst_1 == "THS"))
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
NJS <- induced.subgraph(ig4, which(V(ig4)$Datst_1 == "NJS"))
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
TBS <- induced.subgraph(ig4, which(V(ig4)$Datst_1 == "TBS"))
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


# Descriptives ----
# The following code can be used to explore descriptive network metrics. 
# Please note that “ig” changes every time a new period graph is created, so "ig" needs to be changed to "ig1", "ig2" etc. for the respective periods. 

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

# Figure 3 ----
# The following code was used to create figure 3 in the manuscript.

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
