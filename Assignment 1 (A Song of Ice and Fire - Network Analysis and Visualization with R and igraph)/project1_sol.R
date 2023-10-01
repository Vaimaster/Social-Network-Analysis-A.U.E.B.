#####################################
#   A SONG OF ICE AND FIRE NETWORK  #
#  SOCIAL NETWORK ANALYSIS (graph)  #
# VAIDOMARKAKIS PANAGIOTIS p2822203 #
#####################################

# First Question

#install.packages("igraph","rgl")
library("igraph","rgl")

alledges <- read.csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")
head(alledges)

cat("Min Weight:",paste(min(alledges$weight),",",sep = ""),"\nMax Weight:",max(alledges$weight),"\n")
# Keep only Source, Target and Weight columns
data = subset(alledges, select = c('Source', 'Target', 'weight'))
head(data)
str(data)
summary(data)

# Make the network
network = graph_from_data_frame(data, directed = FALSE, vertices = NULL)
plot(network, vertex.label = NA, edge.arrow.width = 0.5, vertex.size = 3, vertex.color = "lightblue", edge.width = data$weight/100, layout = layout_with_fr, main="Q1 graph")

# Second Question

# Number of vertices/nodes
vcount(network)

# Number of edges/links
ecount(network)

# Diameter
diameter(network, weights = data$weight)

# Unique Triangles
length(triangles(network))/3

# All triangles with duplicates
sum(count_triangles(network))

# Number of edges with weight more that 15
cat("Number of edges having weight more than 15: ", length(E(network)[which(E(network)$weight > 15)]), "\n")

# Top 10 characters of the network based on Degree
top_10_degree<-sort(degree(network, v = V(network),loops = TRUE, normalized = FALSE), decreasing = TRUE)[1:10]
for (i in 1:length(top_10_degree)) {
  if (i==1){cat("Top-10 characters by degree:\n")}
  cat(i, " - ", names(top_10_degree)[i], ": ", top_10_degree[i], "\n")
}

# Top 10 characters of the network based on weighted Degree
top_10_weighted_degree<-sort(strength(network, vids = V(network), loops = TRUE, weights = data$weight), decreasing = TRUE)[1:10]
for (i in 1:length(top_10_weighted_degree)) {
  if (i==1){cat("Top-10 characters by weighted degree:\n")}
  cat(i, " - ", names(top_10_weighted_degree)[i], ": ", top_10_weighted_degree[i], "\n")
}

# Top 10 characters of the network based on local clustering coefficients
lcc <- transitivity(network, type = "local",weights = data$weight)
j <- 1
for (i in 1:length(lcc)) {
  if (i==1){cat("Top-10 characters by local clustering coefficient:\n")}
  if (!is.na(lcc[i]) && lcc[i]==1){
    cat(j, " - ", V(network)$name[i], ": ", lcc[i], "\n")
    j=j+1
  }
}

# Top 10 characters of the network based on local clustering coefficients
print(transitivity(network, type = "globalundirected",weights = data$weight))

# Third Question

#Entire Plot
plot(network, vertex.label = NA, edge.arrow.width = 0.5, vertex.size = 3, vertex.color = "lightblue", edge.width = data$weight/100, layout = layout_with_fr, main="Entire Plot")
# Create subgraph with vertices that have degree >= 10
subgraph <- delete.vertices(network, V(network)[degree(network) < 10])
# Plot subgraph with custom plot parameters
plot(subgraph, vertex.label = NA, edge.arrow.width = 0.5, vertex.size = 3, vertex.color = "lightblue", edge.width = data$weight/100, layout = layout_with_fr, main="Subgraph with 10+ connections")
# Edge densities
cat("Entire graph density:",edge_density(network),"\nSubgraph density:",edge_density(subgraph),"\n")

# Fourth Question

# Calculate closeness centrality
closeness <- closeness(network)
top_15_closeness <- sort(closeness, decreasing=TRUE)[1:15]
for (i in 1:length(top_15_closeness)) {
  if (i==1){cat("Top-15 nodes by closeness centrality:\n")}
  cat(i, " - ", V(network)$name[which(closeness == top_15_closeness[i])], ": ", top_15_closeness[i], "\n")
}

# Calculate betweenness centrality
betweenness <- betweenness(network)
top_15_betweenness <- sort(betweenness, decreasing = TRUE)[1:15]
for (i in 1:length(top_15_betweenness)) {
  if (i==1){cat("Top-15 nodes by betweenness centrality:\n")}
  cat(i, " - ", V(network)$name[which(betweenness == top_15_betweenness[i])], ": ", top_15_betweenness[i], "\n")
}
# Jon Snow's rank
cat("Jon Snow's ranking position:\n","Closeness centrality: ", paste(rank(-closeness)[which(names(closeness) == "Jon-Snow")], "th\n",sep=""),"Betweenness centrality: ", paste(rank(-betweenness)[which(names(betweenness) == "Jon-Snow")],"st\n",sep = ""))

# Fifth Question

# Calculate Page Rank
pagerank <- page_rank(network, weights = data$weight)$vector
summary(pagerank)
plot(network, vertex.label = ifelse(pagerank > 0.02, V(network)$name, NA), edge.arrow.width = 0.5, vertex.size = pagerank * 500, vertex.color = "lightblue", edge.width = data$weight/10, layout = layout_with_fr, main="Pagerank Plot")