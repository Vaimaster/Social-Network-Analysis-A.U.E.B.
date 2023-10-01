# Install these packages if you don't have them already

#install.packages("csv")
#install.packages("igraph")
#install.packages("dplyr")
#install.packages("RColorBrewer")
#install.packages("ggplot2")

#Libraries required in order to execute the following R code

require(csv)
require(igraph)
require(dplyr)
library(RColorBrewer)
library(ggplot2)

getwd()
# Set below your working directory

# setwd("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

# Question 1

#1) Read the csv files for the 5 first days of July:

df1 = read.csv("CSV Files/edgelist_2009_07_01.csv")
df2 = read.csv("CSV Files/edgelist_2009_07_02.csv")
df3 = read.csv("CSV Files/edgelist_2009_07_03.csv")
df4 = read.csv("CSV Files/edgelist_2009_07_04.csv")
df5 = read.csv("CSV Files/edgelist_2009_07_05.csv")

#2) Read the csv files with the topic per distinct user:

dftopic1 = read.csv("CSV Files/topic_of_interest_2009_07_01.csv")
dftopic2 = read.csv("CSV Files/topic_of_interest_2009_07_01.csv")
dftopic3 = read.csv("CSV Files/topic_of_interest_2009_07_01.csv")
dftopic4 = read.csv("CSV Files/topic_of_interest_2009_07_01.csv")
dftopic5 = read.csv("CSV Files/topic_of_interest_2009_07_01.csv")

#Creation of the graphs:

dfedgelist1 = df1[, c("from","to","weight")]
g1 <- graph_from_data_frame(dfedgelist1[,c("from","to","weight")], directed = TRUE)

dfedgelist2 = df2[, c("from","to","weight")]
g2 <- graph_from_data_frame(dfedgelist2[,c("from","to","weight")], directed = TRUE)

dfedgelist3 = df3[, c("from","to","weight")]
g3 <- graph_from_data_frame(dfedgelist3[,c("from","to","weight")], directed = TRUE)

dfedgelist4 = df4[, c("from","to","weight")]
g4 <- graph_from_data_frame(dfedgelist4[,c("from","to","weight")], directed = TRUE)

dfedgelist5 = df5[, c("from","to","weight")]
g5 <- graph_from_data_frame(dfedgelist5[,c("from","to","weight")], directed = TRUE)

#Update graphs with attributes for each user:

V(g1)$topic_of_interest <- dftopic1$topic_of_interest[match(V(g1)$name, dftopic1$user)]
V(g2)$topic_of_interest <- dftopic2$topic_of_interest[match(V(g2)$name, dftopic2$user)]
V(g3)$topic_of_interest <- dftopic3$topic_of_interest[match(V(g3)$name, dftopic3$user)]
V(g4)$topic_of_interest <- dftopic4$topic_of_interest[match(V(g4)$name, dftopic4$user)]
V(g5)$topic_of_interest <- dftopic5$topic_of_interest[match(V(g5)$name, dftopic5$user)]
rm(df1,df2,df3,df4,df5,dftopic1,dftopic2,dftopic3,dftopic4,dftopic5,dfedgelist1,dfedgelist2,dfedgelist3,dfedgelist4,dfedgelist5)

# Question 2

# Dates

date = c("2009-07-01", "2009-07-02", "2009-07-03", "2009-07-04", "2009-07-05")

# Number of vertices

verticesFrame <- data.frame("VerticesNumber"=c(vcount(g1), vcount(g2), vcount(g3), vcount(g4), vcount(g5)), 
                            "Date" = date)

# Number of edges

edgesFrame <- data.frame("EdgesNumber"=c(ecount(g1), ecount(g2), ecount(g3), ecount(g4), ecount(g5)), 
                         "Date" = date)

# Diameter

diameterFrame <- data.frame("Diameter"=c(diameter(g1, directed = TRUE), diameter(g2, directed = TRUE), diameter(g3, directed = TRUE), diameter(g4, directed = TRUE), diameter(g5, directed = TRUE)), 
                            "Date" = date)

# Average in-degree

inDegFrame <- data.frame("InDegree"=c(mean(degree(g1, mode="in")), mean(degree(g2, mode="in")), mean(degree(g3, mode="in")), mean(degree(g4, mode="in")), mean(degree(g5, mode="in"))), 
                         "Date" = date)

# Average out-degree

outDegFrame <- data.frame("OutDegree"=c(mean(degree(g1, mode="out")), mean(degree(g2, mode="out")), mean(degree(g3, mode="out")), mean(degree(g4, mode="out")), mean(degree(g5, mode="out"))), 
                          "Date" = date)

# Select the first 5 colors for dates in the Dark2 palette

cols <- brewer.pal(n=5, name="Dark2")
cols_t1 <- cols[as.numeric(factor(verticesFrame$Date))]
cols_t2 <- cols[as.numeric(factor(edgesFrame$Date))]
cols_t3 <- cols[as.numeric(factor(diameterFrame$Date))]
cols_t4 <- cols[as.numeric(factor(inDegFrame$Date))]
cols_t5 <- cols[as.numeric(factor(outDegFrame$Date))]

# Vertices plot

plot_vertices = ggplot(verticesFrame, aes(x = Date, y = VerticesNumber, fill = cols_t1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = VerticesNumber), vjust = -0.5, size = 3)+
  labs(x = "Date", y = "Number of Vertices") +
  ggtitle("Number of Network Vertices per day") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = FALSE)

plot_vertices

# Edges plot

plot_edges = ggplot(edgesFrame, aes(x = Date, y = EdgesNumber, fill = cols_t2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = EdgesNumber), vjust = -0.5, size = 3)+
  labs(x = "Date", y = "Number of Edges") +
  ggtitle("Number of Network Edges per day") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = FALSE)

plot_edges

# Diameter plot

plot_diam = ggplot(diameterFrame, aes(x = Date, y = Diameter, fill = cols_t3)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Diameter), vjust = -0.5, size = 3)+
  labs(x = "Date", y = "Graph Diameter") +
  ggtitle("Network Diameter per day") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = FALSE)

plot_diam

# Average in-degree plot

plot_avg_in_dg = ggplot(inDegFrame, aes(x = Date, y = InDegree, fill = cols_t4)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = InDegree), vjust = -0.5, size = 3)+
  labs(x = "Date", y = "Avg In Degree") +
  ggtitle("Average Network In-Degree per day") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = FALSE)

plot_avg_in_dg

# Average out-degree plot

plot_avg_PgRnk = ggplot(outDegFrame, aes(x = Date, y = OutDegree, fill = cols_t5)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = OutDegree), vjust = -0.5, size = 3)+
  labs(x = "Date", y = "Avg Out Degree") +
  ggtitle("Average Network Out-Degree per day") +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = FALSE)

plot_avg_PgRnk

# Question 3

# Top 10 Twitter users (in-degree)

inDegr1 <- sort(degree(g1, mode="in"), decreasing = TRUE)
top_10_inDegr1 <- data.frame("User"= head(names(inDegr1), 10), "In_Degree"=head(inDegr1, 10),"Date" = "2009-07-01")
inDegr2 <- sort(degree(g2, mode="in"), decreasing = TRUE)
top_10_inDegr2 <- data.frame("User"= head(names(inDegr2), 10), "In_Degree"=head(inDegr2, 10),"Date" = "2009-07-02")
inDegr3 <- sort(degree(g3, mode="in"), decreasing = TRUE)
top_10_inDegr3 <- data.frame("User"= head(names(inDegr3), 10), "In_Degree"=head(inDegr3, 10),"Date" = "2009-07-03")
inDegr4 <- sort(degree(g4, mode="in"), decreasing = TRUE)
top_10_inDegr4 <- data.frame("User"= head(names(inDegr4), 10), "In_Degree"=head(inDegr4, 10),"Date" = "2009-07-04")
inDegr5 <- sort(degree(g5, mode="in"), decreasing = TRUE)
top_10_inDegr5 <- data.frame("User"= head(names(inDegr5), 10), "In_Degree"=head(inDegr5, 10),"Date" = "2009-07-05")
combined_df_In_Dg <- rbind(top_10_inDegr1, top_10_inDegr2, top_10_inDegr3, top_10_inDegr4, top_10_inDegr5)
rownames(combined_df_In_Dg) <- 1:nrow(combined_df_In_Dg)
combined_df_In_Dg <- combined_df_In_Dg[order(combined_df_In_Dg$Date, -combined_df_In_Dg$In_Degree), ]
View(combined_df_In_Dg)
rm(inDegr1,inDegr2,inDegr3,inDegr4,inDegr5)

# Top 10 Twitter users (out-degree)

outDegr1 <- sort(degree(g1, mode="out"), decreasing = TRUE)
top_10_outDegr1 <- data.frame("User"= head(names(outDegr1), 10), "Out_Degree"=head(outDegr1, 10),"Date" = "2009-07-01")
outDegr2 <- sort(degree(g2, mode="out"), decreasing = TRUE)
top_10_outDegr2 <- data.frame("User"= head(names(outDegr2), 10), "Out_Degree"=head(outDegr2, 10),"Date" = "2009-07-02")
outDegr3 <- sort(degree(g3, mode="out"), decreasing = TRUE)
top_10_outDegr3 <- data.frame("User"= head(names(outDegr3), 10), "Out_Degree"=head(outDegr3, 10),"Date" = "2009-07-03")
outDegr4 <- sort(degree(g4, mode="out"), decreasing = TRUE)
top_10_outDegr4 <- data.frame("User"= head(names(outDegr4), 10), "Out_Degree"=head(outDegr4, 10),"Date" = "2009-07-04")
outDegr5 <- sort(degree(g5, mode="out"), decreasing = TRUE)
top_10_outDegr5 <- data.frame("User"= head(names(outDegr5), 10), "Out_Degree"=head(outDegr5, 10),"Date" = "2009-07-05")
combined_df_Out_Dg <- rbind(top_10_outDegr1, top_10_outDegr2, top_10_outDegr3, top_10_outDegr4, top_10_outDegr5)
rownames(combined_df_Out_Dg) <- 1:nrow(combined_df_Out_Dg)
combined_df_Out_Dg <- combined_df_Out_Dg[order(combined_df_Out_Dg$Date, -combined_df_Out_Dg$Out_Degree), ]
View(combined_df_Out_Dg)
rm(outDegr1,outDegr2,outDegr3,outDegr4,outDegr5)

# Top 10 Twitter users (PageRank) 

pgrnk1 <- sort(page.rank(g1, weights = E(g1)$weight, directed = TRUE)$vector, decreasing = TRUE)
top_10_pgrnk1 <- data.frame("User"= head(names(pgrnk1), 10), "PageRank"=head(pgrnk1, 10),"Date" = "2009-07-01")
pgrnk2 <- sort(page.rank(g2, weights = E(g2)$weight, directed = TRUE)$vector, decreasing = TRUE)
top_10_pgrnk2 <- data.frame("User"= head(names(pgrnk2), 10), "PageRank"=head(pgrnk2, 10),"Date" = "2009-07-02")
pgrnk3 <- sort(page.rank(g3, weights = E(g3)$weight, directed = TRUE)$vector, decreasing = TRUE)
top_10_pgrnk3 <- data.frame("User"= head(names(pgrnk3), 10), "PageRank"=head(pgrnk3, 10),"Date" = "2009-07-03")
pgrnk4 <- sort(page.rank(g4, weights = E(g4)$weight, directed = TRUE)$vector, decreasing = TRUE)
top_10_pgrnk4 <- data.frame("User"= head(names(pgrnk4), 10), "PageRank"=head(pgrnk4, 10),"Date" = "2009-07-04")
pgrnk5 <- sort(page.rank(g5, weights = E(g5)$weight, directed = TRUE)$vector, decreasing = TRUE)
top_10_pgrnk5 <- data.frame("User"= head(names(pgrnk5), 10), "PageRank"=head(pgrnk5, 10),"Date" = "2009-07-05")
combined_df_PgRnk <- rbind(top_10_pgrnk1, top_10_pgrnk2, top_10_pgrnk3, top_10_pgrnk4, top_10_pgrnk5)
rownames(combined_df_PgRnk) <- 1:nrow(combined_df_PgRnk)
combined_df_PgRnk <- combined_df_PgRnk[order(combined_df_PgRnk$Date, -combined_df_PgRnk$PageRank), ]
View(combined_df_PgRnk)
rm(pgrnk1,pgrnk2,pgrnk3,pgrnk4,pgrnk5)

# Question 4
# First Part

# Apply fast greedy clustering

com_fg1 = cluster_fast_greedy(as.undirected(g1))
com_fg2 = cluster_fast_greedy(as.undirected(g2))
com_fg3 = cluster_fast_greedy(as.undirected(g3))
com_fg4 = cluster_fast_greedy(as.undirected(g4))
com_fg5 = cluster_fast_greedy(as.undirected(g5))

# Apply Infomap clustering

# com_infmap1 = cluster_infomap(as.undirected(g1))
# com_infmap2 = cluster_infomap(as.undirected(g2))
# com_infmap3 = cluster_infomap(as.undirected(g3))
# com_infmap4 = cluster_infomap(as.undirected(g4))
# com_infmap5 = cluster_infomap(as.undirected(g5))
#The above code practically doesn't run!!

# Apply Louvain clustering

com_lvcl1 = cluster_louvain(as.undirected(g1))
com_lvcl2 = cluster_louvain(as.undirected(g2))
com_lvcl3 = cluster_louvain(as.undirected(g3))
com_lvcl4 = cluster_louvain(as.undirected(g4))
com_lvcl5 = cluster_louvain(as.undirected(g5))
louvain_communities = list(com_lvcl1, com_lvcl2, com_lvcl3, com_lvcl4, com_lvcl5)

# Compare fast greedy communities with louvain clustering

compare(com_fg1, com_lvcl1)
compare(com_fg2, com_lvcl2)
compare(com_fg3, com_lvcl3)
compare(com_fg4, com_lvcl4)
compare(com_fg5, com_lvcl5)

# Second Part

# Finding the random user

alldg<-c(degree(g1,mode='total',loops=FALSE),degree(g2,mode='total',loops=FALSE),degree(g3,mode='total',loops=FALSE),degree(g4,mode='total',loops=FALSE),degree(g5,mode='total',loops=FALSE))
common_user <- table(names(alldg))
common_user<- common_user[common_user == 5]
set.seed(42)
random_user <- names(sample(common_user, 1))

# Get the community that the random user belongs to in each graph

com1<-membership(com_lvcl1)[random_user]
com2<-membership(com_lvcl2)[random_user]
com3<-membership(com_lvcl3)[random_user]
com4<-membership(com_lvcl4)[random_user]
com5<-membership(com_lvcl5)[random_user]

# Creating functions for measures

closeness <- function(graph, community) {
  subgraph <- induced_subgraph(graph, which(membership(community) == 1))
  sapply(V(subgraph), function(v) igraph::closeness(subgraph)[[v]])
}

betweenness <- function(graph, community) {
  subgraph <- induced_subgraph(graph, which(membership(community) == 1))
  igraph::betweenness(graph = subgraph, v = V(subgraph))
}

in_degree <- function(graph, community) {
  subgraph <- induced_subgraph(graph, which(membership(community) == 1))
  igraph::degree(subgraph, mode = "in")
}

out_degree <- function(graph, community) {
  subgraph <- induced_subgraph(graph, which(membership(community) == 1))
  igraph::degree(subgraph, mode = "out")
}

# Calculate average measures for each community

closeness1 <- mean(closeness(as.undirected(g1), com_lvcl1))
betweenness1 <- mean(betweenness(as.undirected(g1), com_lvcl1))
in_degree1 <- mean(in_degree(as.undirected(g1), com_lvcl1))
out_degree1 <- mean(out_degree(as.undirected(g1), com_lvcl1))
modularity1 <- mean(modularity(as.undirected(g1), membership(com_lvcl1)))

closeness2 <- mean(closeness(as.undirected(g2), com_lvcl2))
betweenness2 <- mean(betweenness(as.undirected(g2), com_lvcl2))
in_degree2 <- mean(in_degree(as.undirected(g2), com_lvcl2))
out_degree2 <- mean(out_degree(as.undirected(g2), com_lvcl2))
modularity2 <- mean(modularity(as.undirected(g2), membership(com_lvcl2)))

closeness3 <- mean(closeness(as.undirected(g3), com_lvcl3))
betweenness3 <- mean(betweenness(as.undirected(g3), com_lvcl3))
in_degree3 <- mean(in_degree(as.undirected(g3), com_lvcl3))
out_degree3 <- mean(out_degree(as.undirected(g3), com_lvcl3))
modularity3 <- mean(modularity(as.undirected(g3), membership(com_lvcl3)))

closeness4 <- mean(closeness(as.undirected(g4), com_lvcl4))
betweenness4 <- mean(betweenness(as.undirected(g4), com_lvcl4))
in_degree4 <- mean(in_degree(as.undirected(g4), com_lvcl4))
out_degree4 <- mean(out_degree(as.undirected(g4), com_lvcl4))
modularity4 <- mean(modularity(as.undirected(g4), membership(com_lvcl4)))

closeness5 <- mean(closeness(as.undirected(g5), com_lvcl5))
betweenness5 <- mean(betweenness(as.undirected(g5), com_lvcl5))
in_degree5 <- mean(in_degree(as.undirected(g5), com_lvcl5))
out_degree5 <- mean(out_degree(as.undirected(g5), com_lvcl5))
modularity5 <- mean(modularity(as.undirected(g5), membership(com_lvcl5)))

# Print all metrics values for each community:

metrics <- c("closeness", "betweenness", "in_degree", "out_degree", "modularity")

for (metric in metrics) {
  cat(metric, ":\n")
  for (i in 1:5) {
    metric_value <- get(paste0(tolower(metric), i))
    cat("Community", i, ":", metric_value, "\n")
  }
  cat("\n")
}

# Find Top 5 Topics for each of the users community:

# Define a function first:

calculate_topic_importance <- function(community_topics) {
  topic_counts <- table(community_topics, exclude = c("", NA, NULL, "null/na"))
  topic_importance <- prop.table(topic_counts)
  return(topic_importance)
}

com_topics1 <- V(as.undirected(g1))$topic_of_interest[membership(com_lvcl1) == membership(com_lvcl1)[random_user]]
importance_topic_com1 <- calculate_topic_importance(com_topics1)

com_topics2 <- V(as.undirected(g2))$topic_of_interest[membership(com_lvcl2) == membership(com_lvcl2)[random_user]]
importance_topic_com2 <- calculate_topic_importance(com_topics2)

com_topics3 <- V(as.undirected(g3))$topic_of_interest[membership(com_lvcl3) == membership(com_lvcl3)[random_user]]
importance_topic_com3 <- calculate_topic_importance(com_topics3)

com_topics4 <- V(as.undirected(g4))$topic_of_interest[membership(com_lvcl4) == membership(com_lvcl4)[random_user]]
importance_topic_com4 <- calculate_topic_importance(com_topics4)

com_topics5 <- V(as.undirected(g5))$topic_of_interest[membership(com_lvcl5) == membership(com_lvcl5)[random_user]]
importance_topic_com5 <- calculate_topic_importance(com_topics5)

# Sort the topics by importance
sorted1 <- sort(importance_topic_com1, decreasing = TRUE)
sorted2 <- sort(importance_topic_com2, decreasing = TRUE)
sorted3 <- sort(importance_topic_com3, decreasing = TRUE)
sorted4 <- sort(importance_topic_com4, decreasing = TRUE)
sorted5 <- sort(importance_topic_com5, decreasing = TRUE)

for (i in 1:5) {
  sorted <- get(paste0("sorted", i))
  metric_value <- names(sorted[1:5])
  cat("Top 5 Community Topics for Community", i, "are:", metric_value, "\n")
}

# Extract the topics from the sorted topic importance lists
topics1 <- names(sorted1)
topics2 <- names(sorted2)
topics3 <- names(sorted3)
topics4 <- names(sorted4)
topics5 <- names(sorted4)

# Find the common topics between pairs of lists
common_topics_1_2 <- intersect(topics1, topics2)
common_topics_1_3 <- intersect(topics1, topics3)
common_topics_1_4 <- intersect(topics1, topics4)
common_topics_1_5 <- intersect(topics1, topics5)
common_topics_2_3 <- intersect(topics2, topics3)
common_topics_2_4 <- intersect(topics2, topics4)
common_topics_2_5 <- intersect(topics2, topics5)
common_topics_3_4 <- intersect(topics3, topics4)
common_topics_3_5 <- intersect(topics3, topics5)
common_topics_4_5 <- intersect(topics4, topics5)

# Combine all the common topics from pairs of lists
common_topics <- c(
  common_topics_1_2,
  common_topics_1_3,
  common_topics_1_4,
  common_topics_1_5,
  common_topics_2_3,
  common_topics_2_4,
  common_topics_2_5,
  common_topics_3_4,
  common_topics_3_5,
  common_topics_4_5
)

# Find the topics that are common between at least two lists
common_between_two_lists <- unique(common_topics[duplicated(common_topics)])

# Print the topics that are common between at least two lists
print(common_between_two_lists[1:5])

# Visualization of Communities
graphList<-list(as.undirected(g1),as.undirected(g2),as.undirected(g3),as.undirected(g4),as.undirected(g5))

# Visualization of the 5 graphs using a different color for each community
for (i in 1:5) {
  n <- graphList[[i]]
  m <- louvain_communities[[i]]
  V(n)$color <- factor(membership(m))
  is_crossing <- crossing(n, communities = m)
  community_size <- sizes(m)
  in_mid_community <- unlist(m[community_size > 25 & community_size < 100]) #Set some limits
  sub <- induced.subgraph(n, in_mid_community)
  plot(sub, vertex.label = NA, edge.arrow.width = 0.5, edge.arrow.size = 0.2,
       coords = layout_with_fr(sub), margin = 0, vertex.size = 4, sub = paste("Communities - Day: ", date[i], sep = ""))  
}