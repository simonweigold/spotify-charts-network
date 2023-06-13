# Load packages -----------------------------------------------------------
library(tidyverse)
library(rio)
library(tidyr)
library(igraph)
library(here)
library(ggraph)


# Data import -------------------------------------------------------------
spotify <- import(here::here("data", "charts.csv"))
#global charts
global <- spotify %>% filter(region == "Global")
global <- global %>% filter(trend == 'NEW_ENTRY') %>% filter(chart == 'top200')
global = global[order(global[,'title'],-global[,'streams']),]
global = global[!duplicated(global$title),]
rm(spotify)
# usa <- spotify %>% filter(region == "United States")
# usa = usa[order(usa[,'title'],-usa[,'streams']),]
# usa = usa[!duplicated(usa$title),]
# germany <- spotify %>% filter(region == "Germany")
# germany = germany[order(germany[,'title'],-germany[,'streams']),]
# germany = germany[!duplicated(germany$title),]


# Pre-processing ----------------------------------------------------------
# remove redundant variables
df <- global
df <- df %>%
  select(c(title, artist, streams)) %>% 
  separate_rows(artist, sep = ", ")
# count chart appearances and total number of streams per artist
success1 <- data.frame(table(df$artist))
success2 <- aggregate(streams ~ artist, df, sum)
# Rename the sum_streams column
colnames(sum_streams)[2] <- "total_streams"
success = success[order(-success[,'Freq']), ]
# create incidence matrix
incidence_matrix <- xtabs(~ artist + title, data = df) > 0
incidence_matrix <- as.data.frame(incidence_matrix)
incidence_matrix <- apply(incidence_matrix, c(1, 2),
                          function(x) ifelse(x == FALSE, 0, ifelse(x == TRUE, 1, x)))
# create adjacency matrix
adjacency_matrix <- incidence_matrix %*% t(incidence_matrix)
adjacency_matrix <- ifelse(adjacency_matrix > 0, 1, 0)
# create edge list
edges <- which(adjacency_matrix == 1, arr.ind = TRUE)
artist_names <- names(as.data.frame(adjacency_matrix))
source_names <- artist_names[edges[, "row"]]
target_names <- artist_names[edges[, "col"]]
edge_list <- cbind(source_names, target_names)
# create graph object
graph_artists <- graph_from_edgelist(edge_list, directed = F)
graph_artists <- simplify(graph_artists, remove.loops = TRUE)
# filter out nodes with 0 connections
graph_artists_filtered <- delete_vertices(graph_artists,
                                          which(degree(graph_artists) == 0))

# exploratory data analysis -----------------------------------------------
# first plotting
plot(graph_artists_filtered,
     vertex.label = NA,
     vertex.size = 2)


# network analysis --------------------------------------------------------
gsize(graph_artists) #size
edge_density(graph_artists) #density
count_components(graph_artists) #components
diameter(graph_artists, directed = F) #diameter
transitivity(graph_artists) #clustering
# Degree
degree <- degree(graph_artists)
degree(graph_artists)[order(-degree(graph_artists))]
# Closeness
closeness <- closeness(graph_artists)
closeness(graph_artists)[order(-closeness(graph_artists))]
# Betweenness
betweenness <- betweenness(graph_artists)
betweenness(graph_artists)[order(-betweenness(graph_artists))]

# attach calculations to network object
V(graph_artists)$degree <- degree # Add degree as attribute
V(graph_artists)$betweenness <- betweenness # Add degree as attribute
V(graph_artists)$closeness <- closeness # Add degree as attribute

# plot with vertex attributes degree and closeness
# create a color palette with transparency values based on degree
cols <- colorRampPalette(c("seagreen1", "seagreen4"))(100)
alpha_vals <- seq(0.2, 1, length.out = 100)
alpha_palette <- cbind(cols, alpha_vals)
# plot
pdf("imgs/graph_artists.pdf")  # Specify the file name and path for the PDF file
plot(graph_artists,
          vertex.label = NA,
          vertex.size = log(V(graph_artists)$degree, 5),
          vertex.color = "springgreen", #alpha_palette[findInterval(V(graph_artists)$closeness*85, seq(0, 100, length.out = 101)),],
          edge.color = "black",
          layout = layout_with_fr(graph_artists, niter = 20000))
dev.off()

# plot with modularity subgroups
# detect communities using fast greedy algorithm
communities <- fastgreedy.community(graph_artists)
# calculate the modularity
modularity(graph_artists, membership = as.vector(communities$membership))
# Plot the communities
plot(communities, graph_artists,
     vertex.label = NA,
     vertex.size = log10(V(graph_artists)$degree),
     vertex.color = "seagreen3", #alpha_palette[findInterval(V(graph_artists)$closeness*85, seq(0, 100, length.out = 101)),],
     edge.color = "black",
     layout = layout_with_fr(graph_artists, niter = 20000))






# danger zone -------------------------------------------------------------
# from here on its getting dangerous
graph <- graph_from_data_frame(df %>% select(c(title, artist)),
                               directed = F, vertices = NULL)
artists_graph <- graph.incidence(incidence_matrix)
# create graph object
# create empty graph object
graph <- make_empty_graph(directed = FALSE)
# add songs and artists as vertices to graph
all_vertices <- unique(c(df$artist, df$title))
graph <- add_vertices(graph, nv = length(all_vertices))
V(graph)$name <- all_vertices
# add edges between songs and artists
for (i in 1:nrow(df)) {
  title_vertex <- which(V(graph)$name == df$title[i])
  artist_vertex <- which(V(graph)$name == df$artist[i])
  graph <- add_edges(graph, c(artist_vertex, title_vertex))
}



# create an adjacency matrix


# create an affiliation matrix
affiliation_matrix <- as_adjacency_matrix(graph, sparse = FALSE)

# create an edge list
affiliation_graph <- graph_from_adjacency_matrix(affiliation_matrix, mode = "undirected")
edge_list <- get.edgelist(affiliation_graph, names = TRUE)


# Residual code -----------------------------------------------------------

# Pre-processing ----------------------------------------------------------
df <- Spotify %>% select(artist_names)
df_split <- separate(df, col = artist_names, into = paste0("text", 1:7), sep = ",", extra = "merge")
df_split <- df_split %>% filter(!is.na(df_split$text2))
df_test <- df_split %>% select(text1, text2)
for (col in names(df_split)) {
  if (is.character(df_split[[col]])) {
    df_split[[col]] <- trimws(df_split[[col]])
  }
}
for (col in names(df_test)) {
  if (is.character(df_test[[col]])) {
    df_test[[col]] <- trimws(df_test[[col]])
  }
}
network <- graph_from_data_frame(df_test, directed = F)


# Visualisation of top nodes ----------------------------------------------
# get the degree of each node
degree <- degree(network)
V(network)$degree <- degree
# sort the degrees in descending order and select the top n indices
n <- 100
top_nodes <- order(degree, decreasing = TRUE)[1:n]
# extract the subgraph consisting of the selected nodes
subgraph <- induced_subgraph(network, top_nodes)
# plot the subgraph
plot(subgraph,
     vertex.shape = "circle",
     vertex.size = V(subgraph)$degree,
     edge.arrow.size = 0.15,
     vertex.label.cex = 0.8, # label font size
     vertex.color = "seagreen1",
     edge.color = "black",
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     vertex.label.font = 2, # bold  font
     )


simple_subgraph <- simplify(subgraph)
sg_degree <- degree(simple_subgraph)
V(simple_subgraph)$degree <- sg_degree
communities <- fastgreedy.community(simple_subgraph)
# calculate the modularity
modularity(simple_subgraph, membership = as.vector(communities$membership))
# Plot the communities
plot(communities, simple_subgraph,
     vertex.shape = "circle",
     vertex.size = V(simple_subgraph)$degree,
     edge.arrow.size = 0.15,
     vertex.label.cex = 0.8, # label font size
     vertex.label.color = "black",
     vertex.label.family = "Helvetica",
     vertex.label.font = 2, # bold  font
)
