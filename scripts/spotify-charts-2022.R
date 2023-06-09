# Load packages -----------------------------------------------------------
library(tidyverse)
library(rio)
library(tidyr)
library(igraph)
library(here)


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
# first plotting
plot(graph_artists,
     vertex.label = NA,
     vertex.size = 2)




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
