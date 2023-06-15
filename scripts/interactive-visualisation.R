# Load required libraries
library(visNetwork)
library(igraph)

saveWidget(visIgraph(graph_artists), file = "test.html")
