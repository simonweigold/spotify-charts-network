library(here)
source(here::here("scripts", "preparation.R"))


write.graph(graph_artists, file = "data/full-network.graphml", format = "graphml")
write.graph(largest_subgraph, file = "data/largest-subgraph.graphml", format = "graphml")


name <- str_c("data/", V(subgraphs[[1]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[1]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[2]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[2]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[3]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[3]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[4]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[4]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[5]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[5]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[6]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[6]], file = name, format = "graphml")

name <- str_c("data/", V(subgraphs[[7]])$genre_recoded[1], "-subgraph.graphml")
write.graph(subgraphs[[7]], file = name, format = "graphml")
