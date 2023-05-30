library(tidyverse)
library(igraph)

# Example data frame
df <- data.frame(
  Song = c("Song1", "Song2", "Song3"),
  Artists = c("Artist1, Artist2", "Artist2, Artist3", "Artist1, Artist3")
)

df_split <- df %>%
  separate_rows(Artists, sep = ", ")

graph <- make_empty_graph(directed = FALSE)

all_vertices <- unique(c(df$Song, df$Artists))
graph <- add_vertices(graph, vcount = length(all_vertices))
V(graph)$name <- all_vertices

for (i in 1:nrow(df)) {
  event_vertex <- which(V(graph)$name == df$Song[i])
  participant_vertex <- which(V(graph)$name == df$Artists[i])
  graph <- add_edges(graph, c(event_vertex, participant_vertex))
}


