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


# Pre-processing ----------------------------------------------------------
# remove redundant variables
df <- global
df <- df %>%
  select(c(title, artist, streams)) %>% 
  separate_rows(artist, sep = ", ")
# count chart appearances and total number of streams per artist
success1 <- data.frame(table(df$artist))
success2 <- aggregate(streams ~ artist, df, sum)
# create incidence matrix
incidence_matrix <- xtabs(~ artist + title, data = df) > 0
incidence_matrix <- as.data.frame(incidence_matrix)
incidence_matrix <- apply(incidence_matrix, c(1, 2),
                          function(x) ifelse(x == FALSE, 0, ifelse(x == TRUE, 1, x)))
# create adjacency matrix
adjacency_matrix <- incidence_matrix %*% t(incidence_matrix)
adjacency_matrix <- ifelse(adjacency_matrix > 0, 1, 0)


# graph creation ----------------------------------------------------------
#graph_artists <- graph_from_edgelist(edge_list, directed = F)
graph_artists <- graph_from_adjacency_matrix(adjacency_matrix, mode = "undirected")
graph_artists <- simplify(graph_artists, remove.loops = TRUE)
# add number of streams as vertex attribute
order <- as.data.frame(names(as.list(V(graph_artists))))
colnames(order)[1] <- "artist"
streams <- inner_join(order, success2, by = "artist")
streams <- streams$streams
V(graph_artists)$streams <- streams

# add genre as vertex attribute
#write.csv(order, here::here("data", "artist_names.csv"), row.names = FALSE)
# run Python script to access Spotify API and collect artists genres
genres <- import(here::here("data", "artist_genre.csv"))
# recode to create some more aggregated categories
genres$genre2 <- genres$Genre
genres$genre2 <- "other"
genres$genre2 <- ifelse(grepl("pop", genres$Genre, ignore.case = TRUE),
                        "pop", genres$genre2)
genres$genre2 <- ifelse(grepl("hip hop", genres$Genre, ignore.case = TRUE),
                        "hip hop", genres$genre2)
genres$genre2 <- ifelse(grepl("trap", genres$Genre, ignore.case = TRUE),
                        "trap", genres$genre2)
genres$genre2 <- ifelse(grepl("rap", genres$Genre, ignore.case = TRUE),
                        "rap", genres$genre2)
genres$genre2 <- ifelse(grepl("house", genres$Genre, ignore.case = TRUE),
                        "house", genres$genre2)
genres$genre2 <- ifelse(grepl("rock", genres$Genre, ignore.case = TRUE),
                        "rock", genres$genre2)
genres$genre2 <- ifelse(grepl("Unknown", genres$Genre, ignore.case = TRUE),
                        "Unknown", genres$genre2)
# add genres to graph object
V(graph_artists)$genre_untouched <- genres$Genre
V(graph_artists)$genre_recoded <- genres$genre2

#to test some relations# test <- as_edgelist(graph_artists)


# network analysis --------------------------------------------------------

# metrics -----------------------------------------------------------------
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


# plotting ----------------------------------------------------------------
# plot with vertex attributes degree and closeness
# create a color palette with transparency values based on degree
#cols <- colorRampPalette(c("seagreen1", "seagreen4"))(100)
vertex_pal <- c("#ABA9BF", "#C1666B", "#4DAA57", "#D5A021", "#094D92", "#FF57BB", "#439775")
alpha_vals <- seq(0.2, 1, length.out = 7)
alpha_palette <- cbind(vertex_pal, alpha_vals)
# plot
pdf("imgs/graph_artists.pdf")  # Specify the file name and path for the PDF file
plot(graph_artists,
          vertex.label = NA,
          vertex.size = (V(graph_artists)$streams/sum(V(graph_artists)$streams))*75, #log(V(graph_artists)$degree, 5),
          vertex.color = vertex_pal[as.factor(V(graph_artists)$genre_recoded)], #alpha_palette[findInterval(V(graph_artists)$degree*85, seq(0, 100, length.out = 101)),],
          #edge.color = "black",
          layout = layout_with_fr(graph_artists, niter = 20000))
legend("topright", legend = unique(V(graph_artists)$genre_recoded),
       col = vertex_pal, pch = 16, pt.cex = 1.5, text.font = 6)
dev.off()

# plot only biggest component
components <- clusters(graph_artists)
largest_component <- which.max(components$csize)
graph_component <- induced_subgraph(graph_artists,
                                    which(components$membership == largest_component))
plot(graph_component,
     vertex.label = NA, #V(graph_component)$name,
     vertex.size = (V(graph_component)$streams/sum(V(graph_component)$streams))*100, #log(V(graph_artists)$degree, 5),
     vertex.color = "springgreen", #alpha_palette[findInterval(V(graph_artists)$closeness*85, seq(0, 100, length.out = 101)),],
     edge.color = "black",
     layout = layout_with_fr(graph_artists, niter = 20000))

# plot only top-XX
sorted_vertices <- sort(V(graph_artists)$streams, decreasing = TRUE)
top_vertices <- head(sorted_vertices, 50)
subgraph <- induced_subgraph(graph_artists, which(V(graph_artists)$streams %in% top_vertices))

vertex_pal <- c("#094D92", "#D5A021", "#4DAA57", "#C1666B", "#ABA9BF")

plot(subgraph,
     vertex.label = V(subgraph)$name,
     vertex.label.cex = 1, # label font size
     vertex.label.color = "black",
     vertex.label.family = "serif",
     vertex.label.font = 1, # bold  font
     vertex.size = sqrt((V(subgraph)$streams/sum(V(subgraph)$streams)))*50, #log(V(graph_artists)$degree, 5),
     vertex.color = vertex_pal[as.factor(V(subgraph)$genre_recoded)],#'red',#alpha_palette[findInterval(V(subgraph)$closeness*85, seq(0, 100, length.out = 101)),],
     edge.color = "black",
     layout = layout_with_fr(subgraph, niter = 20000))
legend("topright", legend = unique(V(subgraph)$genre_recoded),
       col = vertex_pal, pch = 16, pt.cex = 2.5)

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

