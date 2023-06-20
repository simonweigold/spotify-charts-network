# Load packages -----------------------------------------------------------
library(tidyverse)
library(rio)
library(tidyr)
library(igraph)
library(here)
library(ggraph)
library(visNetwork)
library(htmlwidgets)
library(corrplot)
library(stargazer)


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

# add metrics
degree <- degree(graph_artists) # degree centrality
V(graph_artists)$degree <- degree # Add degree as attribute
betweenness <- betweenness(graph_artists) # betweenness centrality
V(graph_artists)$betweenness <- betweenness # Add degree as attribute
closeness <- closeness(graph_artists) # closeness centrality
V(graph_artists)$closeness <- closeness # Add closeness as attribute
eigenvector <- eigen_centrality(graph_artists)$vector # eigenvector centrality
V(graph_artists)$eigenvector <- eigenvector # Add eigenvector as attribute

# create metrics df to store metrics
degree_df <- as.data.frame(degree)
degree_df$artist <- rownames(degree_df)
betweenness_df <- as.data.frame(betweenness)
betweenness_df$artist <- rownames(betweenness_df)
closeness_df <- as.data.frame(closeness)
closeness_df$artist <- rownames(closeness_df)
eigenvector_df <- as.data.frame(eigenvector)
eigenvector_df$artist <- rownames(eigenvector_df)
metrics <- inner_join(success2, degree_df, by = "artist")
metrics <- inner_join(metrics, betweenness_df, by = "artist")
metrics <- inner_join(metrics, closeness_df, by = "artist")
metrics <- inner_join(metrics, eigenvector_df, by = "artist")
colnames(genres)[1] <- "artist"
metrics <- inner_join(metrics, genres, by = "artist")

# avgs per genre
metrics$genre3 <- metrics$genre2
metrics$genre3 <- ifelse(grepl("rap", metrics$genre2, ignore.case = TRUE),
                         "hip hop", metrics$genre3)
avgs <- metrics %>% 
  group_by(genre3) %>% 
  summarise(avg_streams = mean(streams),
            avg_degree = mean(degree),
            avg_betweenness = mean(betweenness),
            avg_closeness = mean(closeness, na.rm = T),
            avg_eigenvector = mean(eigenvector))
