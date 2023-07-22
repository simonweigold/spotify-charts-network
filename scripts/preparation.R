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
library(visNetwork)
library(networkD3)
library(plotly)
library(car)
library(Hmisc)
library(lmtest)
library(sandwich)
library(performance)
library(factoextra)
library(generics)
library(glmnet)
library(wordcloud2)
library(jsonlite)
library(webshot)
library(caret)



# Data import -------------------------------------------------------------

spotify <- import(here::here("data", "charts.csv"))
#global charts
global <- spotify %>% filter(region == "Global")
global <- global %>% filter(chart == 'top200') #%>% filter(trend == 'NEW_ENTRY') 
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
colnames(success1)[1] <- "artist"
success2 <- aggregate(streams ~ artist, df, sum)
# combine success variables
dep_var <- inner_join(success1, success2, by = "artist")
dep_var$output <- dep_var$streams/dep_var$Freq
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
order <- as.data.frame(names(as.list(V(graph_artists)))) # order is a df with all the artist names
colnames(order)[1] <- "artist"
streams <- inner_join(order, dep_var %>% select(c(artist, output)), by = "artist")
streams <- streams$output
V(graph_artists)$streams <- streams
# save streams as df
streams_df <- as.data.frame(cbind(order, streams))

# add genre as vertex attribute
#write.csv(order, here::here("data", "artist_names.csv"), row.names = FALSE)
# run Python script to access Spotify API and collect artists genres
genres <- import(here::here("data", "artist_genre.csv"))
colnames(genres)[1] <- "artist"
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
genres$genre3 <- genres$genre2
genres$genre3 <- ifelse(grepl("rap", genres$genre2, ignore.case = TRUE),
                         "hip hop", genres$genre3)
# add genres to graph object
V(graph_artists)$genre_untouched <- genres$Genre
V(graph_artists)$genre_recoded <- genres$genre2

# add popularity as vertex attribute
# run Python script to access Spotify API and collect popularity scores
popularity <- import(here::here("data", "artist_popularity.csv"))
colnames(popularity)[1] <- "artist"
colnames(popularity)[2] <- "popularity"
popularity$popularity[popularity$popularity == "Unknown"] = NA
popularity$popularity = as.numeric(popularity$popularity)
# add popularity to graph object
V(graph_artists)$popularity <- popularity$popularity

# largest component subgraph ----------------------------------------------

# create main component subgraph
comps <- igraph::components(graph_artists)
# Get the component membership vector
membership <- comps$membership
# Find the largest component
largest_comp <- which(membership == which.max(comps$csize))
# Extract the subgraph corresponding to the largest component
largest_subgraph <- induced_subgraph(graph_artists, largest_comp)

# add metrics
V(largest_subgraph)$degree <- degree(largest_subgraph) # degree centrality
V(largest_subgraph)$betweenness <- betweenness(largest_subgraph) # betweenness centrality
V(largest_subgraph)$closeness <- closeness(largest_subgraph) # closeness centrality
V(largest_subgraph)$eigenvector <- eigen_centrality(largest_subgraph)$vector # eigenvector centrality



# components + centrality measures ----------------------------------------

# create components
components <- igraph::decompose.graph(graph_artists)

# List to store separate network objects
result_df <- data.frame(artist = character(), degree = numeric(),
                        betweenness = numeric(), closeness = numeric(),
                        eigenvector = numeric(), stringsAsFactors = FALSE)

# Loop through each component
for (i in seq_along(components)) {
  component <- components[[i]]
  
  # Calculate degree centrality
  degree <- degree(component)
  betweenness <- betweenness(component)
  closeness <- closeness(component)
  eigenvector <- eigen_centrality(component)$vector
  
  # Create a data frame for the component's centrality results
  component_df <- data.frame(artist = V(component)$name, degree = degree,
                             betweenness = betweenness, closeness = closeness,
                             eigenvector = eigenvector)
  
  # Append the component's data frame to the result data frame
  result_df <- rbind(result_df, component_df)
}

# join streams and genres
metrics <- inner_join(result_df, streams_df, by = "artist")
metrics <- inner_join(metrics, genres, by = "artist")
metrics <- inner_join(metrics, popularity, by = "artist")
metrics <- metrics %>% filter(!is.na(popularity))


# Genre subgraphs ---------------------------------------------------------

# Get the unique genre values
unique_genres <- unique(V(graph_artists)$genre_recoded)

# Create a list to store the subgraphs
subgraphs <- list()

# Split the graph by genre and create subgraphs
for (genre in unique_genres) {
  subgraph <- induced_subgraph(graph_artists, V(graph_artists)$genre_recoded == genre)
  subgraphs[[genre]] <- subgraph
}

# get centrality measures of each subgraph + component
# empty gs_result_df to store all metrics
gs_result_df <- data.frame(artist = character(), gs_degree = numeric(),
                           gs_betweenness = numeric(), gs_closeness = numeric(),
                           gs_eigenvector = numeric(), stringsAsFactors = FALSE)
for (i in 1:length(subgraphs)) {
  #current_subgraph <- subgraphs[[i]]
  # create components
  current_component <- igraph::decompose.graph(subgraphs[[i]])

  # Loop through each component
  for (j in seq_along(current_component)) {
    component <- current_component[[j]]
    #print(V(component)$genre_recoded)

    # Calculate degree centrality
    gs_degree <- degree(component)
    gs_betweenness <- betweenness(component)
    gs_closeness <- closeness(component)
    gs_eigenvector <- eigen_centrality(component)$vector

    # Create a data frame for the component's centrality results
    gs_component_df <- data.frame(artist = V(component)$name, gs_degree = gs_degree,
                                  gs_betweenness = gs_betweenness, gs_closeness = gs_closeness,
                                  gs_eigenvector = gs_eigenvector)

    # Append the component's data frame to the result data frame
    gs_result_df <- rbind(gs_result_df, gs_component_df)


  #current <- subgraphs[[unique_genres[i]]]
  #V(current)$name
  #V(current)$genre_recoded[1]
  }
}

# join with previous df

metrics <- inner_join(metrics, gs_result_df, by = "artist")

# collaboration only within genre?
metrics$collaboration <- "multiple genres"
metrics$collaboration[metrics$degree == metrics$gs_degree] = "only same genre"
metrics$collaboration[metrics$degree == 0] = "no collaboration"



# Metrics DFs -------------------------------------------------------------
metrics$degree[metrics$degree == 0] <- 0.0001
metrics$betweenness[metrics$betweenness == 0] <- 0.0001
metrics$eigenvector[metrics$eigenvector == 0] <- 0.0001
metrics$gs_degree[metrics$gs_degree == 0] <- 0.0001
metrics$gs_betweenness[metrics$gs_betweenness == 0] <- 0.0001

# boxcox metrics
#source(here::here("scripts", "boxcox_function.R"))
#metrics_boxcox = metrics
#boxcox_transform(metrics_boxcox, streams)
#lambda <- boxcox(metrics_boxcox$streams ~ 1)$x[which.max(boxcox(metrics_boxcox$streams ~ 1)$y)]
#metrics_boxcox$streams<- ((metrics_boxcox$streams^lambda) - 1)/lambda

#lambda <- boxcox(metrics_stand$streams)
#metrics_boxcox$streams <- ifelse(lambda == 0, log(metrics_stand$streams), ((metrics_stand$streams^lambda) - 1) / lambda)

# log metrics
metrics_log = metrics
metrics_log$streams <- log(metrics$streams)
metrics_log$popularity <- log(metrics$popularity)
metrics_log$degree <- log(metrics$degree)
metrics_log$closeness <- log(metrics$closeness)
metrics_log$betweenness <- log(metrics_log$betweenness)
metrics_log$eigenvector <- log(metrics$eigenvector)
metrics_log$gs_degree <- log(metrics$gs_degree)
metrics_log$gs_closeness <- log(metrics$gs_closeness)
metrics_log$gs_betweenness <- log(metrics_log$gs_betweenness)
metrics_log$gs_eigenvector <- log(metrics$gs_eigenvector)

# standardized metrics
metrics_scale = metrics
metrics_scale$streams <- scale(metrics$streams)
metrics_scale$popularity <- scale(metrics$popularity)
metrics_scale$degree <- scale(metrics$degree)
metrics_scale$closeness <- scale(metrics$closeness)
metrics_scale$betweenness <- scale(metrics_scale$betweenness)
metrics_scale$eigenvector <- scale(metrics$eigenvector)
metrics_scale$gs_degree <- scale(metrics$gs_degree)
metrics_scale$gs_closeness <- scale(metrics$gs_closeness)
metrics_scale$gs_betweenness <- scale(metrics_scale$gs_betweenness)
metrics_scale$gs_eigenvector <- scale(metrics$gs_eigenvector)

# min max metrics
source(here::here("scripts", "minmax_function.R"))
metrics_minmax <- metrics_scale
metrics_minmax$streams <- min_max_normalize(metrics_log$streams)
metrics_minmax$popularity <- min_max_normalize(metrics$popularity)
metrics_minmax$degree <- min_max_normalize(metrics_log$degree)
metrics_minmax$closeness <- min_max_normalize(metrics_log$closeness)
metrics_minmax$betweenness <- min_max_normalize(metrics_log$betweenness)
metrics_minmax$eigenvector <- min_max_normalize(metrics_log$eigenvector)
metrics_minmax$gs_degree <- min_max_normalize(metrics_log$gs_degree)
metrics_minmax$gs_closeness <- min_max_normalize(metrics_log$gs_closeness)
metrics_minmax$gs_betweenness <- min_max_normalize(metrics_log$gs_betweenness)
metrics_minmax$gs_eigenvector <- min_max_normalize(metrics_log$gs_eigenvector)

# sqrt metrics
metrics_sqrt = metrics
metrics_sqrt$streams <- sqrt(metrics$streams)
metrics_sqrt$popularity <- sqrt(metrics$popularity)
metrics_sqrt$degree <- sqrt(metrics$degree)
metrics_sqrt$closeness <- sqrt(metrics$closeness)
metrics_sqrt$betweenness <- sqrt(metrics$betweenness)
metrics_sqrt$eigenvector <- sqrt(metrics$eigenvector)


# avgs per genre
avgs <- metrics %>% 
  group_by(genre3) %>% 
  summarise(avg_streams = mean(streams),
            avg_popularity = mean(popularity, na.rm = T),
            avg_degree = mean(degree),
            avg_betweenness = mean(betweenness),
            avg_closeness = mean(closeness, na.rm = T),
            avg_eigenvector = mean(eigenvector))



# remove unnecessary objects ----------------------------------------------

#rm(adjacency_matrix, betweenness_df, closeness_df, degree_df, df,
   #eigenvector_df, global, incidence_matrix, order, success1, success2,
   #betweenness, closeness, degree, eigenvector, streams)