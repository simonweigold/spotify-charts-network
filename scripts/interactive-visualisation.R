# Run preparation script before proceeding if not done so

saveWidget(visIgraph(graph_artists), file = "test.html")

# visualisation of main component
#saveWidget(visIgraph(largest_subgraph), file = "main_component.html")


# networkD3 ---------------------------------------------------------------


# Assuming you have the largest subgraph stored in 'largest_subgraph' and the desired vertex attributes 'genre_recoded' and 'streams'

# Extract the vertex attributes
genre_recoded_ls <- V(largest_subgraph)$genre_recoded
streams_ls <- V(largest_subgraph)$streams

# Create a color palette based on the unique values in 'genre_recoded'
color_palette <- rainbow(length(unique(genre_recoded_ls)))

# Assign colors based on 'genre_recoded'
node_colors <- color_palette[as.factor(genre_recoded_ls)]

# Create a data frame with node attributes
node_data <- data.frame(id = V(largest_subgraph)$name,
                        genre_recoded_ls = genre_recoded_ls,
                        streams_ls = streams_ls,
                        node_colors = node_colors)

# Plot the graph with specific coloring and node sizes
graph_vis <- visIgraph(largest_subgraph) %>%
  visNodes(id = as.list(V(largest_subgraph)$name),
           color = as.list(color_palette[as.factor(V(largest_subgraph)$genre_recoded)]),
           size = as.list(sqrt((V(largest_subgraph)$streams/sum(V(largest_subgraph)$streams)))*50))

# Save the graph as an interactive HTML widget
saveWidget(graph_vis, file = "main_component.html")



# visNetwork --------------------------------------------------------------



# Assuming you have the largest subgraph stored in 'largest_subgraph'
# Define the color palette for different genres
color_palette <- c("rock" = "blue", "pop" = "red", "hip hop" = "green", "rap" = "yellow",
                   "Unknown" = "purple", "other" = "black", "house" = "pink")

# Get the genre and stream attributes from the vertex attributes
genres_ls <- V(largest_subgraph)$genre_recoded
streams_ls <- V(largest_subgraph)$streams

# Create a visNetwork object
vis_net <- visIgraph(largest_subgraph)

# Set node color based on genre attribute
vis_net$nodes$color <- color_palette[genres_ls]

# Set node size based on stream attribute
vis_net$nodes$size <- streams_ls

# Save the resulting visualization as an HTML file
visSave(vis_net, file = "main_component.html")
