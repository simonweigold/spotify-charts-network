# Initialisation ----------------------------------------------------------
library(here)
source(here::here("scripts", "preparation.R"))


# network analysis --------------------------------------------------------

# metrics -----------------------------------------------------------------
gsize(graph_artists) #size
edge_density(graph_artists) #density
count_components(graph_artists) #components
diameter(graph_artists, directed = F) #diameter
transitivity(graph_artists) #clustering
# Degree
degree(graph_artists)[order(-degree(graph_artists))] %>% head(25)
degree(largest_subgraph)[order(-degree(largest_subgraph))] %>% head(25)
# Closeness
closeness(graph_artists)[order(-closeness(graph_artists))] %>% head(25)
closeness(largest_subgraph)[order(-closeness(largest_subgraph))] %>% head(25)
# Betweenness
betweenness(graph_artists)[order(-betweenness(graph_artists))] %>% head(25)
betweenness(largest_subgraph)[order(-betweenness(largest_subgraph))] %>% head(25)
# Eigenvector
eigen_centrality(graph_artists)$vector[order(-eigen_centrality(graph_artists)$vector)] %>% head(25)
eigen_centrality(largest_subgraph)$vector[order(-eigen_centrality(largest_subgraph)$vector)] %>% head(25)

# correlation
metrics %>%
  select(c(streams, degree, betweenness, closeness, eigenvector)) %>% 
  cor(use = "na.or.complete") %>% 
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# multiple linear regression for h1
fit <- lm(streams ~ degree + betweenness + closeness + eigenvector, data = metrics)
stargazer(fit, type = "text")

# anova
anova <- aov(streams ~ genre3 + degree + betweenness + closeness + eigenvector, data = metrics)
summary(anova)


# plotting ----------------------------------------------------------------
# plot with vertex attributes degree and closeness
# create a color palette with transparency values based on degree
#cols <- colorRampPalette(c("seagreen1", "seagreen4"))(100)
vertex_pal <- c("#ABA9BF", "#C1666B", "#4DAA57", "#D5A021", "#094D92", "#FF57BB", "#439775")
alpha_vals <- seq(0.2, 1, length.out = 7)
alpha_palette <- cbind(vertex_pal, alpha_vals)
# plot
# Set the filename and resolution
png("imgs/graph_artists.png", width = 8, height = 6, units = "in", res = 3000)
#pdf("imgs/graph_artists.pdf")  # Specify the file name and path for the PDF file
plot(largest_subgraph,
     vertex.label = V(largest_subgraph)$name,
     vertex.size = sqrt((V(largest_subgraph)$streams/sum(V(largest_subgraph)$streams)))*50, #(V(largest_subgraph)$streams/sum(V(largest_subgraph)$streams))*100, #log(V(graph_artists)$degree, 5),
     vertex.color = vertex_pal[as.factor(V(largest_subgraph)$genre_recoded)], #alpha_palette[findInterval(V(graph_artists)$degree*85, seq(0, 100, length.out = 101)),],
     #edge.color = "black",
     edge.alpha = 0.75,
     layout = layout_with_fr(largest_subgraph, niter = 20000))
legend("topright", legend = unique(V(largest_subgraph)$genre_recoded),
       col = vertex_pal, pch = 16, pt.cex = 1.5, text.font = 6)
dev.off()
png(filename, width = 8, height = 6, units = "in", res = resolution)
plot(graph_artists,
     vertex.label = NA,
     vertex.size = (V(graph_artists)$streams/sum(V(graph_artists)$streams))*75)
dev.off()
saveWidget(visIgraph(graph_artists), file = "test.html")

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
top_vertices <- head(sorted_vertices, 100)
subgraph <- induced_subgraph(graph_artists, which(V(graph_artists)$streams %in% top_vertices))

vertex_pal <- c("#094D92", "#D5A021", "#4DAA57", "#C1666B", "#ABA9BF")

png("imgs/top-150.png", width = 8, height = 6, units = "in", res = 3000) 
plot(subgraph,
     vertex.label = NA, #V(subgraph)$name,
     vertex.label.cex = 1, # label font size
     vertex.label.color = "black",
     vertex.label.family = "serif",
     vertex.label.font = 1, # bold  font
     vertex.size = sqrt((V(subgraph)$streams/sum(V(subgraph)$streams)))*50, #log(V(graph_artists)$degree, 5),
     vertex.color = vertex_pal[as.factor(V(subgraph)$genre_recoded)],#'red',#alpha_palette[findInterval(V(subgraph)$closeness*85, seq(0, 100, length.out = 101)),],
     edge.color = "black",
     layout = layout_with_fr(subgraph, niter = 20000))
legend("topright", legend = c("hip hop", "other", "pop", "rap", "rock"), #unique(V(subgraph)$genre_recoded),
       col = vertex_pal, pch = 16, pt.cex = 2.5)
dev.off()

saveWidget(visIgraph(subgraph), file = "top-xx.html")

# plot with modularity subgroups
# detect communities using fast greedy algorithm
communities <- fastgreedy.community(subgraph)
# calculate the modularity
modularity(subgraph, membership = as.vector(communities$membership))
# Plot the communities
png("imgs/top-100_modularity.png", width = 8, height = 6, units = "in", res = 3000) 
plot(communities, subgraph,
     vertex.label = NA,
     vertex.size = sqrt((V(subgraph)$streams/sum(V(subgraph)$streams)))*50,
     vertex.color = vertex_pal[as.factor(V(subgraph)$genre_recoded)], #alpha_palette[findInterval(V(graph_artists)$closeness*85, seq(0, 100, length.out = 101)),],
     edge.color = "black",
     layout = layout_with_fr(subgraph, niter = 20000))
dev.off()


# plot genre subgraphs
for (i in 1:length(subgraphs)) {
  current <- subgraphs[[unique_genres[i]]]
  plot(current,
       main = V(current)$genre_recoded[1],
       vertex.label = NA, #V(current)$name,
       vertex.label.cex = 1, # label font size
       vertex.label.color = "black",
       vertex.label.family = "serif",
       vertex.label.font = 1, # bold  font
       vertex.size = sqrt((V(current)$streams/sum(V(current)$streams)))*50, #(V(largest_subgraph)$streams/sum(V(largest_subgraph)$streams))*100, #log(V(graph_artists)$degree, 5),
       vertex.color = "seagreen2",
       edge.color = "black",
       edge.alpha = 0.75,
       layout = layout_with_fr(current, niter = 20000))
}

