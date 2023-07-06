


write(jsonlite::toJSON(igraph_to_networkD3(graph_artists)),
      file = here::here("data", "graph.json"))
