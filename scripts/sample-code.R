library(tidyverse)
# Example data frame
df <- data.frame(
  Song = c("Song1", "Song2", "Song3"),
  Artists = c("Artist1, Artist2", "Artist2, Artist3", "Artist1, Artist3")
)

df_split <- df %>%
  separate_rows(Artists, sep = ", ")

affiliation_matrix <- df_split %>%
  table() %>%
  crossprod()
