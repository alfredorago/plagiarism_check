## Check for similarity between student assignments

require(tm)
require(here)
require(magrittr)
library(dplyr)

# Load student .rmd assignments into a corpora object (a list of documents)
# The path is relative to the project directory

corpora <- 
  here::here("./rmds/") %>%
  DirSource(pattern = ".*\\.[R,r]md") %>% 
  Corpus()

# Filter out punctuation, numbers, whitespace and common words

corpora_filtered <- 
  corpora %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords, stopwords("english"))

# Check frequency of words in each document

corpora_term_matrix <-
  TermDocumentMatrix(corpora_filtered)

inspect(corpora_term_matrix)

# Create matrix of word occurrencies in each document
# NOTE: I use the dataset with punctuation, number and whitespaces since those are plagiarism tells
# I think it is better to run with and without it so we can check for different kinds of plagiarism

corpora_document_matrix <- 
  corpora %>% 
  DocumentTermMatrix(
    control = list(weighting = weightTfIdf)
  ) %>% 
  as.matrix() %>% 
  # Set readable file names for the dendrogram
  # The exact pattern will change based on the assignment naming convention
  set_rownames(
    stringr::str_extract(
      string = rownames(.), 
      pattern = "^[^_]*")
  ) %>% 
  scale(center = T, scale = T)

# Cluster according to word usage differences
# complete clustering works better usually
# Probably bc minimizing distances within clusters is more important than maximizing separation

corpora_hclust <-
  corpora_document_matrix %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "complete")

plot(corpora_hclust)

## Plot k nearest neighbours graph to highlight cliques
# Values of k between 2 and 3 work best

require(dbscan)
require(igraph)

nearest_neighbours <-
  corpora_document_matrix %>%
  .[,-which(apply(., 2, function(x){any(is.na(x))}))] %>% 
  dist() %>% 
  dbscan::kNN(k = 1)

dbscan::adjacencylist(nearest_neighbours) %>% 
  igraph::graph_from_adj_list() %>% 
  igraph::add_layout_(igraph::nicely()) %>% 
  plot()
