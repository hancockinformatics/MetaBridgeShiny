# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pathview)
  library(rlang)
  library(magrittr)
})


# Load data ----------------------------------------------------------

load("example_data/example_data.RData")

# Last update was in February 2024
load("data/metaCyc_data_v27.RData")
load("data/kegg_data_r109.RData")

# Keep only enzyme-gene relationships
k03_keggGenes <- k03_keggGenes %>%
  select(-KEGG) %>%
  unique()
