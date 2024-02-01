# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pathview)
  library(rlang)
  library(magrittr)
})


# Load data ----------------------------------------------------------

load("example_data/example_data.RData")

# Last update was in January/February 2023
load("data/metaCyc_data_v26.RData")
load("data/kegg_data_r105.RData")

# Keep only enzyme-gene relationships
k03_keggGenes <- k03_keggGenes %>%
  select(-KEGG) %>%
  unique()


# Load functions ----------------------------------------------------------

purrr::walk(
  .x = list.files("functions/", full.names = TRUE),
  .f = ~source(.x)
)
