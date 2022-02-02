
# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pathview)
  library(rlang)
  library(DT)
  library(stringr)
  library(magrittr)
  library(dplyr)
})


# Load example data -------------------------------------------------------

load("data/example_data.RData")


# Load data ----------------------------------------------------------

# KEGG data, updated data as of January 31st, 2022
load("data/kegg_data_r101.RData", verbose = TRUE)

# Keep only enzyme-gene relationships
k03_keggGenes <- k03_keggGenes %>%
  select(-KEGG) %>%
  unique()

# MetaCyc data, updated data as of January 31st, 2022
load("data/metaCyc_data_v25.RData", verbose = TRUE)


# Load functions ----------------------------------------------------------

purrr::walk(
  .x = list.files("functions/", full.names = TRUE),
  .f = ~import::from(.x, .all = TRUE, .character_only = TRUE)
)


# Set global DT options ---------------------------------------------------

options(
  DT.options = list(
    scrollX = "100%",
    scrollY = "30vh",
    scrollCollapse = TRUE,
    paging  = FALSE,
    dom     = "tir"
  )
)
