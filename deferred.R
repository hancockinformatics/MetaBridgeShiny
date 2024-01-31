# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(pathview)
  library(rlang)
  library(DT)
  library(stringr)
  library(magrittr)
})


# Load example data -------------------------------------------------------

load("data/example_data.RData")


# Load data ----------------------------------------------------------

# Last update was in January/February 2023
load("data/metaCyc_data_v26.RData", verbose = TRUE)
load("data/kegg_data_r105.RData", verbose = TRUE)

# Keep only enzyme-gene relationships
k03_keggGenes <- k03_keggGenes %>%
  select(-KEGG) %>%
  unique()


# Load functions ----------------------------------------------------------

purrr::walk(
  .x = list.files("functions/", full.names = TRUE),
  .f = ~source(.x)
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
