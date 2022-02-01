
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


# Load KEGG data ----------------------------------------------------------

# Updated data as of January 31st, 2022
load("data/k01_keggCompounds_r101.RData")
load("data/k02_keggEnzymeNames_r101.RData")
load("data/k03_keggGenes_r101.RData")
load("data/k04_keggPathwayNames_r101.RData")
load("data/k05_keggPathwayIDs_r101.RData")


# For the moment, only keep enzyme-gene relationships
k03_keggGenes <- k03_keggGenes %>%
  select(-KEGG) %>%
  unique()


# Load MetaCyc data -------------------------------------------------------

# Updated as of January 31st, 2022
load("data/m01_metaCycDBLinks_v25.RData")
load("data/m02_metaCycCompoundsReactions_v25.RData")
load("data/m03_metaCycReactionsGenes_v25.RData")
load("data/m04_metaCycGeneIDs_v25.RData")
load("data/m05_metaCycPathways_v25.RData")


# Load functions ----------------------------------------------------------

# Utility functions
source(file.path("functions", "utilityFunctions.R"), local = TRUE)$value

# Primary mapping functions
source(file.path("functions", "mapGenerally.R"), local = TRUE)$value
source(file.path("functions", "mapPathways.R"), local = TRUE)$value
source(file.path("functions", "visualizePathways.R"), local = TRUE)$value

# App and UI functions
source(file.path("functions", "alertFunctions.R"), local = TRUE)$value
source(file.path("functions", "generateTables.R"), local = TRUE)$value


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
