
# Load packages -----------------------------------------------------------

library(tools)
library(magrittr)
library(rlang)
library(DT)
# install.packages("BiocManager")
# BiocManager::install("pathview")
library(pathview)
library(shinycssloaders)
library(tidyverse)


# Load data -------------------------------------------------------

# Examples
load("data/examples_2.RData")

# Load KEGG database files, updated on/around November 1st, 2019
load("data/k00_keggCompounds_r92.RData")
load("data/k02b_keggEnzymeShortNames_r92.RData")
load("data/k03_keggGenes_r92.RData")
load("data/k04_keggPathways_r92.RData")
load("data/k05_keggHumanPathways_r92.RData")

# For the moment, only keep enzyme-gene relationships
keggGenes <- keggGenes %>%
  dplyr::select(-KEGG) %>%
  unique()

# Load MetaCyc database files, updated on/around September 19th, 2019
load("data/m01_metaCycDBLinks_v23.RData")
load("data/m02_metaCycCompoundsReactions_v23.RData")
load("data/m03_metaCycReactionsGenes_v23.RData")
load("data/m04_metaCycGeneIDs_v23.RData")
load("data/m05_metaCycPathways_v23.RData")


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
