
# MetaCyc data was last updated on January 18th, 2022, using MetaCyc v25.5


# Load packages -----------------------------------------------------------

library(tidyverse)
import::from(splitstackshape, cSplit, .into = "")


# Set paths ---------------------------------------------------------------

rootDir    <- rprojroot::find_rstudio_root_file()
updateDir  <- file.path(rootDir, "database_updates")
exampleDir <- file.path(rootDir, "example_data")
dataDir    <- file.path(rootDir, "data")

metacycVersion <- "v26"


# 1. MetaCyc Compounds ----------------------------------------------------

metaCycDBLinks_0 <- read_tsv(file.path(updateDir, "1-compounds-ids.tsv"))

m01_metaCycDBLinks <- metaCycDBLinks_0 %>%
  rename("KEGG" = Kegg) %>%
  mutate(
    across(HMDB:KEGG, ~str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(Compound:KEGG, str_trim)
  )


# 2. MetaCyc Reactions ----------------------------------------------------

metaCycCompoundsReactions_0 <-
  read_tsv(file.path(updateDir, "2-compounds-reactions.tsv"))

m02_metaCycCompoundsReactions <- metaCycCompoundsReactions_0 %>%
  select("reaction" = ID, "reactionName" = Name, "compound" = Matches) %>%
  cSplit(
    splitCols    = c("compound"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  ) %>%
  as_tibble()


# 3. MetaCyc Genes --------------------------------------------------------

metaCycReactionsGenes_0 <-
  read_tsv(file.path(updateDir, '3-reactions-genes.tsv'))

m03_metaCycReactionsGenes <- metaCycReactionsGenes_0 %>%
  select("geneID" = ID, "geneName" = Name, "reaction" = Matches) %>%
  cSplit(
    splitCols    = c("reaction"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  ) %>%
  as_tibble()


# 4. Map to Gene IDs ------------------------------------------------------

metaCycGeneIDs_0 <- read_tsv(file.path(updateDir, "4-genes-ids.tsv"))

m04_metaCycGeneIDs <- metaCycGeneIDs_0 %>%
  mutate(
    across(Ensembl:GeneCards, ~str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(everything(), str_trim)
  ) %>%
  rename("geneID" = `Gene Name`, "Symbol" = GeneCards)


# 5. Map to Pathways ------------------------------------------------------

metaCycPathways_0 <- read_tsv(file.path(updateDir, "5-pathways-reactions.tsv"))

m05_metaCycPathways <- metaCycPathways_0 %>%
  select("pathwayID" = ID, "pathwayName" = Name, "reaction" = Matches) %>%
  cSplit(
    splitCols    = c("reaction"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  ) %>%
  as_tibble()


# Save it all as one object -----------------------------------------------

save(
  m01_metaCycDBLinks,
  m02_metaCycCompoundsReactions,
  m03_metaCycReactionsGenes,
  m04_metaCycGeneIDs,
  m05_metaCycPathways,
  file = glue::glue("data/metaCyc_data_{metacycVersion}.RData")
)

