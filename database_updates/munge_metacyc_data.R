
# MetaCyc data was last updated on 2022-01-18, using MetaCyc v25.5


# Load packages -----------------------------------------------------------

library(splitstackshape)
library(glue)
library(tidyverse)


# Set paths ---------------------------------------------------------------

rootDir    <- rprojroot::find_rstudio_root_file()
updateDir  <- file.path(rootDir, 'database_updates')
exampleDir <- file.path(rootDir, 'example_data')
dataDir    <- file.path(rootDir, 'data')

metacycVersion <- "v25"


# 1. MetaCyc Compounds ----------------------------------------------------

metaCycDBLinks_0 <- read_tsv(file.path(updateDir, "1-compounds-ids.txt"))

metaCycDBLinks_1 <- metaCycDBLinks_0 %>%
  rename("KEGG" = Kegg) %>%
  mutate(
    across(HMDB:KEGG, ~str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(Compound:KEGG, str_trim)
  )

# save(
#   metaCycDBLinks_1,
#   file = file.path(dataDir, glue("m01_metaCycDBLinks_{metacycVersion}.RData"))
# )


# 2. MetaCyc Reactions ----------------------------------------------------

metaCycCompoundsReactions_0 <-
  read_tsv(file.path(updateDir, "2-compounds-reactions.tsv"))

metaCycCompoundsReactions_1 <- metaCycCompoundsReactions_0 %>%
  select("reaction" = ID, "reactionName" = Name, "compound" = Matches) %>%
  cSplit(
    splitCols    = c("compound"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  )

# save(
#   metaCycCompoundsReactions_1,
#   file = file.path(
#     dataDir,
#     glue("m02_metaCycCompoundsReactions_{metacycVersion}.RData")
#   )
# )


# 3. MetaCyc Genes --------------------------------------------------------

metaCycReactionsGenes_0 <-
  read_tsv(file.path(updateDir, '3-reactions-genes.tsv'))

metaCycReactionsGenes_1 <- metaCycReactionsGenes_0 %>%
  select("geneID" = ID, "geneName" = Name, "reaction" = Matches) %>%
  cSplit(
    splitCols    = c("reaction"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  )

# save(
#   metaCycReactionsGenes_1,
#   file = file.path(
#     dataDir,
#     glue("m03_metaCycReactionsGenes_{metacycVersion}.RData")
#   )
# )


# 4. Map to Gene IDs ------------------------------------------------------

metaCycGeneIDs_0 <- read_tsv(file.path(updateDir, "4-genes-ids.txt"))

metaCycGeneIDs_1 <- metaCycGeneIDs_0 %>%
  mutate(
    across(Ensembl:GeneCards, ~str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(everything(), str_trim)
  ) %>%
  rename("geneID" = `Gene Name`, "Symbol" = GeneCards)

# save(
#   metaCycGeneIDs_1,
#   file = file.path(dataDir, glue("m04_metaCycGeneIDs_{metacycVersion}.RData"))
# )


# 5. Map to Pathways ------------------------------------------------------

metaCycPathways_0 <- read_tsv(file.path(updateDir, "5-pathways-reactions.tsv"))

metaCycPathways_1 <- metaCycPathways_0 %>%
  select("pathwayID" = ID, "pathwayName" = Name, "reaction" = Matches) %>%
  cSplit(
    splitCols    = c("reaction"),
    sep          = ", ",
    direction    = "long",
    type.convert = FALSE,
    stripWhite   = FALSE
  )

# save(
#   metaCycPathways_1,
#   file = file.path(dataDir, glue("m05_metaCycPathways_{metacycVersion}.RData"))
# )
