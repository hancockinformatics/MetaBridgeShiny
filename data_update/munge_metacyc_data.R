# Load packages -----------------------------------------------------------

library(dplyr)


# Set paths ---------------------------------------------------------------

updateDir <- "data_update"
exampleDir <- "example_data"
dataDir <- "data"

metacycVersion <- "v26"


# 1. MetaCyc Compounds ----------------------------------------------------

metaCycDBLinks_0 <- read.delim(
  file.path(updateDir, "1-compounds-ids.tsv"),
  sep = "\t"
)

m01_metaCycDBLinks <- metaCycDBLinks_0 %>%
  rename("KEGG" = Kegg) %>%
  mutate(
    across(HMDB:KEGG, ~stringr::str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(Compound:KEGG, str_trim)
  )


# 2. MetaCyc Reactions ----------------------------------------------------

metaCycCompoundsReactions_0 <- read.delim(
  file.path(updateDir, "2-compounds-reactions.tsv"),
  sep = "\t",
  quote = ""
)

m02_metaCycCompoundsReactions <- metaCycCompoundsReactions_0 %>%
  select("reaction" = ID, "reactionName" = Name, "compound" = Matches) %>%
  tidyr::separate_longer_delim(cols = compound, delim = ", ") %>%
  as_tibble()


# 3. MetaCyc Genes --------------------------------------------------------

metaCycReactionsGenes_0 <- read.delim(
  file.path(updateDir, '3-reactions-genes.tsv'),
  sep = "\t"
)

m03_metaCycReactionsGenes <- metaCycReactionsGenes_0 %>%
  select("geneID" = ID, "geneName" = Name, "reaction" = Matches) %>%
  tidyr::separate_longer_delim(cols = reaction, delim = ", ") %>%
  as_tibble()


# 4. Map to Gene IDs ------------------------------------------------------

metaCycGeneIDs_0 <- read.delim(
  file.path(updateDir, "4-genes-ids.tsv"),
  sep = "\t"
)

m04_metaCycGeneIDs <- metaCycGeneIDs_0 %>%
  mutate(
    across(Ensembl:GeneCards, ~stringr::str_remove_all(.x, "^<a href='.*'>|<\\/a>$")),
    across(everything(), str_trim)
  ) %>%
  rename("geneID" = `Gene Name`, "Symbol" = GeneCards)


# 5. Map to Pathways ------------------------------------------------------

metaCycPathways_0 <- read.delim(
  file.path(updateDir, "5-pathways-reactions.tsv"),
  sep = "\t"
)

m05_metaCycPathways <- metaCycPathways_0 %>%
  select("pathwayID" = ID, "pathwayName" = Name, "reaction" = Matches) %>%
  tidyr::separate_longer_delim(cols = reaction, delim = ", ") %>%
  as_tibble()


# Save it all as one object -----------------------------------------------

save(
  m01_metaCycDBLinks,
  m02_metaCycCompoundsReactions,
  m03_metaCycReactionsGenes,
  m04_metaCycGeneIDs,
  m05_metaCycPathways,
  file = paste0("data/metaCyc_data_", metacycVersion, ".RData")
)

