#' mapPathways
#'
#' @param idType Selected ID type used for mapping
#' @param selectedRow Highlighted row from the user with the metabolite to get
#'   mapping info for
#' @param summaryTable Summary table of metabolite mapping
#' @param fullTable Complete results table from metabolite mapping
#'
#' @return Mapped pathways returned if mapping with KEGG.
#'
mapPathways <- function(
    idType,
    selectedRow,
    summaryTable,
    fullTable
) {
  # To be treated like a variable
  namedIDType <- as.name(idType)
  KEGGname <- as.name("KEGG")

  selectedMetab <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2(idType) %>%
    stringr::str_extract("C[0-9]{5}")

  quotedMetab <- enquo(selectedMetab)

  selectedMetabName <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2("Compound")

  pathwaysOfInterest <- k04_keggPathwayNames %>%
    filter(
      !!(namedIDType) == !!(quotedMetab),
      id %in% k05_keggPathwayIDs,
      id != "01100"
    )

  genesOfInterest <- fullTable %>%
    filter(!!(KEGGname) == !!(quotedMetab)) %>%
    extract2("Gene Name")

  return(list(
    "selectedCompound" = selectedMetab,
    "selectedCompoundName" = selectedMetabName,
    "genesOfSelectedCompound" = genesOfInterest,
    "pathwaysOfSelectedCompound" = pathwaysOfInterest
  ))
}
