#' mapKEGGPathways
#'
#' @param idType ID type/format being used in the mapping
#' @param selectedRow Row which is selected/highlighted by the user
#' @param summaryTable Summary table prodiced by earlier mapping
#' @param fullTable Full table...?
#'
#' @return Mapped pathways returned if mapping with KEGG.
#'
mapKEGGPathways <- function(
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


#' MapMetaCycPathways
#'
#' @param idType ID type being used in the mapping
#' @param selectedRow Row selected or highlighted by the user
#' @param summaryTable Summary table produced in earlier mapping step
#' @param fullTable
#'
#' @return Mapped pathways as determined by mapping with MetaCyc.
#'
mapMetaCycPathways <- function(
    idType,
    selectedRow,
    summaryTable,
    fullTable
) {
  # To be treated like a variable
  namedIDType <- as.name(idType)

  selectedMetab <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2(idType)

  # To be treated like a character string
  quotedMetab <- enquo(selectedMetab)

  selectedMetabName <-
    summaryTable[as.numeric(rownames(summaryTable)) == selectedRow, ] %>%
    extract2("Compound")

  genesOfInterest <- fullTable %>%
    filter(!!(namedIDType) == !!(quotedMetab)) %>%
    extract2("Official Gene Symbol")

  selectedReaction <- fullTable %>%
    filter(!!(namedIDType) == !!(quotedMetab)) %>%
    extract2("Reaction")

  quotedSelectedReaction <- enquo(selectedReaction)

  pathwaysOfInterest <- m05_metaCycPathways %>%
    filter(reaction %in% !!(selectedReaction))

  return(list(
    "selectedCompound" = selectedMetab,
    "selectedCompoundName" = selectedMetabName,
    "genesOfSelectedCompound" = genesOfInterest,
    "pathwaysOfSelectedCompound" = pathwaysOfInterest
  ))
}




#' generalPathwayMapping
#'
#' @param db Selected databse, one of KEGG or MetaCyc
#' @param idType Selected ID type used for mapping
#' @param selectedRow Highlighted row from the user with the metabolite to get
#'                    mapping info for
#' @param summaryTable Summary table; the output of the earlier mnapping step
#' @param fullTable
#'
#' @return Mapped pathways to be returned to the user.
#'
generalPathwayMapping <- function(
    db,
    idType,
    selectedRow,
    summaryTable,
    fullTable
) {
  if (db == "KEGG") {
    mapKEGGPathways(
      idType = "KEGG",
      selectedRow = selectedRow,
      summaryTable = summaryTable,
      fullTable = fullTable
    )
  } else if (db == "MetaCyc") {
    mapMetaCycPathways(
      idType = idType,
      selectedRow = selectedRow,
      summaryTable = summaryTable,
      fullTable = fullTable
    )
  }
}
