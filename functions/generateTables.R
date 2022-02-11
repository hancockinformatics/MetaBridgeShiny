#' generateSummaryTable
#'
#' @param mappingObject Metabolite-containing object produced by mapping step
#' @param idType Selected ID type
#' @param dbChosen Database chosen for the mapping
#'
#' @return Table to be rendered by `DT::renderDataTable()`
#' @export
#'
#' Generates a nice DT table summarizing the mapping results
#'
generateSummaryTable <- function(mappingObject,
                                 idType,
                                 dbChosen) {

  # Should never be null since we're not responding until map button is clicked,
  # but good to have just in case
  if (is.null(mappingObject$data)) {
    return(NULL)


  # Statement of there was an error or silent failure
  } else if (mappingObject$status == "error" | mappingObject$status == "empty") {
    return(mappingObject$data)


  # Summary if MetaCyc was the selected database
  } else if (dbChosen == "MetaCyc") {

    table <- mappingObject$data %>%
      group_by(Compound, HMDB, KEGG) %>%
      summarize(
        "Reactions" = n_distinct(`Reaction`, na.rm = TRUE),
        "Genes (MetaCyc)" = n_distinct(`MetaCyc Gene`, na.rm = TRUE),
        "Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
        "Genes (Ensembl)" = n_distinct(`Ensembl`, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      ungroup() %>%
      arrange(Compound)

    # Return the results for the user, and provide database info
    return(list("table" = table, "dbChosen" = "MetaCyc"))


  # Summary table if KEGG was the chosen database
  } else if (dbChosen == "KEGG") {

    table <- mappingObject$data %>%
      group_by(Compound, KEGG, HMDB) %>% # changed here
      summarize(
        "Enzymes" = n_distinct(`Enzyme`, na.rm = TRUE),
        "Gene Names" = n_distinct(`Gene Name`, na.rm = TRUE),
        "Genes (Entrez)" = n_distinct(`Entrez`, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      ungroup() %>%
      arrange(Compound)

    # Return the results for the user, and provide database info
    return(list("table" = table, "dbChosen" = "KEGG"))
  }

}




#' generateMetaCycMetabTable
#'
#' @param mappingObject Object produced by mapping function mapGenerally()
#' @param summaryTable Summary table produced by above function
#'   generateSummaryTable()
#' @param selectedRows Row selected by the user containing a metabolite that we
#'   will grab info for
#' @param idType ID type of selected row (done by the user)
#'
#' @return Table to be rendered by `DT::renderDataTable()`
#' @export
#'
#' Generate MetaCyc table for the user-selected row
#'
generateMetaCycMetabTable <- function(mappingObject,
                                      summaryTable,
                                      selectedRows,
                                      idType) {

  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data) | is.null(selectedRows) | is.null(summaryTable)) {

    # Returns an empty data frame
    return(data.frame())

  # If the mapping object is not NULL, continue as planned
  } else {

    # Quote necessary variables for dplyr
    namedIDType <- as.name(idType)
    quotedIDType <- quo(idType)
    pastedIDType <- paste0(idType)

    # Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) == selectedRows, ]

    # Extract the particular ID from the selected row
    selectedMetab <- selectedMetab %>%
      extract2(pastedIDType)

    # Quote for NSE
    quotedSelectedMetab <- enquo(selectedMetab)

    # Filter the mapping object for the selected metabolite
    filteredMappedMetaboliteTable <- mappingObject$data %>%
      filter(!!(namedIDType) == !!(quotedSelectedMetab)) %>%
      arrange(`Gene Name`)

    # Return the filtered table to the user (i.e. the details behind the summary
    # for that particular metabolite)
    return(filteredMappedMetaboliteTable)
  }
}




#' generateKEGGMetabTable
#'
#' @param mappingObject Full results from the metabolite mapping
#' @param summaryTable Summary table as made by generateSummarytable()
#' @param selectedRows Row selected by the user with desired metabolite
#' @param idType ID type for the selected metabolite
#'
#' @return Table to be rendered by `DT::renderDataTable()`
#' @export
#'
#' Generate the more detailed table for a particular metabolite if the chosen
#' database was KEGG
#'
generateKEGGMetabTable <- function(mappingObject,
                                   summaryTable,
                                   selectedRows,
                                   idType) {

  # Should never be null since we're not responding until map button is
  # clicked, but good to have just in case
  if (is.null(mappingObject$data) | is.null(selectedRows) | is.null(summaryTable)) {
    return(data.frame())

  # Continue if there was no failure
  } else {

    # Quote necessary variables for dplyr
    namedIDType <- as.name(idType)
    quotedIDType <- quo(idType)
    pastedIDType <- paste0(idType)

    # Pull the selected row and extract its compound ID
    selectedMetab <-
      summaryTable[as.numeric(rownames(summaryTable)) == selectedRows, ]

    # If mapping against the KEGG database, pull out the KEGG CPD ID (even if
    # not what was supplied), and extract the ID from the HTML contents of the
    # cell
    selectedMetab <- selectedMetab %>%
      extract2("KEGG") %>%
      # Make sure we've just got the kegg compound ID
      str_extract("C[0-9]{5}")

    # Quote for NSE
    quotedSelectedMetab <- enquo(selectedMetab)

    # Establish column name for filtering step
    namedIDType <- as.name("KEGG")

    # Filter the full mapping table based on chosen compund
    filteredMappedMetaboliteTable <- mappingObject$data %>%
      filter(!!(namedIDType) == !!(quotedSelectedMetab)) %>%
      arrange(`Gene Name`)

    return(filteredMappedMetaboliteTable)
  }
}




#' hyperlinkTable
#'
#' @param table Table of mapped metabolites
#' @param dbChosen Chosen database
#'
#' @return Table which renders the various IDs as links to their respective page
#' @export
#'
#' Generates a hyperlink column based on the IDs present
#'
hyperlinkTable <- function(table, dbChosen) {

  # If KEGG IDs, and either database
  if ("KEGG" %in% colnames(table)) {
    table <- table %>%
      mutate(KEGG = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "http://www.genome.jp/dbget-bin/www_bget?cpd:",
        KEGG, '">', KEGG, "</a>"
      ))
  }

  # Enzyme names and KEGG database
  if ("Enzyme" %in% colnames(table) && dbChosen == "KEGG") {
    table <- table %>%
      mutate(Enzyme = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "http://www.genome.jp/dbget-bin/www_bget?ec:",
        Enzyme, '">', Enzyme, "</a>"
      ))
  }

  # HMDB IDs, and either database
  if ("HMDB" %in% colnames(table)) {
    table <- table %>%
      mutate(HMDB = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "http://www.hmdb.ca/metabolites/",
        HMDB, '">', HMDB, "</a>"
      ))
  }

  # Compound names and MetaCyc database
  if ("Compound" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(Compound = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "https://metacyc.org/compound?orgid=META&id=",
        Compound, '">', Compound, "</a>"
      ))
  }

  # Reaction name and MetaCyc database
  if ("Reaction" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(Reaction = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "https://metacyc.org/META/NEW-IMAGE?type=REACTION&object=",
        Reaction, '">', Reaction, "</a>"
      ))
  }

  # MetaCyc Gene and MetaCyc database
  if ("MetaCyc Gene" %in% colnames(table) && dbChosen == "MetaCyc") {
    table <- table %>%
      mutate(`MetaCyc Gene` = paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "https://metacyc.org/gene?orgid=META&id=",
        `MetaCyc Gene`, '">', `MetaCyc Gene`, "</a>"
      ))
  }

  # Ensembl gene, and either database
  if ("Ensembl" %in% colnames(table)) {
    table <- table %>%
      rowwise() %>%
      mutate(Ensembl = ifelse(is.na(Ensembl), NA, paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "http://www.ensembl.org/id/",
        Ensembl, '">', Ensembl, "</a>"
      ))) %>%
      ungroup()
  }

  # Entrez gene and either database
  if ("Entrez" %in% colnames(table)) {
    table <- table %>%
      rowwise() %>%
      mutate(Entrez = ifelse(is.na(Entrez), NA, paste0(
        '<a target="_blank" rel="noopener noreferrer" href="',
        "https://www.ncbi.nlm.nih.gov/gene/",
        Entrez, '">', Entrez, "</a>"
      ))) %>%
      ungroup()
  }

  return(table)
}
