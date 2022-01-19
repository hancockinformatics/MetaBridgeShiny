#' visualizePathview
#'
#' @param pathway Pathway to make the diagram of
#' @param genes Genes to be included in the pathway map
#' @param cpd Data being used to generate the pathway map
#'
#' @return Create PNG file of pathway which will be displayed to the user.
#' @export
#'
#' Given a selected pathway, calls pathview to show the enzyme/metabolite
#' interactions involving the selected genes
#'
visualizePathview <- function(pathway, genes, cpd) {

  # Generate the PNG of desired pathway, and suppress messages from pathview()
  suppressWarnings({
    pathview(
      gene.data   = genes,
      cpd.data    = cpd,
      pathway.id  = pathway,
      gene.idtype = "SYMBOL",
      species     = "hsa",
      kegg.dir    = "pathways"
    )
  })

  # Move the file created by pathview into a separate folder
  filename_original <- paste0("hsa", pathway, ".pathview.png")
  filename_new <- paste0("pathways_results/", filename_original)

  file.rename(from = filename_original, to = filename_new)
  message("INFO: Pathway file was renamed/moved.")

  return(filename_new)
}
