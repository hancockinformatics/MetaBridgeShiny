#' visualizePathview
#'
#' @param pathway Pathway to make the diagram of
#' @param genes Genes to be included in the pathway map
#' @param cpd Data being used to generate the pathway map
#'
#' @return Create PNG file of pathway which will be displayed to the user.
#'
visualizePathview <- function(pathway, genes, cpd) {
  suppressWarnings({
    pathview::pathview(
      gene.data = genes,
      cpd.data = cpd,
      pathway.id = pathway,
      gene.idtype = "SYMBOL",
      species = "hsa",
      kegg.dir = "pathways"
    )
  })

  filename_original <- paste0("hsa", pathway, ".pathview.png")
  filename_new <- paste0("pathways/results/", filename_original)

  file.rename(from = filename_original, to = filename_new)
  message("INFO: Pathway file was renamed/moved.\n")

  return(filename_new)
}
