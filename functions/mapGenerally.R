#' mapMetaCyc
#'
#' @param importDF Input data from the user
#' @param col Selcted column to be used for mapping
#' @param idType ID type of the selected column
#'
#' @return Table of mapped metabolites, done via MetaCyc
#'
mapMetaCyc <- function(importDF, col, idType) {

  # Deal with NSE
  enquotedCol <- enquo(col)
  quotedCol <- quo(col)
  quotedID <- quo(idType)

  mappingDF <- tryCatch(
    {
      this <- tibble(
        !!(idType) := importDF %>%
          use_series(!!(col)) %>%
          as.character() %>%
          notNAs() %>%
          notEmpty() %>%
          str_trim()
      )

      # Sanitize our HMDB IDs if we are using HMDB IDs
      if (idType == "HMDB") {
        this <- this %>%
          rowwise() %>%
          mutate(!!(idType) := matchHMDB(!!(as.name(idType)))) %>%
          ungroup() %>%
          tidyr::drop_na(!!(idType))
      }

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = importDF,
          message = "We were unable to properly import your data.",
          suggest = "Try changing your mapping parameters."
        )
      } else {
        list(
          status  = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "We were unable to properly import your data.",
        suggest = "Try changing your mapping parameters."
      )
    },
    error = function(errorMessage) {
      list(
        status = "error",
        data = importDF,
        internalMessage = errorMessage,
        message = "We were unable to properly import your data.",
        suggest = "Try changing your mapping parameters."
      )
    })

  message("INFO: mapMetaCyc step 1 (prep):")
  message(paste(
    "\t", mappingDF[names(mappingDF) != "data"], collapse = "\n"
  ))

  if (mappingDF$status == "error" | mappingDF$status == "empty") {
    return(mappingDF)
  }

  # Next step: Map these to the MetaCyc Object IDs
  mappedToObjects <- tryCatch(
    {
      if (idType == "Compound") {
        this <- mappingDF$data %>%
          rename("compound" = Compound)
      } else {
        this <- inner_join(mappingDF$data, m01_metaCycDBLinks, by = idType) %>%
          select(all_of(idType), Compound, HMDB, KEGG) %>%
          rename("compound" = Compound)
      }

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = mappingDF$data,
          message = paste0(
            "We were unable to find any matches in the MetaCyc database for the ",
            "compound IDs you provided."
          ),
          suggest = "Try using a different compound ID or mapping via KEGG."
        )

      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "There was an unspecified error in mapping your compounds.",
        suggest = paste0(
          "Please check the About page for a link to our Github, where you can ",
          "report this issue."
        )
      )
    }, error = function(errorMessage) {
      list(
        status = "error",
        data = mappingDF$data,
        internalMessage = errorMessage,
        message = "We were unable to map your metabolites to MetaCyc IDs.",
        suggest = "Try changing your mapping parameters."
      )

    })

  # If tryCatch exited with status != 0, stop here
  message("INFO: mapMetaCyc step 2 (objects):")
  message(paste(
    "\t", mappedToObjects[names(mappedToObjects) != "data"], collapse = "\n"
  ))

  if (mappedToObjects$status == "error" | mappedToObjects$status == "empty") {
    return(mappedToObjects)
  }

  # Finally, join the reaction-gene table!
  mappedToReactions <- tryCatch(
    {

      this <- inner_join(
        mappedToObjects$data,
        m02_metaCycCompoundsReactions,
        by = "compound"
      )

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = mappedToObjects$data,
          message = "We were unable to map your compounds to any reactions.",
          suggest = "Try using a different compound ID or mapping via KEGG."
        )
      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "Your compounds were mapped, but there was a problem.",
        suggest = NULL
      )
    }, error = function(errorMessage) {
      list(
        status = "error",
        data = mappedToObjects$data,
        internalMessage = errorMessage,
        message = "We were unable to map your compounds to any reactions.",
        suggest = "Try changing your mapping parameters."
      )
    })

  # Return to the user of there was an error or if the join failed silently
  message("INFO: mapMetaCyc step 3 (reactions):")
  message(paste(
    "\t", mappedToReactions[names(mappedToReactions) != "data"], collapse = "\n"
  ))

  if (mappedToReactions$status == "error" |
      mappedToReactions$status == "empty") {
    return(mappedToReactions)
  }


  # Finally, join the reaction-gene table!
  mappedToGenes <- tryCatch({

    # Join and filter the data
    this <- inner_join(
      mappedToReactions$data,
      m03_metaCycReactionsGenes,
      by = "reaction",
      relationship = "many-to-many"
    ) %>%
      # Make sure we only return human genes
      filter(str_detect(tolower(geneID), "^hs")) %>%
      dplyr::rename(
        "Reaction" = reaction,
        "Reaction Name" = reactionName,
        "Compound" = compound,
        "MetaCyc Gene" = geneID,
        "Gene Name" = geneName
      )

    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = "empty",
        data = mappedToReactions$data,
        message = "We were unable to map your reactions to any genes.",
        suggest = "Try using a different compound ID or mapping via KEGG."
      )
    } else {
      list(
        status = "success",
        data = this,
        message = "Your metabolites have been successfully mapped!",
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = "warn",
      data = this,
      internalMessage = warningMessage,
      message = "Your compounds were mapped, but there may have been a problem.",
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = "error",
      data = mappedToReactions$data,
      internalMessage = errorMessage,
      message = "We were unable to map your reactions to any genes.",
      suggest = "Try changing your mapping parameters."
    )
  })

  # Return and exit if there was an error, silent or otherwise
  message("INFO: mapMetaCyc step 4 (genes):")
  message(paste(
    "\t", mappedToGenes[names(mappedToGenes) != "data"], collapse = "\n"
  ))

  if (mappedToGenes$status == "error" | mappedToGenes$status == "empty") {
    return(mappedToGenes)
  }


  # Finally, finally, map biocyc gene IDs to ensembl gene IDs
  mappedToEnsembl <- tryCatch({

    this <- left_join(
      mappedToGenes$data,
      m04_metaCycGeneIDs,
      by = c("MetaCyc Gene" = "geneID")
    ) %>%
      dplyr::select(
        Compound,
        all_of(idType),
        HMDB,
        KEGG,
        Reaction,
        `Reaction Name`,
        `MetaCyc Gene`,
        `Gene Name`,
        Ensembl
      ) %>%
      # Filter out rows where no gene IDs are present
      filter(!(
        is.na(`MetaCyc Gene`) &
          is.na(`Gene Name`) & is.na(`Ensembl`)
      ))

    # Check to see if join failed silently
    if (nrow(this) == 0) {
      list(
        status = "empty",
        data = mappedToReactions$data,
        message = paste0(
          "We were unable to find any matches for human gene IDs given the ",
          "enzymes mapped."
        ),
        suggest = "Try using a different compound ID or mapping via KEGG."
      )

    } else {
      list(
        status = "success",
        data = this,
        message = "Your metabolites have been successfully mapped!",
        suggest = NULL
      )
    }
  }, warning = function(warningMessage) {
    list(
      status = "warn",
      data = this,
      internalMessage = warningMessage,
      message = "Your compounds were mapped, but there may have been a problem.",
      suggest = NULL
    )
  }, error = function(errorMessage) {
    list(
      status = "error",
      data = mappedToReactions$data,
      internalMessage = errorMessage,
      message = "There was an error mapping your compounds to human gene IDs.",
      suggest = "Try changing your mapping parameters."
    )
  }
  )

  message("INFO: mapMetaCyc step 5 (Ensembl):")
  message(paste(
    "\t", mappedToEnsembl[names(mappedToEnsembl) != "data"], collapse = "\n"
  ))

  return(mappedToEnsembl)
}




#' mapKEGG
#'
#' @param importDF Input data frame from the user
#' @param col Column to be used for the mapping
#' @param idType # ID type contained in the selected column
#'
#' @return Table of mapped metabolites accomplished with KEGG.
#'
mapKEGG <- function(importDF, col, idType) {

  # Deal with NSE
  quotedIDtype <- enquo(idType)
  namedIDtype <- as.name(idType)
  keggName <- as.name("KEGG")
  keggQuote <- quo("KEGG")

  mappingDF <- tryCatch(
    {
      # The return value in this chunk is the actual value that will be returned
      # in case there is no condition (e.g. warning or error). You don't need to
      # state the return value via `return()` as code in the "try" part is not
      # wrapped insided a function (unlike that for the condition handlers for
      # warnings and error below)

      this <- tibble(
        !!(namedIDtype) := extract2(importDF, col) %>%
          notNAs() %>%
          notEmpty() %>%
          str_trim()
      )

      # Use our `matchHMDB()` function to sanitize HMDB IDs
      if (idType == "HMDB") {
        this <- this %>%
          rowwise() %>%
          mutate(!!(idType) := matchHMDB(!!(as.name(idType)))) %>%
          ungroup() %>%
          tidyr::drop_na(!!(idType))
      }

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = importDF,
          message = "We were unable to properly import your data.",
          suggest = "Try changing your mapping parameters."
        )
      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "We were unable to properly import your data.",
        suggest = "Try changing your mapping parameters."
      )

    }, error = function(errorMessage) {
      list(
        status = "error",
        data = importDF,
        internalMessage = errorMessage,
        message = "We were unable to properly import your data.",
        suggest = "Try changing your mapping parameters."
      )
    }
  )

  message("INFO: mapKEGG step 1 (prep):")
  message(paste(
    "\t", mappingDF[names(mappingDF) != "data"], collapse = "\n"
  ))

  # Mapping if NOT using KEGG IDs
  if (idType != "KEGG") {

    keggIDs <- tryCatch({
      this <- m01_metaCycDBLinks %>%
        filter(!!(namedIDtype) %in% extract2(mappingDF$data, !!(quotedIDtype)))

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = importDF,
          message = paste0(
            "We were unable to map the ", idType, " IDs you provided to KEGG ",
            "compound IDs."
          ),
          suggest = "Try using a different compound ID or mapping via MetaCyc."
        )
      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "Your compounds were mapped, but there was a problem.",
        suggest = NULL
      )
    }, error = function(errorMessage) {
      list(
        status = "error",
        data = importDF,
        internalMessage = errorMessage,
        message = paste0(
          "We were unable to map the ", idType, " IDs you provided to KEGG ",
          "compound IDs."
        ),
        suggest = "Try using a different compound ID or mapping via MetaCyc."
      )
    })

    # Mapping if using KEGG IDs
  } else if (idType == "KEGG") {

    keggToHMDB <- m01_metaCycDBLinks %>%
      select(KEGG, HMDB) %>%
      tidyr::drop_na(KEGG)

    this <- left_join(mappingDF$data, k01_keggCompounds, by = "KEGG") %>%
      left_join(., keggToHMDB, by = "KEGG")

    keggIDs <- list(
      status = "success",
      data = this,
      message = "Your metabolites have been successfully mapped!",
      suggest = NULL
    )
  }

  message("INFO: mapKEGG step 2 (IDs):")
  message(paste(
    "\t", keggIDs[names(keggIDs) != "data"], collapse = "\n"
  ))

  # Mapping to KEGG enzymes
  keggEnzymesOfInterest <- tryCatch(
    {
      this <- left_join(keggIDs$data, k02_keggEnzymeNames, by = "KEGG")

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = keggIDs$data,
          message = paste0(
            "We were unable to find any matches for the compounds you ",
            "supplied. Here are the KEGG compound IDs we queried."
          ),
          suggest = "Try using a different compound ID or mapping via MetaCyc."
        )
      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "Your compounds were mapped, but there may have been a problem.",
        suggest = NULL
      )
    }, error = function(errorMessage) {
      list(
        status = "error",
        data = keggIDs$data,
        internalMessage = errorMessage,
        message = paste0(
          "There was an error mapping your compounds via KEGG. Here are the ",
          "KEGG compound IDs we queried."
        ),
        suggest = "Try changing your mapping parameters."
      )
    })

  message("INFO: mapKEGG step 3 (enzymes):")
  message(paste(
    "\t", keggEnzymesOfInterest[names(keggEnzymesOfInterest) != "data"], collapse = "\n"
  ))

  # Mapping to genes
  keggGenesOfInterest <- tryCatch(
    {
      keggGeneDB <- k03_keggGenes %>% select(enzymes, entrez, symbol)

      this <- inner_join(
        keggEnzymesOfInterest$data,
        keggGeneDB,
        by = "enzymes",
        relationship = "many-to-many"
      ) %>%
        rename(
          "KEGG" = KEGG,
          "Enzyme" = enzymes,
          "Enzyme Name" = enzymeName,
          "Gene Name" = symbol,
          "Entrez" = entrez
        ) %>%
        select(
          Compound,
          KEGG,
          HMDB,
          Enzyme,
          `Enzyme Name`,
          `Gene Name`,
          `Entrez`
        )

      if (nrow(this) == 0) {
        list(
          status = "empty",
          data = keggEnzymesOfInterest$data,
          message = paste0(
            "We were unable to match the enzymes your compounds interact with ",
            "to any human genes. Here are the enzymes and their directly ",
            "interacting enzymes. "
          ),
          suggest = "Try using a different compound ID or mapping via MetaCyc."
        )
      } else {
        list(
          status = "success",
          data = this,
          message = "Your metabolites have been successfully mapped!",
          suggest = NULL
        )
      }
    }, warning = function(warningMessage) {
      list(
        status = "warn",
        data = this,
        internalMessage = warningMessage,
        message = "Your compounds were mapped, but there may have been a problem.",
        suggest = NULL
      )
    }, error = function(errorMessage) {
      list(
        status = "error",
        data = keggEnzymesOfInterest$data,
        internalMessage = errorMessage,
        message = paste0(
          "There was an error mapping your compounds via KEGG. Here are the ",
          "enzymes and their directly interacting enzymes. "
        ),
        suggest = "Try changing your mapping parameters."
      )
    })

  message("INFO: mapKEGG step 4 (genes):")
  message(paste(
    "\t", keggGenesOfInterest[names(keggGenesOfInterest) != "data"], collapse = "\n"
  ))

  return(keggGenesOfInterest)
}




#' mapGenerally
#'
#' @param importDF User input data frame
#' @param col Column selected by the user
#' @param db Database to use in the mapping
#' @param idType ID type of selected column
#'
#' @return Table of mapped metabolites generated by either `mapMetaCyc()` or
#'   `mapKEGG()`.
#'
mapGenerally <- function(importDF, col, db, idType) {
  # Mapping if KEGG is selected
  if (db == "KEGG") {
    mappedMetabolites <- mapKEGG(
      importDF = importDF,
      col      = col,
      idType   = idType
    )

    # Mapping if MetaCyc is selected
  } else if (db == "MetaCyc") {
    mappedMetabolites <- mapMetaCyc(
      importDF = importDF,
      col      = col,
      idType   = idType
    )

    # Return a basic error when something goes wrong
  } else {
    mappingAlert(
      status = "error",
      message = paste0(
        "Something went wrong when mapping your metabolites, probably an ",
        "error with the database parameter."
      ),
      suggest = paste0(
        "Please check the About page for a link to our Github, where you can ",
        "report this issue."
      )
    )
  }

  return(mappedMetabolites)
}
