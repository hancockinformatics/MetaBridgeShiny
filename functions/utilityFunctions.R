#' notNAs
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any NA values.
#'
notNAs <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}


#' notEmpty
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any empty values.
#'
notEmpty <- function(vector) {
  vector <- vector[!grepl(x = vector, pattern = "^$")]
  return(vector)
}


#' matchHMDB
#'
#' @param hmdbID HMDB ID to be cleaned and returned
#'
#' @return Sanitized HMDB IDs which can be used in mapping.
#'
matchHMDB <- function(hmdbID) {

  # Make sure the ID is a character and starts with 'HMDB' or 'hmdb' Look at the
  # syntax very carefully here, the parentheses are IMPORTANT
  if (!is.character(hmdbID) | !(str_detect(hmdbID, "^HMDB") | str_detect(hmdbID, "^hmdb"))) {
    return(NA)

  # If the ID is in the new, 7 digit format, check the leading digits
  } else if (nchar(hmdbID) == 11) {

    # If the leading characters are 00, simply trim the string
    if (str_sub(hmdbID, start = 5, end = 6) == "00") {
      newID <- paste0("HMDB", str_sub(hmdbID, start = -5, end = -1))
      return(newID)
    } else {
      return(NA)
    }
  } else if (nchar(hmdbID) == 9) {
    newID <- paste0("HMDB", str_sub(hmdbID, start = -5, end = -1))
    return(newID)
  } else {
    return(NA)
  }
}


#' cleanReactions
#'
#' @param metabTable Data frame containing reactions
#'
#' @return Clean version of output table for download purposes
#'
cleanReactions <- function(metabTable) {
  find_replace <- c(
    "<.*?>" = "",
    "&harr;" = "<-->",
    "&rarr;" = "-->",
    "&larr;" = "<--",
    "&alpha;" = "a",
    "&beta;"  = "b",
    "&omega;" = "o",
    "&gamma;" = "g"
  )

  metabTable %>% mutate(
    `Reaction Name` = str_replace_all(`Reaction Name`, find_replace)
  )
}