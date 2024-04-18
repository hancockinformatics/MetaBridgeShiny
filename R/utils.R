#' cleanReactions
#'
#' @param metabTable Data frame containing reactions
#'
#' @return Clean version of output table for download purposes
#'
cleanReactions <- function(metabTable) {
  metabTable %>% mutate(
    `Reaction Name` = stringr::str_replace_all(
      `Reaction Name`,
      c(
        "<.*?>" = "",
        "&harr;" = "<-->",
        "&rarr;" = "-->",
        "&larr;" = "<--",
        "&alpha;" = "a",
        "&beta;"  = "b",
        "&omega;" = "o",
        "&gamma;" = "g"
      )
    )
  )
}


#' listItem
#'
#' @param link Link to a website
#' @param name Name for the link
#' @param description Short description to accompany the link
#'
#' @return HTML wrapping up a dependency entry
#'
listItem <- function(link, name, description) {
  tagList(
    tags$dt(
      a(
        href = link,
        target = "_blank",
        rel = "noopener noreferrer",
        name
      )
    ),
    tags$dd(description)
  )
}



#' mappingAlert
#'
#' @param message Message to return to the user
#' @param suggest Suggestion if something goes wrong in some step
#' @param status Status of data import/mapping from other functions, namely
#'   mapGenerally()
#'
#' @return Notification with mapping information
#'
mappingAlert <- function(message, suggest, status) {
  n_type = switch(
    status,
    "error" = "error",
    "empty" = "error",
    "warn" = "warning",
    "success" = "message"
  )

  n_header <- switch(
    status,
    "error" = "Error",
    "empty" = "Error",
    "warn" = "Warning",
    "success" = "Success"
  )

  n_action <-
    if (!is.null(suggest)) {
      actionLink(inputId = "remap", label = suggest)
    } else {
      NULL
    }

  n_ui <- message

  showNotification(
    id = "mappingAlert",
    type = n_type,
    duration = ifelse(n_type == "message", 5, 10),
    ui = HTML(paste0(
      "<h4 class='alert-heading'><b>", n_header, "</b></h4>",
      "<p class='mb-0'>", n_ui, "</p>"
    )),
    action = n_action
  )
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
  if (!is.character(hmdbID) |
      !(stringr::str_detect(hmdbID, "^HMDB") |
        stringr::str_detect(hmdbID, "^hmdb"))) {
    return(NA)

    # If the ID is in the new, 7 digit format, check the leading digits
  } else if (nchar(hmdbID) == 11) {

    # If the leading characters are 00, simply trim the string
    if (stringr::str_sub(hmdbID, start = 5, end = 6) == "00") {
      newID <- paste0("HMDB", stringr::str_sub(hmdbID, start = -5, end = -1))
      return(newID)
    } else {
      return(NA)
    }
  } else if (nchar(hmdbID) == 9) {
    newID <- paste0("HMDB", stringr::str_sub(hmdbID, start = -5, end = -1))
    return(newID)
  } else {
    return(NA)
  }
}


#' notEmpty
#'
#' @param x Input vector to be cleaned
#'
#' @return Vector stripped of any empty values
#'
notEmpty <- function(x) {
  x[!grepl(x = x, pattern = "^$")]
}


#' wrapList
#'
#' @param x A tibble of dependencies to wrap up into the UI
#'
#' @return A div which splits the dependency entries into two columns
#'
wrapList <- function(x) {
  col_1 <- seq(1, ceiling(nrow(x) / 2))
  col_2 <- seq(max(col_1) + 1, nrow(x))

  tagList(
    div(
      class = "row align-items-start",
      style = "font-size: 1.1em; font-weight: 300",
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_1, ], listItem))
      ),
      div(
        class = "col",
        tags$dl(purrr::pmap(x[col_2, ], listItem))
      )
    )
  )
}
