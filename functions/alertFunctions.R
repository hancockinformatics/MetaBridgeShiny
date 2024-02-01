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
    duration = 20,
    action = n_action,
    n_ui
  )
}
