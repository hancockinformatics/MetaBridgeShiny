#' mappingAlert
#'
#' @param message Message to return to the user
#' @param suggest Suggestion if something goes wrong in some step
#' @param status Status of data import/mapping as defined by other functions
#'   (e.g. mapGenerally())
#'
#' @return UI elements to be rendered as needed.
#'
mappingAlert <- function(message, suggest, status) {
  insertUI(
    selector = "#mapPanelSidebar",
    where = "beforeEnd",
    ui = tags$div(
      id = "mappingAlert",
      class = if (status == "error" | status == "empty") {
        "alert alert-dismissible alert-danger"
      } else if (status == "warn") {
        "alert alert-dismissible alert-warning"
      } else if (status == "success") {
        "alert alert-dismissible alert-success"
      },

      tags$button(
        HTML("&times;"),
        type = "button",
        class = "close",
        `data-dismiss` = "alert"
      ),

      message,

      if (!is.null(suggest)) {
        actionLink(
          inputId = "remap",
          label = suggest
        )
      },

      if (status == "warn") {
        paste0(
          "<p>Please submit an issue at the",
          "<a href='https://github.com/hancockinformatics/MetaBridgeShiny' ",
          "style='color: white;'>Github page.</a></p>"
        )
      }
    )
  )
}
