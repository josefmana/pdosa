#' Set-up Regressions
#'
#' Prepares a set-up file for fitting regressions for
#' this project
#'
#' @param help A list with helpers files prepared
#'   by \code{extract_helpers}.
#'
#' @seealso [fit_regressions()] makes use of the outcome
#'    of this function.
#'
#' @returns A list with linear models, one per outcome.
#'
#' @export
setup_regressions <- function(help) {
  # Extract helpers:
  for (i in names(help)) {
    assign(i, help[[i]])
  }
  # Set-up basic formulas:
  forms <- data.frame(
    object = c(
      "subco",
      "hippo",
      "mta",
      "psych"
    ),
    y = c(
      "subcortical",
      "hippocampi",
      "mta",
      "cognition"
    ),
    x = NA,
    X = c(
      "SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV",
      "SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV",
      "SUBJ * AHI.F * GENDER",
      "SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI"
    ),
    model = c(
      "lm",
      "lm",
      "glm",
      "lm"
    )
  )
  # Add "cognition on hippocampi" models:
  for (x in hippo$name) {
    forms <- forms |>
      tibble::add_row(
        object = "psych",
        y = "cognition",
        x = x,
        X = glue::glue("SUBJ * {x} + AGE + GENDER + EDU.Y + BMI"),
        model = "lm"
      )
  }
  # Return:
  forms
}
