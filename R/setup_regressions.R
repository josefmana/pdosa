#' Set-up Regressions
#'
#' Prepares a set-up file for fitting regressions for
#' this project
#'
#' @param help A list with helpers files prepared
#'   by \code{extract_helpers}.
#' @param MoCA A logical indicating whether MoCA ought
#'   to be added as covariate to volume analyses (`TRUE`)
#'   or not (`FALSE`, default). A dirty trick to accommodate
#'   Reviewer 1's in NPJ comment.
#' @param addons A character with additional additive
#'   covariates to include in the brain volumes regressions.
#'   Needs to be written as `" + covariate1 + covariate2"`.
#'   Defaults to `""` adding no further covariates.
#'
#' @seealso [fit_regressions()] makes use of the outcome
#'    of this function.
#'
#' @returns A list with linear models, one per outcome.
#'
#' @export
setup_regressions <- function(help, MoCA = FALSE, addons = "") {
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
      #paste0("SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV", ifelse(MoCA, " + moca", "")),
      #paste0("SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV", ifelse(MoCA, " + moca", "")),
      paste0(ifelse(MoCA, "moca * ", ""), "SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV", addons),
      paste0(ifelse(MoCA, "moca * ", ""), "SUBJ * AHI.F + AGE + GENDER + BMI + sBTIV", addons),
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
  for (x in help$hippo$name) {
    forms <- forms |>
      tibble::add_row(
        object = "psych",
        y = "cognition",
        x = x,
        X = glue::glue("SUBJ * {x} + AGE + GENDER + EDU.Y + BMI + sBTIV"),
        model = "lm"
      )
  }
  # Return:
  forms
}
