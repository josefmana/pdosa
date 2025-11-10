#' Prepare 'Helper' Files
#'
#' This function needs to be adjusted manually
#' if desired.
#' It extract helper files that are later used to
#' control analysis flow.
#'
#' @param paths Paths to files.
#'
#' @returns A list with data frames containing different
#'    kinds of helper files.
#'
#' @export
extract_helpers <- function(paths) {
  with(paths, list(
    psychohelp = read.csv(psychvar, sep = ";"), # psychological variables
    calculator = openxlsx::read.xlsx(calculat, sheet = calc_sheet, startRow = 2),
    psych = read.csv(psychvar, sep = ";") |> dplyr::filter(!is.na(domain)), # psychological variables for PD vs CON comparisons
    subco = read.csv(subcortex, sep = ","), # subcortical structures
    hippo = read.csv(hippocampi, sep = ",") |> dplyr::filter(complete.cases(name)), # hippocampal structures
    mta = read.csv(mta, sep = ",") # Medial Temporal Atrophy
  ))
}

#' Extracts Response Times Variables
#'
#' Takes in helpers files, finds the neuropsychology helper
#' and identifies tests in domains that are based on response
#' times.
#' The function is very dataset specific!
#'
#' @param help A list containing data.frame named psych with
#'    a proper study-specific structure.
#'
#' @returns A vector of variable names.
#'
#' @export
extract_rt_variables <- function(help) {
  with(help, {
    psych |>
      dplyr::filter(domain %in% c("Attention", "Executive function", "Processing speed")) |>
      dplyr::pull(variable)
  })
}

#' Extract Coefficients of Bayesian Regressions
#'
#' From a Bayesian regression fit, extracts parameter
#' estimates with associated metrics.
#'
#' @param fit The regression model computed by \code{\link[brms]{brm}}.
#'
#' @returns A table summarising models coefficient of interest.
#'
#' @export
extract_coefficients <- function(fit) {
  purrr::map_dfr(names(fit), function(i) {
    do.call(rbind.data.frame, list(
      lm_coeff(fit[[i]], term = "SUBJ1", type = "Bayesian"),
      lm_coeff(fit[[i]], term = "AHI.F1", type = "Bayesian"),
      lm_coeff(fit[[i]], term = "SUBJ1:AHI.F1", type = "Bayesian")
    ))
  }) |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("X","sigma","coefficient")), re_formulate))
}

#' Extract Marginal Probabilities
#'
#' From a logistic regression, extracts all marginal
#' probabilities and all pairwise comparisons via
#' the \pkg{emmeans}.
#'
#' @param fit The regression model computed by \code{glm}.
#'
#' @returns A list.
#'
#' @export
extract_marginal_probabilities <- function(fit) {
  form <- glue::glue("~ {as.character(formula(fit))[3]}")
  emm <- emmeans::emmeans(fit, as.formula(form))
  marginal_means <- summary(emm, type = "response")
  contrasts <- pairs(emm, type = "response")
}
