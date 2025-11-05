#' Prepare 'Helper' Files
#'
#' This function takes no parameter but needs to be
#' adjusted manually if desired.
#' It is used in the \pkg{targets} pipeline to check
#' for changes in files that do not contain data proper
#' but 'helper' data, i.e., data mapping raw data
#' structures to functions used throughout the pipeline.
#'
#' @returns A list with data frames containing different
#'    kinds of helper files.
#'
#' @export
extract_helpers <- function() {
  with(data.frame(
    psychvar = here::here("helpers", "psychs.csv"),
    calculat = here::here("data-raw", "calculator_final_v7_c_301116.xlsx"),
    calc_sheet = "equations",
    subcortex = here::here("helpers", "subcortical.csv"),
    hippocampi = here::here("helpers", "hippocampus.csv")
  ),
  list(
    psychohelp = read.csv(psychvar, sep = ";"), # psychological variables
    calculator = openxlsx::read.xlsx(calculat, sheet = calc_sheet, startRow = 2),
    psych = read.csv(psychvar, sep = ";") |> dplyr::filter(!is.na(domain)), # psychological variables for PD vs CON comparisons
    subco = read.csv(subcortex, sep = ","), # subcortical structures
    hippo = read.csv(hippocampi, sep = ",") |> dplyr::filter(complete.cases(name)) # hippocampal structures
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

#' Extracts Coefficients of Bayesian Regressions
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

