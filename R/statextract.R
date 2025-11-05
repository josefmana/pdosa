#' Extracts and formats test statistics
#'
#' Takes in results of a regression analysis and extracts
#' regression coefficients with accompanying test statistics
#' and p-values.
#'
#' @param coeffs Regression coefficients
#' @param y Outcome of interest
#' @param stat A character indicating whether t-values
#'    ("t", default) or z-values ("z") should be looked for.
#'
#' @returns A tibble containing regression coefficients
#'    together with test statistics and p-values associated
#'    with them.
#'
#' @export
statextract <- function(coeffs, y, stat = "t") {
  coeffs |>
    as.data.frame() |>
    dplyr::mutate(y = y, .before = 1) |>
    tibble::rownames_to_column("coefficient") |>
    dplyr::mutate(
      out = paste0(
        stat," = ", rprint(get(paste0(stat, " value")), 2),
        ", p ",
        dplyr::if_else(
          get(paste0("Pr(>|", stat, "|)")) < .001,
          zerolead(get(paste0("Pr(>|", stat, "|)"))),
          paste0("= ", zerolead(get(paste0("Pr(>|", stat, "|)"))))
        )
      )
    ) |>
    dplyr::select(-paste0(stat, " value"), -paste0("Pr(>|", stat, "|)") ) |>
    tidyr::pivot_wider(names_from = coefficient, values_from = out) |>
    dplyr::select(-`(Intercept)`)
}
