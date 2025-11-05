#' Summarise a Set of Regressions
#'
#' Loops through linear regression fits and
#' summarises their parameters.
#'
#' @param fit A list of linear regression models
#'
#' @returns A list with summary of linear models, one per model
#'
#' @export
summarise_regressions <- function(fit) {
  lapply(rlang::set_names(names(fit)), function(y) {
    # regression coefficients with threshold-based decisions
    # do full analysis for subcortical structures and cognition, interaction only for hippocampal substructures
    if (y == "hippocampi") {
      lm_coeff(fit[[y]], term = "SUBJ1:AHI.F1") |>
        dplyr::left_join(lm_dia(fit[[y]]), by = c("y","X")) |>
        dplyr::mutate(dplyr::across(tidyselect::all_of(c("X","coefficient")), re_formulate))
    } else {
      dplyr::left_join(
        rbind.data.frame(
          lm_coeff(fit[[y]], term = "SUBJ1"),
          lm_coeff(fit[[y]], term = "AHI.F1"),
          lm_coeff(fit[[y]], term = "SUBJ1:AHI.F1")
        ),
        lm_dia(fit[[y]]),
        by = c("y","X")
      ) |>
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(c("X","coefficient") ), re_formulate),
          sig_FDR = bh_adjust(`p value`) # re-calculate Benjamini-Hochberg adjusted significance statements
        )
    }
  })
}
