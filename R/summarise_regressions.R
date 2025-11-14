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
        dplyr::mutate(dplyr::across(tidyselect::all_of(c("X", "coefficient")), re_formulate))
    } else if (y %in% c("subcortical", "cognition")) {
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
          dplyr::across(tidyselect::all_of(c("X","coefficient")), re_formulate),
          sig_FDR = bh_adjust(`p value`) # re-calculate Benjamini-Hochberg adjusted significance statements
        )
    } else if (y == "mta") {
      compute_marginal_rates(fit = fit[[y]][[1]], tibble = TRUE)
    } else if (y == "cognition|hippocampi") {
      purrr::map_dfr(rlang::set_names(names(fit$`cognition|hippocampi`)), function(x) {
        lm_coeff(fit$`cognition|hippocampi`[[x]], term = glue::glue("SUBJ1:{x}")) |>
          tibble::add_column(x = x, .after = 1) |>
          dplyr::left_join(lm_dia(fit$`cognition|hippocampi`[[x]]), by = c("y", "X"))
      }) |>
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(c("X","coefficient")), re_formulate),
          sig_FDR = bh_adjust(`p value`), # re-calculate Benjamini-Hochberg adjusted significance statements
          sig_FWER = dplyr::if_else(`p value` < 0.05 / dplyr::n(), "*", "")
        )
    }
  })
}
