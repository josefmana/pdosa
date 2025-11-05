#' Extracts Effects
#'
#' Using the \code{\link[emmeans]{emmeans}} function
#' extracts different types of effect estimates from
#' fitted linear models.
#'
#' @param fit A list of regression models computed
#'    by \code{lm} or \code{\link[brms]{brm}}
#' @param type A character indicating whether only
#'    a "moderation" (default) effects or all comparisons
#'    ("full") should be estimated
#'
#' @returns A table summarising models coefficient of interest
#'
#' @export
mass <- function(fit, type = "moderation") {
  purrr::map_dfr(names(fit), function(y) {
    # Moderation comparisons only:
    if (type == "moderation") {
      dplyr::full_join(
        emmeans::emmeans(fit[[y]], specs = pairwise ~ AHI.F | SUBJ) |>
          emmeans::contrast(interaction = "consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub( paste0(y," ~ "), "", c(formula(fit[[y]]))), term = "AHI.F", .before = 1),
        emmeans::emmeans(fit[[y]], specs = pairwise ~ AHI.F * SUBJ) |>
          emmeans::contrast(interaction = "consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub( paste0(y," ~ "), "", c(formula(fit[[y]]))), .before = 1) |>
          dplyr::rename("term" = "SUBJ_consec")
      ) |>
        as.data.frame() |>
        dplyr::rename("contrast" = "AHI.F_consec") |>
        dplyr::mutate(contrast = dplyr::if_else(term == "PD - CON", NA, contrast)) |> # for compatibility with previous versions of the script
        dplyr::mutate(y = y, .before = 1)
      # All comparisons:
    } else if (type == "full") {
      purrr::reduce(list(
        # 'main effects'
        emmeans::emmeans(fit[[y]], specs = pairwise ~ SUBJ) |>
          emmeans::contrast("consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), term = "SUBJ", .before = 1),
        emmeans::emmeans(fit[[y]], specs = pairwise ~ AHI.F) |>
          emmeans::contrast("consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), term = "AHI.F", .before = 1),
        # 'simple main effects'
        emmeans::emmeans(fit[[y]], specs = pairwise ~ AHI.F | SUBJ) |>
          emmeans::contrast("consec") |>
          tibble::as_tibble() |>
          dplyr::mutate( X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), term = "AHI.F", .before = 1),
        emmeans::emmeans(fit[[y]], specs = pairwise ~ SUBJ | AHI.F) |>
          emmeans::contrast("consec") |>
          tibble::as_tibble() |>
          dplyr::mutate( X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), term = "SUBJ", .before = 1),
        # interactions
        emmeans::emmeans(fit[[y]], specs = pairwise ~ AHI.F * SUBJ) |>
          emmeans::contrast(interaction = "consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), .before = 1 ) |>
          dplyr::rename("term" = "SUBJ_consec"),
        emmeans::emmeans(fit[[y]], specs = pairwise ~ SUBJ * AHI.F) |>
          emmeans::contrast(interaction = "consec") |>
          tibble::as_tibble() |>
          dplyr::mutate(X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), .before = 1 ) |>
          dplyr::rename("term" = "AHI.F_consec")
      ),
      dplyr::full_join
      ) |>
        dplyr::mutate(y = y, .before = 1) |>
        dplyr::select(-tidyselect::ends_with("consec"))
    }
  })
}
