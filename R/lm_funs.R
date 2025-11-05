#' Extracts Linear Model Coefficients
#'
#' From a linear regression fit, extracts parameter estimates
#' with their associated metrics.
#'
#' @param fit The regression model computed by \code{lm}
#'    or \code{\link[brms]{brm}}
#' @param term Regression coefficient of interest
#' @param type A character indicating whether it is
#'    a "frequentist" (default) or "Bayesian" model being
#'    evaluated
#'
#' @returns A table summarising models coefficient of interest
#'
#' @export
lm_coeff <- function(fit, term = "SUBJ1:AHI.F1", type = "frequentist") {
  # Coefficients from a frequentist model:
  if (type == "frequentist") {
    sapply(names(fit), function(y) {
      summary(fit[[y]])$coefficients[term, ] |>
        t() |>
        as.data.frame() |>
        dplyr::rename("p value" = "Pr(>|t|)") |>
        cbind(t(confint(fit[[y]])[term, ])) |>
        dplyr::relocate(`2.5 %`, .before = `t value`) |>
        dplyr::relocate(`97.5 %`, .before = `t value`) |>
        dplyr::mutate(X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))), .before = 1)
    }) |>
      t() |>
      as.data.frame() |>
      dplyr::mutate(coefficient = term, .after = X) |>
      dplyr::mutate_if(is.list, unlist) |>
      tibble::rownames_to_column("y") |>
      dplyr::mutate(
        `q value` = p.adjust(`p value`, method = "BH"),
        `s value` = -log(`p value`, base = 2),
        sig_PCER = dplyr::if_else(`p value` < .05, "*", ""),
        sig_FDR = bh_adjust(`p value`),
        sig_FWER = dplyr::if_else(`p value` < .05 / dplyr::n(), "*", "")
      )
    # Coefficients from a Bayesian model:
  } else if (type == "Bayesian") {
    sapply(names(fit), function(y) {
      brms::fixef(fit[[y]])[term, ] |>
        t() |>
        as.data.frame() |>
        dplyr::mutate(
          X = sub(".* ~ ", "", as.character(formula(fit[[y]]))[1]),
          sigma = dplyr::if_else(
            grepl("sigma", formula(fit[[y]])[2]),
            sub(")", "", sub( ".* ~ ", "", as.character(formula(fit[[y]]))[2])),
            "1"
          ),
          .before = 1
        )
    }) |>
      t() |>
      as.data.frame() |>
      dplyr::mutate(coefficient = term, .after = sigma) |>
      dplyr::mutate_if(is.list, unlist) |>
      tibble::rownames_to_column("y")
  }
}

#' Residual Model Diagnostics
#'
#' From a linear regression fit, extracts measures
#' of the Breusch-Pagan test for homoscedasticity,
#' number of outliers according to the Cook distance,
#' and the Shapiro-Wilk test for normality.
#'
#' @param fit The linear regression model computed by \code{lm}
#'
#' @returns A table summarising model diagnostics
#'
#' @export
lm_dia <- function(fit) {
  sapply(names(fit), function(y) {
    data.frame(
      X = sub(paste0(y," ~ "), "", c(formula(fit[[y]]))),
      p_breusch_pagan = c(performance::check_heteroscedasticity(fit[[y]])),
      n_cook = sum(performance::check_outliers(fit[[y]]), na.rm = TRUE),
      p_shapiro_wilk = c(performance::check_normality(fit[[y]]))
    )
  }) |>
    t() |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) unlist(x, use.names = FALSE))) |>
    dplyr::mutate(heteroscedasticity = ifelse(p_breusch_pagan < .05, "!", ""), .after = p_breusch_pagan) |>
    dplyr::mutate(nonnormality = ifelse(p_shapiro_wilk < .05, "!", ""), .after = p_shapiro_wilk) |>
    tibble::rownames_to_column("y")
}
