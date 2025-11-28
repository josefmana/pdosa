#' Fit Linear Regressions
#'
#' Loops through outcomes to calculate a set of
#' linear regressions.
#'
#' @param d The data.
#' @param outcomes A character vector of the outcomes
#'   of interest.
#' @param X The right side of the linear model.
#' @param w Indicator whether regression weights shall
#'   be used (`TRUE`) or not (`FALSE`, default)
#'
#' @seealso [fit_regressions()] wraps this function.
#'
#' @returns A list with linear models, one per outcome.
#'
#' @export
fit_lm <- function(d, outcomes, X = "SUBJ * AHI.F + AGE + GENDER + SBTIV", w = FALSE) {
  lapply(rlang::set_names(outcomes), function(y) {
    if (w == TRUE) {
      lm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, weights = weights)
    } else {
      lm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, weights = NULL)
    }
  })
}

#' Fit Logistic Regressions
#'
#' Loops through outcomes to calculate a set of
#' logistic regressions.
#'
#' @param d The data.
#' @param outcomes A character vector of the outcomes
#'   of interest.
#' @param X The right side of the linear model.
#' @param w Indicator whether regression weights shall
#'   be used (`TRUE`) or not (`FALSE`, default)
#'
#' @seealso [fit_regressions()] wraps this function.
#'
#' @returns A list with logistic regression models,
#'    one per outcome
#'
#' @export
fit_glm <- function(d, outcomes, X = "SUBJ * AHI.F * GENDER", w = FALSE) {
  lapply(rlang::set_names(outcomes), function(y) {
    if (w == TRUE) {
      glm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, family = binomial(), weights = weights)
    } else {
      glm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, family = binomial(), weights = NULL)
    }
  })
}

#' Fit a Set of Linear Regressions
#'
#' Loops through outcomes to calculate a set of
#' linear regressions.
#'
#' @param df The data frame.
#' @param setup A set-up file as prepared by
#'    \code{setup_regressions}.
#' @param help A list with helpers files prepared
#'    by \code{extract_helpers}.
#'
#' @seealso [fit_lm()] is used to fit single regression lists.
#'
#' @returns A list with linear models, one per outcome
#'
#' @export
fit_regressions <- function(df, setup, help){
  # Extract helpers:
  for (i in names(help)) {
    assign(i, help[[i]])
  }
  # Read the formulas set-up:
  forms <- setup |> dplyr::filter(is.na(x)) # the main analyses
  forms2 <- setup |> dplyr::filter(!is.na(x)) # regressing cognition on hippocampi
  # Loop through types of regressions (base vs interaction), and structures
  fits <- lapply(rlang::set_names(seq_len(nrow(forms)), forms$y), function(r) {
    with(get(forms[r, "object"]), {
      y <- forms[r, "y"] # extract outcome
      if (y == "cognition") { # extract variables
        vars <- variable
      } else {
        vars <- unique(name)
      }
      if (forms[r, "model"] == "lm") { # fit the models
        fit_lm(df, vars, X = forms[r, "X"], w = FALSE)
      } else if (forms[r, "model"] == "glm") {
        fit_glm(df, vars, X = forms[r, "X"], w = FALSE)
      }
    })
  })
  # Add 'cognition on hippocampi' regressions:
  fits$`cognition|hippocampi` <- lapply(rlang::set_names(seq_len(nrow(forms2)), forms2$x), function(r) {
    with(get(forms2[r, "object"]), {
      y <- forms[r, "y"]
      vars <- variable
      fit_lm(df, vars, X = forms2[r, "X"], w = FALSE)
    })
  })
  # Return the fits:
  fits
}

#' Fit Bayesian Regressions
#'
#' Loops through outcomes to calculate a set of
#' Bayesian linear regressions.
#'
#' @param df The data.
#' @param formulas A two-layered list with formulas for
#'    Bayesian regression.
#'
#' @returns A list with regressions, one per outcome.
#'
#' @export
fit_bayesian <- function(df, formulas) {
  # Re-fit the models for TMT-B and GPT via brms with
  # differing variance for the groups:
  priors <- list(
    # Priors for base models (assuming homoscedasticity):
    varequal = c(
      brms::prior(normal(0, 1), class = Intercept),
      brms::prior(normal(0, 1), class = b),
      brms::prior(exponential(1), class = sigma)
    ),
    # Priors for variance adjusted models (allowing heteroscedasticity):
    heteroscedastic = c(
      brms::prior(normal(0, 1), class = Intercept),
      brms::prior(normal(0, 1), class = b),
      brms::prior(normal(0, 1), class = Intercept, dpar = sigma),
      brms::prior(normal(0, 1), class = b, dpar = sigma)
    )
  )
  # Fit the models:
  lapply(rlang::set_names(names(formulas)), function(i) {
    lapply(rlang::set_names(names(formulas[[i]])), function(j) {
      brms::brm(
        formula = formulas[[i]][[j]],
        data = df,
        family = gaussian(link = "identity"),
        prior = priors[[i]],
        seed = 87542
      )
    })
  })
}
