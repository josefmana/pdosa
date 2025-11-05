#' Fit Linear Regressions
#'
#' Loops through outcomes to calculate a set of
#' linear regressions.
#'
#' @param d The data.
#' @param outcomes A character vector of the outcomes
#'   of interest.
#' @param X The right side of the linear model.
#' @param w Indicator whether regression weight shall
#'   be used (TRUE) or not(FALSE, default)
#'
#' @seealso [fit_regressions()] wraps this function.
#'
#' @returns A list with linear models, one per outcome
#'
#' @export
fit_reg <- function(d, outcomes, X = "SUBJ * AHI.F + AGE + GENDER + SBTIV", w = FALSE) {
  lapply(rlang::set_names(outcomes), function(y) {
    if (w == TRUE) {
      lm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, weights = weights)
    } else {
      lm(formula = as.formula(paste(y, X, sep = " ~ ")), data = d, weights = NULL)
    }
  })
}

#' Fit a Set of Linear Regressions
#'
#' Loops through outcomes to calculate a set of
#' linear regressions.
#'
#' @param df The data frame
#' @param help A list with helpers files prepared
#'   by \code{extract_helpers}
#'
#' @seealso [fit_reg()] is used to fit single regression lists.
#'
#' @returns A list with linear models, one per outcome
#'
#' @export
fit_regressions <- function(df, help){
  # Extract helpers:
  for (i in names(help)) {
    assign(i, help[[i]])
  }
  # Set-up formulas:
  forms <- data.frame(
    object = c("subco","hippo","psych"),
    y = c("subcortical", "hippocampi","cognition"),
    X = c(rep("SUBJ * AHI.F + AGE + GENDER + sBTIV", 2), "SUBJ * AHI.F + AGE + GENDER + EDU.Y")
  )
  # Loop through types of regressions (base vs interaction), and structures
  lapply(rlang::set_names(seq_len(nrow(forms)), forms$y), function(r) {
    with(get(forms[r, "object"]), {
      y <- forms[r, "y"] # extract outcome
      if(y == "cognition") { # extract variables
        vars <- variable
      } else {
        vars <- unique(name)
      }
      fit_reg(df, vars, X = forms[r, "X"], w = FALSE) # fit the models
    })
  })
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
