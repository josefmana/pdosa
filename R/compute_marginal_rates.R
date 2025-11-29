#' Computes Marginal Probabilities
#'
#' Given a logistic regression model, extracts all marginal
#' mean probabilities of the outcome as well as all pairwise
#' comparisons and associated p-values without adjustment
#' for multiple comparisons.
#'
#' @param fit The regression model computed by \code{glm}
#'    with `binomial()` family.
#' @param tibble Logical. Should the resulting marginal means
#'    and comparisons be returned as tibbles (`TRUE`, default)
#'    or as raw {emmeans} tables (`FALSE`)?
#'
#' @returns A list
#'
#' @export
compute_marginal_rates <- function(fit, tibble = TRUE) {
  # Extract formula:
  form <- glue::glue("~ {as.character(formula(fit))[3]}")
  # Extract marginal means and pairwise comparisons:
  purrr::map(list(latent = NULL, reponse = "response"), function(i) {
    emm0 <- emmeans::emmeans(fit, ~ SUBJ * AHI.F, type = i)
    emm1 <- emmeans::emmeans(fit, as.formula(form), type = i)
    out <- list(
      two_way = list(means = summary(emm0), contrasts = pairs(emm0, adjust = "none")),
      full = list(means = summary(emm1), contrasts = pairs(emm1, adjust = "none"))
    )
    if (tibble) {
      out <- list(
        two_way = lapply(out$two_way, \(i) tibble::as_tibble(i)),
        full = lapply(out$full, \(i) tibble::as_tibble(i))
      )
    }
    if (form == "~ SUBJ * AHI.F") {
      out <- out$two_way
    }
    out
  })
}
