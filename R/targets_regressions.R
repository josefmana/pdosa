# Set-up targets for regression analyses:
targets_regressions <- list(
  targets::tar_target(
    regressions,
    command = fit_regressions(preprocessed_data, helpers)
  ),
  targets::tar_target(
    summaries,
    command = summarise_regressions(regressions)
  ),
  targets::tar_target(
    formulas,
    command = set_formulas()
  ),
  targets::tar_target(
    bayesian_regressions,
    command = fit_bayesian(preprocessed_data, formulas)
  ),
  targets::tar_target(
    prior_sensitivity,
    command = model_check(bayesian_regressions, helpers, formulas, "prior_sense")
  ),
  targets::tar_target(
    posterior_predictive_checks,
    command = model_check(bayesian_regressions, helpers, formulas, "ppc")
  ),
  targets::tar_target(
    bayesian_coefficients,
    command = extract_coefficients(bayesian_regressions)
  )
)
