# Set-up targets for "sensitivity analyses":
targets_sensitivity <- list(
  targets::tar_target(
    setup_moca_cov,
    command = setup_regressions(helpers, MoCA = TRUE)
  ),
  targets::tar_target(
    regressions_moca_cov,
    command = fit_regressions(preprocessed_data, setup_moca_cov, helpers)
  ),
  targets::tar_target(
    summaries_moca_cov,
    command = summarise_regressions(regressions_moca_cov)
  ),
  targets::tar_target(
    bayesian_regressions,
    command = fit_bayesian(preprocessed_data, formulas),
    cue = targets::tar_cue("never") # save time
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
