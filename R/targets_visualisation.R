# Set-up targets for visualisations:
targets_visualisation <- list(
  targets::tar_target(
    boxplot_brains,
    command = boxplots(
      d0 = raw_data,
      df = preprocessed_data,
      fit = regressions$subcortical,
      help = helpers,
      scl = scales,
      rt_vars = rt_variables,
      which = "brains"
    )
  ),
  targets::tar_target(
    boxplot_cognition_osa,
    command = boxplots(
      d0 = raw_data,
      df = preprocessed_data,
      fit = regressions$cognition,
      help = helpers,
      scl = scales,
      rt_vars = rt_variables,
      which = "cognition_1"
    )
  ),
  targets::tar_target(
    boxplot_cognition_pd, boxplots(
      d0 = raw_data,
      df = preprocessed_data,
      fit = regressions$cognition,
      help = helpers,
      scl = scales,
      rt_vars = rt_variables,
      which = "cognition_2"
    )
  )
)
