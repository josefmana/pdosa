#' Print Numeric
#'
#' Takes in a number and prints it with specified decimal places.
#'
#' @param x A numeric to be printed
#' @param d Decimals (defaults to 2)
#'
#' @returns A character with the number
#'
#' @export
rprint <- function(x, d = 2) {
  sprintf(paste0("%.", d, "f"), round(x, d))
}

#' Prints Mean and Standard Deviation
#'
#' Takes in a numeric vector, calculates mean and
#' standard deviation (SD) and returns these formatted.
#'
#' @param x A vector from which mean and SD ought to be calculated
#' @param d Decimals (defaults to 2)
#'
#' @exports A character with mean and SD
msd <- function(x, d = 2) {
  paste0(rprint(mean(x, na.rm = TRUE), d), " ± ", rprint(sd(x, na.rm = TRUE), d))
}

#' Get Rid of Leading Zero
#'
#' Takes in a number and prints it with specified
#' decimal places without the leading zero.
#'
#' @param x A numeric to be stripped of zero
#' @param d Decimals (defaults to 3)
#'
#' @returns A character with the number
#'
#' @export
zerolead <- function(x, d = 3) {
  ifelse(x < .001, "< .001", sub("0.", ".", rprint(x, 3), fixed = TRUE))
}

#' Print Frequency (Proportion)
#'
#' Takes in a character/factor vector, calculates
#' frequency and proportion of its groups and returns
#' these formatted.
#'
#' @param x A vector from which frequencies and proportions
#'    ought to be calculated
#' @param d Decimals (defaults to 0)
#'
#' @returns A character with frequencies and proportions
#'
#' @export
freqprop <- function(x, d = 0) {
  paste0(table(x)[2], " (", rprint(100*prop.table(table(x))[2], d = d), "%)")
}

#' Reformat Model Formula
#'
#' Takes in a model formula and changes the terms
#' to presentation-ready format.
#'
#' @param form A character containing model formula
#'
#' @returns Formated formula for presentation
#'
#' @export
re_formulate <- function(form) {
  # Prepare a data frame with changes to be made:
  map <- data.frame(
    old = c("SUBJ", "AGE", "GENDER", "AHI.F", "EDU.Y", "tmt_b", "gpt_phk", "gpt_lhk", "sigma", "BMI"),
    new = c("Group", "Age", "Sex", "OSA", "Education", "TMT-B", "GPT right", "GPT left", "\u03C3", "BMI")
  )
  # Change it
  for(i in seq_len(nrow(map))) {
    form <- gsub(map$old[i], map$new[i], form, fixed = TRUE)
  }
  form
}

#' Prepare Formulas
#'
#' Lists formulas for Bayesian models in the
#' sensitivity-to-heteroscedasticity analysis.
#'
#' @returns A list with formulas
#'
#' @export
set_formulas <- function() {
  list(
    # Formulas for base models (assuming homoscedasticity):
    varequal = lapply(rlang::set_names(c("tmt_b", "gpt_phk", "gpt_lhk")), function(i) {
      paste0(i, " ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI") |>
        as.formula() |>
        brms::bf()
    }),
    # Formulas for variance adjusted models (allowing heteroscedasticity)
    # (listing them one by one because as.formula does not want work with commas):
    heteroscedastic = list(
      tmt_b = brms::bf(tmt_b ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI, sigma ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI),
      gpt_phk = brms::bf(gpt_phk ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI, sigma ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI),
      gpt_lhk = brms::bf(gpt_lhk ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI, sigma ~ SUBJ * AHI.F + AGE + GENDER + EDU.Y + BMI)
    )
  )
}
