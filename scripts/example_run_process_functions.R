packages <- c("rstan")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}
# Lets start by collating chains for a single analysis type

# compile chains for null_model
null_model <- compile_models('null_model')

# Examine model diagnostics for all null_models
kfold_model_diagnostics(null_model)

# Extract samples for all null_models
# Extract posterior samples
null_samples <- extract_samples(null_model)

# Summarise posteriors
summ_model <- summarise_samples(null_samples)


# Now to extend the above to be applied to multiple analysis types
lotsa_models <- compile_multiple_analyses(c('null_model','no_gamma_model'))

## Diagnostics for multiple analyses
multi_analysis_kfold_diagnostics(lotsa_models)

## Extract samples for multiple analysis types
lotsa_samples <- multi_analysis_extract(lotsa_models)

## Multi Analysis samples
multi_analysis_summarise_samples(lotsa_samples)

