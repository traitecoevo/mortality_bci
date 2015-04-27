
get_chunks_models_constant <- function(...) {
  list(get_chunks_model1_constant(...),
       get_chunks_model2_constant(...),
       get_chunks_model3_constant(...))
}

get_chunks_models_census_err <- function(...) {
  list(get_chunks_model1_census_err(...),
       get_chunks_model2_census_err(...),
       get_chunks_model3_census_err(...))
}
get_chunks_models_no_spp_err <- function(...) {
  list(get_chunks_model1_no_spp_err(...),
       get_chunks_model2_no_spp_err(...),
       get_chunks_model3_no_spp_err(...))
}

get_chunks_models_no_trait <- function(...) {
  list(get_chunks_model1_no_trait(...),
       get_chunks_model2_no_trait(...),
       get_chunks_model3_no_trait(...))
}

get_chunks_models_full <- function(...) {
  list(get_chunks_model1_full(...),
       get_chunks_model2_full(...),
       get_chunks_model3_full(...))
}


get_chunks_models_full_rho_combs <- function(...) {
  list(get_chunks_model_full_rho_combs(...)
  )
}

get_chunks_for_model <- function(pars) {

  models <- switch(pars$effect,
                   model_constant =  get_chunks_models_constant(),
                   model_census_err =  get_chunks_models_census_err(),
                   model_no_trait =  get_chunks_models_no_trait(),
                   model_no_spp_err = get_chunks_models_no_spp_err(),
                   model_full = get_chunks_models_full(),
                   model_full_rho_combs = get_chunks_models_full_rho_combs(pars$rho_effects_on))
  models[[pars$model]]
}

