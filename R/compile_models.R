#' Compile chains for multiple model comparisons
#' 
#' Compile chains for multiple model comparisons
#' @param comparison Character. Vector of model comparisons
#' @return Stan model output
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
compile_models <- function(comparison) {

  # Sub function for combining chains for a given model
  combine_stan_chains <- function(files) {
    rstan::sflist2stanfit(lapply(files, readRDS))
  }
  
  # Sub-function for compiling models for a single comparison
  compile_chains <- function(comparison) {
    
    tasks <- tasks_2_run(comparison)
    sets <- base::split(tasks,  list(tasks$comparison,tasks$model,tasks$growth_measure, tasks$rho_combo, tasks$gap_combo, tasks$size_combo, tasks$kfold), sep='_', drop=TRUE)
    pars <- lapply(sets,  function(s) s[1, c("comparison", "model", "growth_measure", "rho_combo", "gap_combo", "size_combo", "kfold")])
    fits <- lapply(sets, function(s) combine_stan_chains(s[['filename']]))
    list(model_info=pars, fits=fits)
  }
  

  # Function for compiling chains across single or multiple comparisons
  if(length(comparison) == 1) {
    compile_chains(comparison)
  }
  else {
    sapply(comparison, function(x) compile_chains(x), simplify = FALSE)
  }
}