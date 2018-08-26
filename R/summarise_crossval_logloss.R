#' Summarise logloss across kfolds for a single model comparison
#' 
#' Summarise logloss across kfolds for a single model comparison
#' @param comparison Character. Name of model comparison
#' @return Dataframe containing logloss summary statistics across kfolds for a single model comparison
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
summarise_crossval_logloss <- function(comparison) {
  `%>%` <- magrittr::`%>%`
  
  # SUB FUNCTION: Extract logloss samples from single model
  logloss_samples <- function(model) {
    fits <- model$fits
    info <- dplyr::bind_rows(model$model_info, .id='modelid')
    samples <- lapply(fits, function(x) 
      rstan::extract(x, pars = grep('logloss', slot(x, 'model_pars'), value=TRUE)))
    
    res <- lapply(samples, function(x) {
      tidyr::gather(data.frame(x),'logloss','estimate')}) %>% dplyr::bind_rows(., .id='modelid')
    
    dplyr::left_join(info, res, 'modelid') %>%
      dplyr::select(-modelid)
  }
  
  # SUB FUNCTION: Extract log loss samples from multiple models
  extract_logloss_samples <- function(model) {
    if(is.null(model$fits)) { #Check to see if object is multi model 
      samples <- lapply(model, logloss_samples)
      dplyr::bind_rows(samples, .id='modelid') %>%
        dplyr::select(-modelid)
      
    }
    else { 
      logloss_samples(model)
    }
  }
  
  models <- compile_models(comparison)
  samples <- extract_logloss_samples(models)
  samples %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo,gap_combo,size_combo,kfold, logloss) %>%
    dplyr::summarise(kfold_logloss = mean(estimate)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo,gap_combo,size_combo, logloss) %>%
    dplyr::summarise(log_mean = mean(log(kfold_logloss)),
                     st_err = sd(log(kfold_logloss))/sqrt(n())) %>%
    dplyr::mutate(ci = 1.96 * st_err,
                  mean = exp(log_mean),
                  `2.5%` = exp(log_mean - ci),
                  `97.5%` = exp(log_mean + ci)) %>%
    dplyr::ungroup()
}