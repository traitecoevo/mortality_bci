#' Extract model diagnostics summary for single or multiple model comparisons
#' 
#' Extract model diagnostics summary for single or multiple model comparisons
#' @param comparison Character. Vector of model comparisons
#' @return Dataframe containing estimated true growth rate and true dbh's
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
model_diagnostics <- function(comparison) {
  
  `%>%` <- magrittr::`%>%`
  # Sub function extracting model diagnostics for single model
  diagnose <- function(model) {
    fits <- model$fits
    info <- model$model_info
    out1 <- dplyr::bind_rows(lapply(fits, function(x) {
      summary_model <- summary(x)$summary
      sampler_params <- get_sampler_params(x, inc_warmup=FALSE)
      data.frame(
        min_n_eff = min(summary_model[, 'n_eff']),
        max_rhat = max(summary_model[, 'Rhat']),
        n_bad_rhat = length(which(summary_model[, 'Rhat'] > 1.1)),
        n_divergent = sum(sapply(sampler_params, function(y) y[,'divergent__'])),
        max_treedepth = max(sapply(sampler_params, function(y) y[,'treedepth__'])))
    }))
    
    out2 <- suppressWarnings(bind_rows(lapply(info, function(x) {
      data.frame(
        comparison = x$comparison,
        model = x$model,
        growth_measure = x$growth_measure,
        rho_combo = x$rho_combo,
        gap_combo = x$gap_combo,
        size_combo = x$size_combo,
        kfold = as.integer(x$kfold))
    })))
    
    res <- cbind(out2,out1) %>%
      dplyr::arrange(comparison,model,growth_measure,rho_combo, gap_combo, size_combo, kfold)
    
    row.names(res) <- NULL
    return(res)
  }
  
  model <- compile_models(comparison)
  if(is.null(model$fits)) { #Check to see if object is multi model 
    out <- suppressWarnings(dplyr::bind_rows(lapply(model, function(x) {
      diagnose(x)})))
    row.names(out) <- NULL
  }
  else {
    out <- diagnose(model)
  }
  return(out)
}

