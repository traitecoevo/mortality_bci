#' Extract times taken for chain to complete for a given model comparison
#' 
#' Extract times taken for chain to complete for a given model comparison
#' @param models List object obtained from compile_models().
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
get_chain_times <- function(models) {
  fits <- models$fits
  info <- dplyr::bind_rows(models$model_info, .id='modelid')
  times <- lapply(fits, function(x)
    rstan::get_elapsed_time(x))
  
  res <- lapply(times, function(x) {
    tidyr::gather(data.frame(x),'warmup','sample')}) %>% dplyr::bind_rows(., .id='modelid')
  
  left_join(info, res, 'modelid') %>%
    dplyr::select(-modelid) %>%
    dplyr::mutate(total_hours = ((warmup + sample)/3600))
}