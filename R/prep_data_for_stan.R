# Prepare data for stan
#' 
# Prepare data for stan
#' @param data Dataframe.
#' @param growth_measure Character. Can be either "true_dbh_dt" or "true_basal_area_dt"
#' @param crossval Logical. Whether to subdivide data into folds.
#' @return List
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
prep_data_for_stan <- function(data, growth_measure, crossval) {
  
  if(!growth_measure %in% c("true_dbh_dt", "true_basal_area_dt")) {
    stop("growth_measure can only be 'true_dbh_dt' or 'true_basal_area_dt'")
  }
  
  ### SUB_FUNCTION Prepares kfold data for use with stan
  prep_kfold_data_for_stan <- function(data, growth_measure) {
    base::switch(growth_measure,
                 'true_dbh_dt' = {
                   growth_dt <- data$train$true_dbh_dt - 0.172
                   growth_dt_heldout = data$heldout[[growth_measure]] - 0.172
                 },
                 'true_basal_area_dt' = {
                   growth_dt <- data$train$true_basal_area_dt - 0.338
                   growth_dt_heldout = data$heldout$true_basal_area_dt - 0.338
                 })
    
    # find index for first record of each species in each dataset
    # use this below to access traits records for each species
    i <- match(unique(data$train$sp_id), data$train$sp_id)
    h <- match(unique(data$heldout$sp_id), data$heldout$sp_id)
    
    list(
      n_obs = nrow(data$train),
      n_census = max(data$train$censusid),
      n_spp = max(data$train$sp_id),
      census = data$train$censusid,
      spp = data$train$sp_id,
      census_length = data$train$census_interval,
      growth_dt = growth_dt,
      rho_c  = data$train$rho[i]/0.6,
      gap_index_c  = data$train$gap_index[i]/0.7,
      dbh_95_c  = data$train$dbh_95[i]/15,
      y = as.integer(data$train$dead_next_census),
      n_obs_heldout = nrow(data$heldout),
      n_census_heldout = max(data$heldout$censusid),
      n_spp_heldout = max(data$heldout$sp_id),
      census_heldout = data$heldout$censusid,
      spp_heldout = data$heldout$sp_id,
      census_length_heldout = data$heldout$census_interval,
      growth_dt_heldout = growth_dt_heldout,
      rho_c_heldout = data$heldout$rho[h]/0.6,
      gap_index_c_heldout  = data$heldout$gap_index[h]/0.7,
      dbh_95_c_heldout  = data$heldout$dbh_95[h]/15,
      y_heldout = as.integer(data$heldout$dead_next_census)
    )
  }
  
  ### SUB FUNCTION 
  # Prepares full dataset for use with stan
  prep_full_data_for_stan <- function(data, growth_measure) {
    
    base::switch(growth_measure,
                 'true_dbh_dt' = {
                   growth_dt <- data$train$true_dbh_dt - 0.172
                 },
                 'true_basal_area_dt' = {
                   growth_dt <- data$train$true_basal_area_dt - 0.338
                 })
    # find index for first record of each species in each dataset
    # use this below to access traits records for each species
    i <- base::match(unique(data$sp_id), data$sp_id)
    
    list(
      n_obs = nrow(data),
      n_census = max(data$censusid),
      n_spp = max(data$sp_id),
      census = data$censusid,
      spp = data$sp_id,
      census_length = data$census_interval,
      growth_dt = growth_dt,
      rho_c  = data$rho[i]/0.6,
      gap_index_c  = data$gap_index[i]/0.7,
      dbh_95_c  = data$dbh_95[i]/15,
      y = as.integer(data$dead_next_census),
      species = data$species[i],
      sp = data$sp[i],
      raw_rho = data$rho[i],
      raw_gap_index = data$gap_index[i],
      raw_dbh_95 = data$dbh_95[i])
  }
  
  if(crossval==TRUE) {
    prep_kfold_data_for_stan(data, growth_measure)
  }
  else {
    prep_full_data_for_stan(data, growth_measure)
  }
}