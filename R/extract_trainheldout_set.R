# Break data into train and heldout samples
#' 
# Break data into train and heldout samples
#' @param data List. 
#' @param k Integer. The number of folds. If NA data all data will be used as training
#' @return Dataframe or list of dataframes
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

# Break up into train and heldout samples
extract_trainheldout_set <- function(data, k=NA) {
  # by default train on whole dataset
  i_train <- seq_len(length(data))
  if (is.na(k)) {
    i_heldout <- NA
    res <- dplyr::bind_rows(data[i_train])
  } else {
    i_train <- dplyr::setdiff(i_train, k)
    i_heldout <- k
    
    res <- list(
      train = dplyr::bind_rows(data[i_train]),
      heldout  = dplyr::bind_rows(data[i_heldout]))
  }
  return(res)
}