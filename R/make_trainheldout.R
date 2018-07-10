#' Split data into training and testing datasets
#' 
#' Split data into training and testing datasets
#' @param data List. List of dataframes
#' @param k Integer. Number of data folds required. If NA all data will be used for training
#' @return List of training and heldout datasets
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
make_trainheldout <- function(data, k=NA) {
  
  # SUB FUNCTION: Break up into train and heldout samples
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
  
  lapply(seq_along(data), function(i)
    extract_trainheldout_set(data, i))
}