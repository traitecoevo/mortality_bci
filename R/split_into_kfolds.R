#' Split data into k equally sized datasets
#' 
#' Split data into k equally sized datasets
#' @param data Dataframe.
#' @param k Integer. Number of data folds required
#' @return List of dataframe folds
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
split_into_kfolds <- function(data, k=10) {
  # make dataset an even multiple of 10
  data <- data[seq_len(floor(nrow(data) / k) * k), ]
  # execute the split
  # use an ordered vector so that all species distributed
  # approx. equally across groups
  fold <- rep(seq_len(k), nrow(data)/k)
  split(data, fold)
}
