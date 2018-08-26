# Break data into train and heldout samples
#' 
# Break data into train and heldout samples
#' @param data List. A list of data folds 
#' @return List
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
make_trainheldout <- function(data) {
  lapply(seq_along(data), function(i)
    extract_trainheldout_set(data, i))
}