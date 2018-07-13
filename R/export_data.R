#' Multi output export function for remake
#' 
#' Multi output export function for remake
#' @param data List.
#' @param filename Character. Base filename for outputs
#' @return Exports multiple .rds files from a single target in remake
#' @details Required for remake (1 function -> n file outputs)
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
export_data <- function(data, filename) {
  filename_fmt <- sub("\\.rds$", "_%s.rds", filename)
  filename_sub <- sprintf(filename_fmt, seq_along(data))
  for (i in seq_along(data)) {
    saveRDS(data[[i]], filename_sub[[i]])
  }
  saveRDS(data, filename)
}