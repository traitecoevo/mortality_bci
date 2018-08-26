#' Extract and load BCI canopy strata data
#' 
#' Extract and load BCI canopy strata data
#' @param path_to_zip Character. Path to zip file
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
BCI_load_canopy <- function(path_to_zip) {
  
  `%>%` <- magrittr::`%>%`
  
  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))
  
  files <- list.files(tmp, pattern=".csv", full.names=TRUE)
  dplyr::tbl_df(lapply(list.files(tmp, pattern=".csv", full.names=TRUE), function(x)
    readr::read_tsv(x, col_types=cols_only(
      x = "d",
      y = "d",
      ht0_2 = "d",
      ht0_1 = "d",
      ht1_2 = "d",
      ht2_5 = "d",
      ht5_10 = "d",
      ht10_20 = "d",
      ht20_30 = "d",
      ht30_ = "d")) %>% 
      dplyr::mutate_(year = gsub('^.*_|\\D', '', basename(x))) %>%
      dplyr::mutate_at(vars(-x,-y,-year),funs(replace(.,.==100,1)))) %>%
      dplyr::bind_rows(.)) %>%
    dplyr::select(x,y,year,ht0_1,ht0_2,ht1_2,ht2_5,ht5_10,ht10_20,ht20_30,ht30_)
}