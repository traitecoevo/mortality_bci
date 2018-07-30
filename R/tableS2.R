# Prepare table S2 for manuscript
#' 
# Prepare table S2 for manuscript
#' @param spp_params Estimated species level parameters
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
tableS2 <- function(spp_params) {
  
  x <- spp_params %>%
    dplyr::select(param,species, mean, `2.5%`,`97.5%`) %>%
    dplyr::distinct() %>%
    mutate_if(is.numeric, funs(round),4) %>%
    mutate(stat = paste0(mean," (",`2.5%`,",",`97.5%`,")")) %>%
    dplyr::select(species, param, stat) %>%
    tidyr::spread(param, stat) %>%
    dplyr::select("Species" = species, "$\\gamma$" = gamma, "$\\alpha$" = alpha, "$\\beta$" = beta)
  
  Hmisc::latex(x, file = "", booktabs = TRUE, rowname = NULL,
               colnamesTexCmd="bfseries",
               col.just = c("c",rep("l", 3)), label = "table:S2",
               where = "!h",
               longtable = TRUE,
               lines.page = 51,
               caption ="Species geometric mean ($\\pm$ 95$\\%$ credible interval) for $\\alpha$, $\\beta$ and $\\gamma$. Estimates derived from full model")
}
