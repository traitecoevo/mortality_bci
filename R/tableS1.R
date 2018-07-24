# Prepare table S1 for manuscript
#' 
# Prepare table S1 for manuscript
#' @param model List containing stan fit object
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
tableS1 <- function(model) {
  
  param1 <- base::as.data.frame(
    rstan::summary(model$fits[[1]],
                   c('mu_log_alpha',
                     "mu_log_beta",
                     "mu_log_gamma"))$summary) %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, mean, `2.5%`, `97.5%`) %>%
    dplyr::mutate_at(vars(-Parameter), dplyr::funs(exp))
  
  param2 <- base::as.data.frame(rstan::summary(model$fits[[1]], c('a1','a2','a3',
                                                                  'b1','b2','b3',
                                                                  'c1','c2','c3'))$summary) %>%
                                  dplyr::mutate(Parameter = row.names(.)) %>%
                                  dplyr::select(Parameter, mean, `2.5%`, `97.5%`)
  
  param3 <- base::as.data.frame(rstan::summary(model$fits[[1]], 'census_err')$summary) %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, mean, `2.5%`, `97.5%`)
  
  x <- dplyr::bind_rows(param1,param2, param3) %>%
    dplyr::select(Parameter, "Geometric mean" = mean, `2.5%`, `97.5%`) %>%
    dplyr::mutate_at(dplyr::vars(-Parameter), dplyr::funs(round(.,4))) %>%
    dplyr::mutate(Parameter = factor(Parameter,
                                     levels = c("census_err[3]",
                                                "census_err[2]",
                                                "census_err[1]",
                                                "a1",
                                                "a2",
                                                "a3",
                                                "b1",
                                                "b2",
                                                "b3",
                                                "c1",
                                                "c2",
                                                "c3",
                                                "mu_log_gamma",
                                                "mu_log_beta",
                                                "mu_log_alpha"),
                                     labels = c("Census 3 effect ($\\delta_3$)",
                                                "Census 2 effect ($\\delta_2$)",
                                                "Census 1 effect ($\\delta_1$)",
                                                "Wood density effect on $\\alpha$ ($\\alpha_1$)",
                                                "Light demand effect on $\\alpha$ ($\\alpha_2$)",
                                                "Maximum dbh effect on $\\alpha$ ($\\alpha_3$)",
                                                "Wood density effect on $\\beta$ ($\\beta_1$)",
                                                "Light demand effect on $\\beta$ ($\\beta_2$)",
                                                "Maximum dbh effect on $\\beta$ ($\\beta_3$)",
                                                "Wood density effect on $\\gamma$ ($\\gamma_1$)",
                                                "Light demand effect on $\\gamma$ ($\\gamma_2$)",
                                                "Maximum dbh effect on $\\gamma$ ($\\gamma_3$)",
                                                "Mean $\\gamma$ ($\\bar{\\gamma}$)",
                                                "Mean $\\beta$ ($\\bar{\\beta}$)",
                                                "Mean $\\alpha$ ($\\bar{\\alpha}$)")))
  
  Hmisc::latex(x, file = "", booktabs = TRUE, rowname = NULL,
               colnamesTexCmd="bfseries",
               col.just = c(rep("c", 4)), label = "table:1",
               where = "!h",
               caption ="Mean hyper-parameters and coefficients for both species trait and census effects as estimated from full model")
}
