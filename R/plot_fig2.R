# Plot figure 2 of manuscript
#' 
# Plot figure 2 of manuscript
#' @param logloss_summaries Dataframe obtained from collate_logloss_summaries()
#' @return Figure 2 of manuscript
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

plot_fig2 <- function(logloss_summaries) {
  
  `%>%` <- magrittr::`%>%`
  
  dat <- logloss_summaries %>%
    dplyr::filter(model_type %in% c("rho_combinations_base_growth_hazard_abc", 
                                    "gap_combinations_base_growth_hazard_abc", 
                                    "size_combinations_base_growth_hazard_abc") |
                    comparison %in% c("null_model",
                                      "function_growth_comparison",
                                      "species_random_effects", 
                                      "multi_trait_all",
                                      "rho_gap_all",
                                      "rho_size_all",
                                      "gap_size_all")) %>%
    dplyr::mutate(comparison = replace(comparison, comparison %in% c("rho_combinations", 
                                                                     "gap_combinations", 
                                                                     "size_combinations"), "1_trait"),
                  comparison = replace(comparison, comparison %in% c("rho_gap_all",
                                                                     "rho_size_all",
                                                                     "gap_size_all"), "2_traits"),
                  comparison = replace(comparison, comparison == "multi_trait_all", "3_traits"),
                  comparison = replace(comparison, comparison =="function_growth_comparison" & model=="base_hazard", "null_model"),
                  stage = factor(comparison, 
                                 levels=c("null_model","function_growth_comparison","1_trait","2_traits", 
                                          "3_traits","species_random_effects"),
                                 labels = c("bold(Stage~1)","bold(Stage~2)","bold(Stage~3)", 
                                            "bold(Stage~4)", "bold(Stage~5)","bold(Stage~6)")),
                  comparison = factor(comparison, 
                                      levels=c("null_model","function_growth_comparison","1_trait","2_traits", 
                                               "3_traits","species_random_effects"),
                                      labels = c("Baseline","Growth~rate","1~trait~(alpha~beta~gamma)", 
                                                 "2~traits~(alpha~beta~gamma)", "3~traits~(alpha~beta~gamma)","Species")),
                  model_type = factor(model_type, levels = c("null_model_null_model_none",
                                                             "function_growth_comparison_base_hazard_none",
                                                             "function_growth_comparison_growth_hazard_none",
                                                             "function_growth_comparison_base_growth_hazard_none",
                                                             "size_combinations_base_growth_hazard_abc",
                                                             "rho_combinations_base_growth_hazard_abc",
                                                             "gap_combinations_base_growth_hazard_abc",
                                                             "rho_size_all_base_growth_hazard_rho_size_abc",
                                                             "rho_gap_all_base_growth_hazard_rho_gap_abc",
                                                             "gap_size_all_base_growth_hazard_gap_size_abc",
                                                             "multi_trait_all_base_growth_hazard_rho_gap_size_abc",
                                                             "species_random_effects_base_growth_hazard_none")))
  
  ggplot2::ggplot(dat, ggplot2::aes(x = model_type,y =mean, group = growth_measure, fill=growth_measure, shape = model)) + 
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax=`97.5%`), position=ggplot2::position_dodge(0.2), stroke = 0.5, size=0.4) +
    ggplot2::ylab('Logarithmic loss') +
    ggplot2::xlab('Hazard function') +
    ggplot2::scale_shape_manual(values = c(21,21, 24, 22)) +
    ggplot2::scale_fill_manual(values =c('white','grey80','black')) +
    ggplot2::scale_y_continuous(breaks= scales::pretty_breaks(5)) +
    ggplot2::scale_x_discrete(labels=c("null_model_null_model_none" = expression(gamma),
                                       "function_growth_comparison_base_hazard_none" = expression(gamma~delta["t"]),
                                       "function_growth_comparison_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]})~delta["t"]),
                                       "function_growth_comparison_base_growth_hazard_none" = expression((alpha*"e"^{-beta~"X"["i"]} + gamma)~delta["t"]),
                                       "size_combinations_base_growth_hazard_abc" = expression("Max dbh"~(psi)),
                                       "rho_combinations_base_growth_hazard_abc" = expression("Wood density"~(rho)),
                                       "gap_combinations_base_growth_hazard_abc" = expression("Light index"~(upsilon)),
                                       "rho_size_all_base_growth_hazard_rho_size_abc" = expression(rho + psi),
                                       "rho_gap_all_base_growth_hazard_rho_gap_abc" = expression(rho + upsilon),
                                       "gap_size_all_base_growth_hazard_gap_size_abc" = expression(psi + upsilon),
                                       "multi_trait_all_base_growth_hazard_rho_gap_size_abc" = expression(psi + rho + upsilon),
                                       "species_random_effects_base_growth_hazard_none" = expression((alpha[s]*"e"^{-beta[s]~"X"["i"]} + gamma[s])~delta["t"]))) +
    ggplot2::facet_grid(.~stage + comparison, scales='free_x', drop=TRUE, space = "free_x", labeller = labeller(comparison = ggplot2::label_parsed, stage = ggplot2::label_parsed)) +
    plot_theme(strips = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=15, hjust = 1),
                   panel.spacing = unit(.5, 'pt'),
                   strip.text.x = ggplot2::element_text(size=4),
                   strip.background=element_rect(colour ="lightgrey"))
}