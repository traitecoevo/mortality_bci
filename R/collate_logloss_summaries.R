#' Collate logloss summary statistics across kfolds for all model comparisons
#' 
#' Collate logloss summary statistics across kfolds for all model comparisons
#' @return Dataframe containing logloss summary statistics across kfolds for all model comparisons
#' @details This function requires all model comparisons to have been run.
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
collate_logloss_summaries <- function() {
  logloss_null_model <- summarise_crossval_logloss("null_model")
  logloss_func_growth <- summarise_crossval_logloss("function_growth_comparison")
  logloss_rho_comparisons <- summarise_crossval_logloss("rho_combinations")
  logloss_gap_comparisons <- summarise_crossval_logloss("gap_combinations")
  logloss_size_comparisons <- summarise_crossval_logloss("size_combinations")
  logloss_multi_trait_all <- summarise_crossval_logloss("multi_trait_all")
  logloss_rho_gap_all <- summarise_crossval_logloss("rho_gap_all")
  logloss_rho_size_all <- summarise_crossval_logloss("rho_size_all")
  logloss_gap_size_all <- summarise_crossval_logloss("gap_size_all")
  logloss_multi_trait_parsimony <- summarise_crossval_logloss("multi_trait_parsimony")
  logloss_re_comparison <- summarise_crossval_logloss("species_random_effects")
  
  vars <- c("growth_hazard_a", "growth_hazard_b", "growth_hazard_ab", "growth_hazard_c", "growth_hazard_ac", "growth_hazard_bc", "growth_hazard_abc")
  
  logloss_func_growth %>%
    dplyr::bind_rows(list(
      logloss_null_model,
      logloss_rho_comparisons,
      logloss_gap_comparisons, 
      logloss_size_comparisons,
      logloss_multi_trait_all,
      logloss_rho_gap_all,
      logloss_rho_size_all,
      logloss_gap_size_all,
      logloss_multi_trait_parsimony,
      logloss_re_comparison)
    ) %>%
    dplyr::arrange(comparison, growth_measure) %>%
    # remove duplicate / redundant fits
    dplyr::filter(!(
      # unnecessary model base_hazard fit
      (model == "base_hazard" & growth_measure =='true_basal_area_dt') |
        # Already included in logloss_func_growth
        (comparison =='rho_combinations'  &   rho_combo=='none') |
        (comparison =='gap_combinations'  &   gap_combo=='none') |
        (comparison =='size_combinations' &  size_combo=='none')
    )
    ) %>%
    dplyr::mutate(
      # No growth for base model
      growth_measure = base::replace(growth_measure, model %in% c("null_model", "base_hazard"), "none"), 
      # rename for plotting purposes
      model = base::replace(model, model=="base_growth_hazard_re", "base_growth_hazard")
    ) %>%
    dplyr::mutate(
      # paste together strings as identifier for model
      model_type = paste(sep='_', comparison, model,
                         ifelse(comparison == "gap_combinations", gap_combo,
                                ifelse(comparison == "size_combinations", size_combo,
                                       ifelse(comparison == "rho_combinations",rho_combo, 
                                              ifelse(comparison %in% c("function_growth_comparison","species_random_effects", "null_model"), "none",
                                                     ifelse(comparison == "multi_trait_all", "rho_gap_size_abc",
                                                            ifelse(comparison == "rho_gap_all", "rho_gap_abc",
                                                                   ifelse(comparison == "rho_size_all", "rho_size_abc",
                                                                          ifelse(comparison == "gap_size_all", "gap_size_abc", "rho_gap_c"))))))))),
      # make factors with specified order
      model = factor(model, levels=c('null_model','base_hazard','growth_hazard','base_growth_hazard')),
      growth_measure = factor(growth_measure, levels=c('none','true_basal_area_dt','true_dbh_dt')),
      model_type = factor(model_type, levels=
                            c("null_model_null_model_none",
                              sprintf("function_growth_comparison_%s", c("base_hazard_none", "growth_hazard_none", "base_growth_hazard_none")),
                              sprintf("rho_combinations_base_%s", vars),
                              sprintf("gap_combinations_base_%s", vars),
                              sprintf("size_combinations_base_%s", vars),
                              "multi_trait_all_base_growth_hazard_rho_gap_size_abc",
                              "rho_gap_all_base_growth_hazard_rho_gap_abc",
                              "rho_size_all_base_growth_hazard_rho_size_abc",
                              "rho_gap_all_base_growth_hazard_gap_size_abc",
                              "gap_size_all_base_growth_hazard_gap_size_abc",
                              "species_random_effects_base_growth_hazard_none",
                              "multi_trait_parsimony_base_growth_hazard_rho_gap_c")),
      modelid = as.factor(as.numeric(model_type))
    ) %>%
    dplyr::arrange(modelid) %>%
    dplyr::select(modelid, model_type, comparison, model,
                  growth_measure, rho_combo, gap_combo, size_combo, logloss,
                  mean, st_err, ci, `2.5%`,`97.5%`)
}