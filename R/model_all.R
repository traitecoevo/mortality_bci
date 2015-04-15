get_chunks <- function(model, effect) {
  models <- list()

  models$constant <- list(get_chunks_model1_constant,
                          get_chunks_model2_constant,
                          get_chunks_model3_constant)
  models$trait <- list(get_chunks_model1_trait,
                          get_chunks_model2_trait,
                          get_chunks_model3_trait)
  models$species <- list(get_chunks_model1_species,
                         get_chunks_model2_species,
                         get_chunks_model3_species)
  models$trait_species <- list(get_chunks_model1_trait_species,
                               get_chunks_model2_trait_species,
                               get_chunks_model3_trait_species)
  models[[effect]][[model]]()
}
