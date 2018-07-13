# Summarise posteriors of species level trait parameters
#' 
# Summarise posteriors of species level trait parameters
#' @param task Single row of a dataframe obtained from tasks_2_run()
#' @return Returns a .rds
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
run_job <- function(task) {
  `%>%` <- magrittr::`%>%`
  # SUB FUNCTION: Builds the model code
  make_stan_model <- function(chunks) {
    list(
      crossval = chunks$crossval,
      growth_measure = chunks$growth_measure,
      pars = chunks$pars,
      model_code = sprintf("
                           data {
                           %s
                           }
                           
                           parameters {
                           %s
                           }
                           
                           model {
                           %s
                           }
                           
                           generated quantities {
                           %s
                           }",chunks$data, chunks$parameters, chunks$model, chunks$generated_quantities)
    )
  }
  
  
  # SUB FUNCTION: Run chain 
  run_single_stan_chain <- function(model, data, chain_id, iter=4000,
                                    sample_file=NA, diagnostic_file=NA) {
    
    data_for_stan <- prep_data_for_stan(data, model$growth_measure, model$crossval)
    stan(model_code = model$model_code,
         fit = model$fit,
         data = data_for_stan,
         pars = model$pars,
         iter = iter,
         chains=1,
         seed = 12345,
         chain_id=chain_id,
         control =list(stepsize=0.1, adapt_delta=0.99, max_treedepth=15))
  }
  
  
  data <- readRDS(task$fold_data)
  dir.create(dirname(task$filename), FALSE, TRUE)
  model <- task$model
  ## Assemble the stan model:
  base::switch(model,
               "null_model"= {
                 chunks <- get_model_chunks_null(task)
                 chunks$crossval <- TRUE
               },
               "base_hazard"= {
                 chunks <- get_model_chunks_base_haz(task)
                 chunks$crossval <- TRUE
               },
               "base_hazard_re"= {
                 chunks <- get_model_chunks_base_haz_re(task)
                 chunks$crossval <- TRUE
               },
               "growth_hazard"= {
                 chunks <- get_model_chunks_growth_haz(task)
                 chunks$crossval <- TRUE
               },
               "growth_hazard_re"= {
                 chunks <- get_model_chunks_growth_haz_re(task)
                 chunks$crossval <- TRUE
               },
               "base_growth_hazard"= {
                 chunks <- get_model_chunks_base_growth_haz(task)
                 chunks$crossval <- TRUE
               },
               "base_growth_hazard_re"= {
                 chunks <- get_model_chunks_base_growth_haz_re(task)
                 chunks$crossval <- TRUE
               },
               'final_model'= {
                 chunks <- get_final_model_chunks(task)
                 chunks$crossval <- FALSE
               },
               "final_base_growth_hazard_re"= {
                 chunks <- get_model_chunks_base_growth_haz_re(task, TRUE)
                 chunks$crossval <- FALSE
               })
  
  model <- make_stan_model(chunks)
  
  filename <- task$filename %>%
    gsub(".rds", ".stan", .) %>%
    gsub("chain_fits", "stan_models", .)
  
  dir.create(dirname(filename), FALSE, TRUE)
  writeLines(model$model_code, filename)
  
  # Actually run the model
  res <- run_single_stan_chain(model, data,
                               chain_id=task$chain,
                               iter=task$iter)
  ## dump into a file.
  saveRDS(res, task$filename)
  task$filename
}
