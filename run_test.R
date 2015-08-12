# Download data for true dbh growth and save in relevant spot

# remake::make("export/bci_data.rds")

packages <- c("plyr","parallel", "rstan", "remake")
sources <- c("R/dbh_error_model.r",
             "R/true_dbh_model.r",
             "R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}


tasks <- tasks_growth(iter = 20, name = 'test')
tasks <- df_to_list(tasks[c(1,2),])
r1 <- model_compiler(tasks[[1]])
r2 <- model_compiler(tasks[[2]])

# First combine chains
model <- sflist2stanfit(c(readRDS(r1), readRDS(r2)))

# Will give you a trace plot of important parameters
to.png(
  traceplot(model, pars = c("mu_a0","sigma_log_a0","sigma_log_a1","a2",
                       "mu_b0","sigma_log_b0","sigma_log_b1","b2",
                       "mu_c0","sigma_log_c0","sigma_log_c1","c2"),ncol=3, nrow=4)
  , paste0("results/", remake:::git_sha(), ".png"))

# Check sampling diagnostics
# Want max_depth to be less than 15 and n_divergent to = 0 for all (or most iterations)
get_sampler_params(model)
