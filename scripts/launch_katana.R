# This script launches jobs on the UNSW katana cluster
# It is written to be run from the project root. 

source("scripts/pbs.R")

launch_task_i_katana <- function(...) {
	launch_task_i(..., pbs.whisker = "scripts/pbs_katana.whisker")
}

# test
launch_task_i_katana(1, "null_model", iter =10)
launch_task_i_katana(1:10, "function_growth_comparison", iter=10)

# The real deal
i <- 1:30
launch_task_i_katana(i, "null_model", iter =10)

i <- 1:180
launch_task_i_katana(1, "function_growth_comparison")

i <- 1:240
launch_task_i_katana(i, "rho_combinations")

