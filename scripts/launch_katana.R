# This script launches jobs on the UNSW katana cluster
# It is written to be run from the project root.

source("scripts/pbs.R")

launch_task_i_katana <- function(...) {
	launch_task_i(..., pbs.whisker = "scripts/pbs_katana.whisker")
}

# test
# launch_task_i_katana(1, "null_model", iter =10)
# launch_task_i_katana(1:10, "function_growth_comparison", iter=10)

# Launch final model
launch_task_i_katana(1:3, "final_model", iter=2000)


# Multitrait models
launch_task_i_katana(1:29, "multi_trait_all", iter=2000)
launch_task_i_katana(1:29, "multi_trait_parsimony", iter=2000)

# Extra models
# launch_task_i_katana(24, "gap_combinations", iter=2000)
# launch_task_i_katana(58, "gap_combinations", iter=2000)


## Done
# launch_task_i_katana(1:30, "null_model", iter =10)
# launch_task_i_katana(1:180, "function_growth_comparison")
# launch_task_i_katana(1:240, "rho_combinations")

# launch_task_i_katana(101:119, "gap_combinations", iter=2000)
# launch_task_i_katana(101:119, "size_combinations", iter=2000)
# launch_task_i_katana(120:179, "gap_combinations", iter=2000)
# launch_task_i_katana(120:179, "size_combinations", iter=2000)
# launch_task_i_katana(180:240, "gap_combinations", iter=2000)
# launch_task_i_katana(180:240, "size_combinations", iter=2000)
# launch_task_i_katana(100:179, "gap_combinations", iter=2000)
# launch_task_i_katana(100:179, "size_combinations", iter=2000)

# First queued on raijin but didn't run so run on katana
# launch_task_i_katana(c(45, 46, 62:99), "gap_combinations", iter=2000)
# launch_task_i_katana(51:84, "size_combinations", iter=2000)
