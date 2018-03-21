# This script launches jobs on the NCI raijin cluster
# It is written to be run from the project root. 

source("scripts/pbs.R")

launch_task_i_raijin <- function(...) {
	launch_task_i(..., pbs.whisker = "scripts/pbs_raijin.whisker")
}

# test
# launch_task_i_raijin(1, "null_model", iter =100)
# launch_task_i_raijin(10*1:10, "function_growth_comparison", iter=10)
# launch_task_i_raijin(10*1:10, "rho_combinations", iter=10)

# new results

## Done
# i <- 1:30
# i <- 31:60
# i <-  61:100
launch_task_i_raijin(i, "gap_combinations", iter=2000)
launch_task_i_raijin(i, "size_combinations", iter=2000)

## Cleanup additional jobs
launch_task_i_raijin(10, "size_combinations", iter=2000)
launch_task_i_raijin(112, "size_combinations", iter=2000)

launch_task_i_raijin(24, "gap_combinations", iter=2000)
launch_task_i_raijin(58, "gap_combinations", iter=2000)
launch_task_i_raijin(151:156, "gap_combinations", iter=2000)


# launch_task_i_raijin(93, "size_combinations", iter=2000)
#sprintf("qdel %s", paste0(5440152:5440159, ".r-man2", collapse=" "))

