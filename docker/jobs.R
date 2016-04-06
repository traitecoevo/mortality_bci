# THIS CODE IS TO BE USED TO SUBMIT JOBS TO THE AWS CLUSTER OR TO LOCAL DOCKER CONTAINERS.
# PLEASE FOLLOW INSTRUCTIONS IN docker/README.md

# PHASE 1: BEST GROWTH & FUNCTIONAL FORM
func_growth_tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 10, 
                     path="/home/data")
res <- enqueue_bulk(func_growth_tasks, model_compiler, obj, progress_bar = TRUE)

# PHASE 2: BEST GROWTH & FUNCTIONAL FORM
species_re_tasks <- tasks_2_run(comparison = 'species_random_effects',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(species_re_tasks, model_compiler, obj, progress_bar = TRUE)

# PHASE 3:  BEST RHO COMBINATION
rho_combos <- tasks_2_run(comparison = 'rho_combinations',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(rho_combos, model_compiler, obj, progress_bar = TRUE)



