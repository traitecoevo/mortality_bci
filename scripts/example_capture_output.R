
library(remake)
create_bindings()

# run stan model, diverting everything to file
x <- capture_output2(
	run_single_stan_chain(stan_model3_constant, stan_data, chain_id=1, iter=10),
	name = "ignore/trial/temp", divert_messages=TRUE)

# compare saved output to that returned by function
all.equal(x,readRDS("ignore/trial/temp.rds"))
