folder <-'results/medianscale_halfnormdbh_delta/'
mod123 <- sflist2stanfit(c(readRDS(paste0(folder,'1.rds')),
                          readRDS(paste0(folder,'2.rds')),
                          readRDS(paste0(folder,'3.rds'))))

mod456 <- sflist2stanfit(c(readRDS(paste0(folder,'4.rds')),
                           readRDS(paste0(folder,'5.rds')),
                           readRDS(paste0(folder,'6.rds'))))
                        

mod789 <- sflist2stanfit(c(readRDS(paste0(folder,'7.rds')),
                           #readRDS(paste0(folder,'8.rds')),
                           readRDS(paste0(folder,'9.rds'))))

mod101112 <- sflist2stanfit(c(readRDS(paste0(folder,'10.rds')),
                             readRDS(paste0(folder,'11.rds')),
                             readRDS(paste0(folder,'12.rds'))))

                        # Will give you a traceplot of important parameters                        
                        traceplot(mod, pars = c("mu_log_a0","mu_log_b0", "mu_log_c0"),ncol=3, nrow=4)
                        
                        #Check sampling diagnostics
                        # Want max_depth to be less than 15 and n_divergent to = 0 for all (or most iterations)
                        get_sampler_params(model)


print(mod123,  pars = c("mu_log_a0","sigma_log_a0","mu_log_a1","sigma_log_a1","a2",
                          "mu_log_b0","sigma_log_b0","mu_log_b1","sigma_log_b1","b2",
                          "mu_log_c0","sigma_log_c0","c2","mu_log_c1","sigma_log_c1","lp__"))
