
library(stringr)
library(dplyr)
files <- dir("/Users/dfalster/Downloads/mortality_bci/results/pbs/rho_combinations", pattern = "output*", full.names=TRUE)
read_file<- function(filename) {
  f <- readLines(filename)
  s <- f[grepl("Total)", f, fixed=TRUE)] %>% str_trim() %>% str_split(.," ") %>% unlist()
  s[[1]][1]
}

times <- tibble( gsub("/Users/dfalster/Downloads/mortality_bci/results/pbs/rho_combinations/output_", "", files) %>% as.numeric(),
                 time =(lapply(files, read_file) %>% na_if("NULL") %>% unlist() %>% as.numeric()) / 60/60)

data <- left_join(
            remake::make("rho_tasks") %>% as_tibble(),
            times, by = "jobid") %>%
      dplyr::select(rho_combo, jobid, kfold, time) %>%
      mutate(
            rho_combo = factor(rho_combo, levels = c("none", "a", "b", "c","ab", "ac", "bc", "abc")),
            kfold = as.character(kfold)
            )


ggplot(data, aes(rho_combo, time)) +
  geom_boxplot(colour="grey") +
  geom_point(aes(colour = kfold)) +
  labs( x = "Model variant (rho combinations)", y = "time per job (hrs)")


(tot <- data %>% filter(rho_combo != "none") %>% pull(time) %>% sum(na.rm=TRUE))
(per_job <- data %>% pull(time) %>% range(na.rm=TRUE))

# Estimate for new analyses, repeating the above for two more traits
(hrs <- (tot * 2) / 72)

# raijin estimate (ksU)
tot * 2 /1000
# EC2 estimate - https://aws.amazon.com/ec2/spot/pricing/
# A c5.18xlarge machine has 72 VPCU and costs 0.9391c/hr on spot deman pricing
# these amchines have low
# Amazon EC2 C5 instances are the next generation of the Amazon EC2 Compute Optimized instance family. C5 instances offer the lowest price per vCPU in the Amazon EC2 family and are ideal for running advanced compute-intensive workloads.
# https://aws.amazon.com/ec2/instance-types/c5/
(hrs/24)
(cost <- hrs*0.9391)
