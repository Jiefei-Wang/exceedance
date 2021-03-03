## code to prepare `criticals` dataset goes here
## devtools::load_all()
library(exceedance)
library(foreach)
library(doRedis)
source("data-raw/functions.R")
package_cache <- as.list(exceedance:::pkg_data$criticals)
queue <- "jobs"
host <- "192.168.2.100"
registerDoRedis(queue, host = host, progress = TRUE)
startLocalWorkers(8, queue = queue, host = host)

n<-5000
#####################################
## two-sided
## BJ, KS, HC
#####################################
n_list <- seq_len(n)
statName <- "BJ"
alpha_list <- c(0.1)
for(alpha in alpha_list){
    package_cache <- compute_critical(package_cache,cl,statName, 
                                      alpha, n_list,
                                      indexL="seq_len(n)", indexU="seq_len(n)")
}

#####################################
## BJ+, KS+, HC+
#####################################
n_list <- seq_len(n)
statName <- "BJ"
alpha_list <- c(0.1)
for(alpha in alpha_list){
    package_cache <- compute_critical(package_cache,cl,statName, 
                                      alpha, n_list,
                                      indexL="seq_len(n)", indexU="NULL")
}

#####################################
## paper
#####################################
n_list <- seq_len(2000)
statName <- "BJ"
alpha <- 0.1
k_list <- c(27:52,89:109)
for(k in k_list){
    message(k)
    package_cache <- compute_critical(package_cache,cl,statName, 
                     alpha, n_list,
                     indexL=paste0("seq_len(",k,")"), indexU="NULL")
}



save_criticals <- function(){
    package_cached_critical <- as.environment(package_cache)
    usethis::use_data(package_cached_critical, internal = TRUE, overwrite = TRUE)
}

save_criticals()


removeQueue(queue)
