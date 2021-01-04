## code to prepare `criticals` dataset goes here
#devtools::load_all()
library(parallel)
cl <- makeCluster(6)
invisible(clusterEvalQ(cl,library(exceedance)))
package_cached_critical <- exceedance:::pkg_data$criticals

#####################################
## BJ, KS, HC
#####################################

stat_list <- c("BJ")
alpha_list <- c(0.1)
n_list <- 1:10000
for(statName in stat_list){
    for(alpha in alpha_list){
        clusterExport(cl, c("alpha","statName"))
        cur_criticals <- parSapplyLB(cl = cl, n_list, function(n){
            critical <- 
                exceedance:::get_critical(
                    statName=statName, n=n, alpha=alpha,
                    indexL=seq_len(n),indexU=seq_len(n))
        })
        cache <- clusterEvalQ(cl,exceedance:::pkg_data$criticals)
        for(e in cache){
            package_cached_critical <- exceedance:::combine_env(package_cached_critical,e)
        }
    }
}


#####################################
## BJ+, KS+, HC+
#####################################
for(statName in stat_list){
    for(alpha in alpha_list){
        clusterExport(cl, c("alpha","statName"))
        cur_criticals <- parSapplyLB(cl = cl, n_list, function(n){
            idxL <- seq_len(n)
            critical <- 
                exceedance:::get_critical(
                    statName=statName, n=n, alpha=alpha,
                    indexL=idxL, indexU = NULL)
        })
        cache <- clusterEvalQ(cl,exceedance:::pkg_data$criticals)
        for(e in cache){
            package_cached_critical <- exceedance:::combine_env(package_cached_critical,e)
        }
    }
}

usethis::use_data(package_cached_critical, internal = TRUE, overwrite = TRUE)
