## code to prepare `criticals` dataset goes here
devtools::load_all()
library(parallel)
cl <- makeCluster(10)
invisible(clusterEvalQ(cl,library(exceedance)))

#####################################
## BJ, KS, HC
#####################################

stat_list <- c("BJ")
alpha_list <- c(0.05)
n_list <- 1:1000
for(statName in stat_list){
    for(alpha in alpha_list){
        clusterExport(cl, c("alpha","statName"))
        cur_criticals <- parSapplyLB(cl = cl, n_list, function(n){
            critical <- 
                exceedance:::get_critical(
                    statName=statName, n=n, alpha=alpha,
                    indexL=seq_len(n),indexU=seq_len(n))
        })
        for(i in seq_along(n_list)){
            n <- n_list[i]
            critical <- cur_criticals[i]
            cache_key <- get_cache_key(statName = statName, n=n, 
                                       alpha=alpha, indexL=seq_len(n),indexU=seq_len(n))
            set_cache_value(cache_key, critical)
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
            idxL <- seq_len(ceiling(n/2))
            idxU <- n- max(idxL) + idxL
            critical <- 
                exceedance:::get_critical(
                    statName=statName, n=n, alpha=alpha,
                    indexL=idxL, indexU = idxU)
        })
        for(i in seq_along(n_list)){
            n <- n_list[i]
            critical <- cur_criticals[i]
            cache_key <- get_cache_key(statName = statName, n=n, 
                                       alpha=alpha, indexL=seq_len(n),indexU=NULL)
            set_cache_value(cache_key, critical)
        }
    }
}

# e <- clusterEvalQ(cl,exceedance:::pkg_data$criticals)
# for(i in e){
#     pkg_data$criticals<-combine_env(pkg_data$criticals,i)
# }

n <- 4000
key <- get_cache_key("BJ", n =n ,alpha=0.05,indexL = seq_len(n),indexU=NULL)
exist_cache_value(key)

package_cached_critical <- pkg_data$criticals
usethis::use_data(package_cached_critical, internal = TRUE, overwrite = TRUE)
