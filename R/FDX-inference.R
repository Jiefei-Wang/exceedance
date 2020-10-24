## dispatch rule: if postfit is not null, 
## dispatch according to the postfix
## Otherwise, dispatch to the general algorithm

#' Perform multiple hypothesis testing while controlling the exceedance rate
#' 
#' Perform multiple hypothesis testing while controlling the exceedance rate. 
#' The exceedance rate is Prob(FDP > bound), where FDP is the false discover 
#' proportion defined by the number of false positives devided by the number 
#' of total rejections. Small FDP with large number of reject is favorable 
#' in practice.
#' 
#' @inheritParams exceedance_bound
#' @param bound The upper bound of the false discover proportion
#' @examples 
#' ## The 3rd pvalue statistic
#' param <- param_fast_GW(statistic = "kth_p", param1 = 3)
#' 
#' ## generate p-values
#' x <- rbeta(10, 1, 10)
#' 
#' ## profile the data
#' profile <- profile_pvalue(x,param)
#' 
#' 
#' ## compute the 95% confidence envolop
#' alpha <- 0.05
#' 
#' ## reject the first three hypotheses
#' exceedance_bound(profile, alpha, ri = 3)
#' 
#' ## reject the hypothese which pvalues are equal to
#' ## the first three samples.
#' ## In other word, this is equivalent to reject the first three hypotheses
#' exceedance_bound(profile, alpha, rx = x[1:3])
#' 
#' ## reject the hypotheses which have the lowest 3 p-values
#' exceedance_bound(profile, alpha, sri = 3)
#' 
#' 
#' ## Determine which hypotheses can be rejected while controlling the
#' ## exceedance rate: P(FDP > bound) < alpha
#' alpha <- 0.05
#' bound <- 0.2
#' exceedance_inference(profile, alpha, bound)
#' 
#' @return Indices of the hypotheses that are rejected in the procedure.
#' @export
exceedance_inference<-function(profiled_data, alpha, bound){
    result <- profiled_data$param$inferenc_func(
        profiled_data = profiled_data, alpha = alpha,
        bound=bound)
    result
}

inference_general<-function(profiled_data, alpha, bound){
    profile <- profiled_data$profile
    param <- profiled_data$param
    m <- profile$m
    x_sort_index <- profile$x_sort_index
    
    reject <- integer(0)
    for(j in rev(seq_len(m))){
        gammabar = exceedance_bound(profiled_data,alpha,sri = 1L:j)
        if(gammabar<=bound){
            reject<-1L:j
            break
        }
    }
    x_sort_index[reject]
}

inference_fast_GW_kth_p_index<-function(profiled_data, alpha, bound){
    profile <- profiled_data$profile
    param <- profiled_data$param
    max_alpha <- profile$max_alpha
    x_sort_index <- profile$x_sort_index
    k<- param$param1
    m <- profile$m
    
    if(alpha>max_alpha){
        smallest_FDR <- (k-1)/m
        if(smallest_FDR > bound){
            return(integer(0))
        }else{
            return(x_sort_index)
        }
    }
    
    inference_general(profiled_data, alpha, bound)
}