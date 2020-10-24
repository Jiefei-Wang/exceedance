#' Profile the data
#' 
#' Preprocessing the data and collecting information before performing 
#' the exceedance control. This step is developed for reducing the 
#' computational burder only. It is not related to statistics.
#' 
#' @param x numeric, p-values from some tests
#' (E.g T-test for high-thoughput gene data.), with each element representing a 
#' hypothesis.
#' @param params exceedance_parameters object
#' @param ... not used
#' 
#' @inherit exceedance_inference examples
#' @return an exceedance_profile object
#' @export
exceedance_profile<-function(x, params,...){
    m <- length(x)
    sx <- sort(x,index.return = TRUE)
    x_rank <- rep(0,m)
    x_rank[sx$ix] <- seq_len(m)
    profile <- list(
        m = m,
        x = x,
        x_sort = as.numeric(sx$x),
        x_sort_index = sx$ix,
        x_rank = x_rank
    )
    
    profiled_data <- .exceedance_profile(
        list(params = params, profile =profile)
    )
    
    method <- params$method
    if(!is.null(profiled_data$params$postfix_profile))
        postfix <- profiled_data$params$postfix_profile
    else
        postfix <-profiled_data$params$postfix
    
    result <- call_func(root = "profile", 
                        postfix= c(method,postfix),
                        x=x,params = params,
                        profiled_data=profiled_data,...)
    result
}






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
#' params <- param_fast_GW(statistic = "kth_p", param1 = 3)
#' 
#' ## generate p-values
#' x <- rbeta(10, 1, 10)
#' 
#' ## profile the data
#' profile <- profile_pvalue(x,params)
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
exceedance_inference<-function(profiled_data, alpha, bound,...){
    method <- profiled_data$params$method
    postfix_inference <- profiled_data$params$postfix_inference
    if(!is.null(postfix_inference)){
        result <- call_func(root = "inference", postfix= c(method,postfix_inference),
                            profiled_data = profiled_data, alpha = alpha,
                            bound=bound,...)
    }else{
        result <- inference_general(profiled_data = profiled_data, 
                                    alpha = alpha,
                                    bound=bound,...)
    }
    result
}

inference_general<-function(profiled_data, alpha, bound,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    m <- profile$m
    x_sort_index <- profile$x_sort_index
    
    reject <- integer(0)
    for(j in rev(seq_len(m))){
        gammabar = exceedance_bound(profiled_data,alpha,sri = 1L:j,...)
        if(gammabar<=bound){
            reject<-1L:j
            break
        }
    }
    x_sort_index[reject]
}