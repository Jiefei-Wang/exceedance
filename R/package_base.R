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
profile_pvalue<-function(x, params,...){
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



## Reject: pvalue <= alpha
## not reject: pvalue > alpha

#' Computing the confidence envolop of the false discover proportion for the data
#' 
#' Computing the 1 - alpha level confidence envolop of the false discover 
#' proportion(FDP) given a set of rejected hypotheses. 
#' The confidence envolop can be viewed as a measurement of the quality of 
#' the statistical inference.
#' 
#' @param profiled_data an exceedance_profile object
#' @param alpha numeric, the confidence level
#' @param ri integer, the index of the rejected hypotheses, see details.
#' @param sri integer, the index of the ascending ordered p-values which the 
#' corresponding hypotheses are rejected, see details.
#' @param rx numeric, the value of the pvalues which the 
#' corresponding hypotheses are rejected, see details.
#' @param ... not used
#' 
#' @details 
#' This function is for constructing the confidence envolop of the
#' FDP given the set of rejected hypothese. The confidence envolop
#' depends on three factors: 
#' \itemize{
#' \item The p-value samples
#' \item The confidence level `alpha`
#' \item The rejected hypotheses.
#' }
#' Therefore, given the data, confidence level and the hypotheses that you want
#' to reject, we can obtain a `1 - alpha` confidence envolop of the FDP. 
#' 
#' The rejected hypotheses can be expressed in three ways. You can use the 
#' original index `ri` to indicate which hypotheses you want to reject. For
#' example, if `ri = 1:2`, it means the first and second hypotheses are rejected.
#' 
#' However, in practice, it is more common to reject the hypotheses which 
#' have small pvalues. You can achieve it by providing the parameter `sri`. 
#' For example, if `sri = 1:2`, it means the hypothese which have the smallest 
#' or second smallest pvalues are rejected. Alternatively, `rx` can be used if
#' you want to match the pvalues not the index. That is, a hypotheis is 
#' rejected if its pvalue matches any value in `rx`.
#' 
#' @return a `1 - alpha` level confidence envolop
#' @inherit exceedance_inference examples
#' @export
exceedance_bound<-function(profiled_data, alpha,ri=NULL,sri = NULL,rx=NULL,...){
    x_rank <- profiled_data$profile$x_rank
    sorted_i <- as.integer(get_ordered_index(x_rank,ri,sri,rx))
    
    method <- profiled_data$params$method
    if(!is.null(profiled_data$params$postfix_bound))
        postfix <- profiled_data$params$postfix_bound
    else
        postfix <-profiled_data$params$postfix
    
    result <- call_func(root = "bound", postfix= c(method,postfix),
                        profiled_data = profiled_data, alpha = alpha,
                        ri=ri,sri =sri,rx=rx,sorted_i,...)
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