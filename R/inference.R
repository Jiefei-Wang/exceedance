## dispatch rule: if postfit is not null, 
## dispatch according to the postfix
## Otherwise, dispatch to the general algorithm

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


inference_GW_k_order_index<-function(profiled_data, alpha, bound){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    x_sort_index <- profile$x_sort_index
    k<- params$param1
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