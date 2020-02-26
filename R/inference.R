exceedance_inference<-function(profiled_data, alpha, bound,...){
    method <- profiled_data$params$method
    result <- call_func(root = "inference", postfix= method,
                        profiled_data = profiled_data, alpha = alpha,
                        bound=bound,...)
    result
}



inference_GW<-function(profiled_data,...){
    method <- profiled_data$params$method
    result <- call_func(root = "inference_GW", postfix= method,
                        profiled_data = profiled_data,...)
    result
}


inference_GW_k_order<-function(profiled_data, alpha, bound){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    k<- params$param1
    m <- profile$m
    if(alpha>max_alpha){
        j <- ceiling((k-1)/bound)
        if(j > m){
            reject <- integer(0)
        }else{
            reject <- 1L:j
        }
        return(reject)
    }
        
    reject <- integer(0)
    for(j in rev(seq_along(x))){
        gammabar = exceedance_bound(profile,alpha,sri = 1L:j)
        if(gammabar<=bound){
            reject<-1L:j
            break
        }
    }
    reject
}