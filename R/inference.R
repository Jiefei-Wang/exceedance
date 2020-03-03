exceedance_inference<-function(profiled_data, alpha, bound,...){
    inference_postfix <- profiled_data$params$inference_postfix
    if(!is.null(inference_postfix)){
    result <- call_func(root = "inference", postfix= method,
                        profiled_data = profiled_data, alpha = alpha,
                        bound=bound,...)
    result
    }else{
        inference_general(profiled_data, alpha, bound,...)
    }
}



inference_general<-function(profiled_data, alpha, bound,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    k<- params$param1
    m <- profile$m
    
    reject <- integer(0)
    for(j in rev(seq_along(x))){
        gammabar = exceedance_bound(profiled_data,alpha,sri = 1L:j,...)
        if(gammabar<=bound){
            reject<-1L:j
            break
        }
    }
    reject
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
    
    inference_general(profiled_data, alpha, bound)
}