inference_fast_GW_kth_p_index<-function(profiled_data, alpha, bound){
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