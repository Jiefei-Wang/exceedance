exceedance_bound<-function(profiled_data, alpha,ri=NULL,sri = NULL,rx=NULL,...){
    method <- profiled_data$params$method
    algorithm <- profiled_data$params$algorithm
    result <- call_func(root = "bound", postfix= c(method,algorithm),
                        profiled_data = profiled_data, alpha = alpha,
                        ri=ri,sri =sri,rx=rx,...)
    result
}



bound_GW_k_order <- function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    k<- params$param1
    m <- profile$m
    x_rank <- profile$x_rank
    local_level <- profile$local_level
    
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    if(alpha>max_alpha)
        return(min(k-1,length(sorted_i))/length(sorted_i))
    
    J_all <- which(local_level[k:m]>=alpha)
    
    if(length(J_all)==0){
        J <- m + 1L
    }else{
        J <- min(J_all)+k-1
    }
    U <- setdiff(seq_len(m),k-1+seq_len(J-k))
    
    gammabar <- length(intersect(U,sorted_i))/length(sorted_i)
    gammabar
}



bound_general_general <- function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    m <- profile$m
    x_rank <- profile$x_rank
    S<- profile$S
    pvalues <- profile$pvalues
    S_key <- profile$S_key
    
    ## Compute the unrejected set
    U_index <- pvalues$i[pvalues$p>alpha]
    U_prime_id <- S_key[U_index]
    
    ## If there is no unrejected set
    ## FRP = 0
    if(length(U_index)==0){
        return(0)
    }
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    
    cur_key <- get_set_key(sorted_i,m)
    
    FP <- get_overlapped_num(cur_key,U_prime_id)
    
    tau_hat <- max(FP)/length(sorted_i)
    tau_hat
}

