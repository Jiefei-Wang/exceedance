exceedance_bound<-function(profiled_data, alpha,ri=NULL,sri = NULL,rx=NULL,...){
    method <- profiled_data$params$method
    postfix <- profiled_data$params$postfix
    result <- call_func(root = "bound", postfix= c(method,postfix),
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
    
    FDR <- length(intersect(U,sorted_i))/length(sorted_i)
    FDR
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
    
    ## If there is no unrejected set
    ## FRP = 0
    if(length(U_index)==0){
        return(0)
    }
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    
    cur_key <- get_set_key(sorted_i,m)
    
    FP <- get_overlapped_num(S_key,U_index,cur_key)
    
    FDR <- max(FP)/length(sorted_i)
    FDR
}

bound_general_JW <- function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    pvalue_func <- params$pvalue_func
    m <- profile$m
    sx<- profile$sx
    x_rank <- profile$x_rank
    cache <- profile$cache
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    sorted_i_descent<- rev(sorted_i)
    
    candidate_set_descent <- rev(setdiff(seq_len(m),sorted_i))
    max_candidate_num <- length(candidate_set_descent)
    
    total_positive <- length(sorted_i)
    
    FDR <- 0
    ## pick false positive number
    for(FP in rev(seq_len(total_positive))){
        ## pick the size of the candidate set(exclude the false positive)
        for(unreject_size in seq_len(max_candidate_num+1L)-1L){
            ## construct the entire candidate set
            index <- c(candidate_set_descent[seq_len(unreject_size)],
                       sorted_i_descent[seq_len(FP)])
            id <- digest::digest(index)
            if(!exists(id,envir=cache$pvalues)){
                pvalue <- pvalue_func(sx[sort(index)])
                cache$pvalues[[id]] <- pvalue
            }else{
                pvalue <- cache$pvalues[[id]]
            }
            ## test if the candidate set is rejected at level alpha
            if(pvalue > alpha){
                FDR <- FP/length(sorted_i)
                break;
            }
        }
        if(pvalue > alpha){
            break;
        }
    }
    FDR
}