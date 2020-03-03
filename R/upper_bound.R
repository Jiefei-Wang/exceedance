exceedance_bound<-function(profiled_data, alpha,ri=NULL,sri = NULL,rx=NULL,...){
    method <- profiled_data$params$method
    postfix <- profiled_data$params$postfix
    result <- call_func(root = "bound", postfix= c(method,postfix),
                        profiled_data = profiled_data, alpha = alpha,
                        ri=ri,sri =sri,rx=rx,...)
    result
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



bound_GW_k_order_index <- function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    k<- params$param1
    m <- profile$m
    x_rank <- profile$x_rank
    local_level <- profile$local_level
    
    
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

bound_GW_k_order_proportion <- 
    function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    
    q<- params$param1
    m <- profile$m
    sx <- profile$sx
    x_rank <- profile$x_rank
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    
    
    k_list <- pmax(ceiling(seq_len(m)*q),1L)
    cut <- qbeta(alpha,k_list,seq_len(m)-k_list+1L)
    # browser()
    nonsig_index <- rep(m+1L,m)
    for(i in seq_along(nonsig_index)){
        ind <- which(sx>cut[i])
        if(length(ind!=0)){
            if(ind[1]<=m-i+k_list[i]){
                nonsig_index[i] <- max(ind[1],k_list[i])
            }
        }
    }
    # browser()
    FP <- 0
    for(n in seq_along(cut)){
        cur_cut <- cut[n]
        cur_k <- k_list[n]
        index <- nonsig_index[n]
        ## all sets are rejected
        if(index == m+1L)
            next
        L <- sum(sorted_i<index)
        if(L<=cur_k-1){
            FP <- length(sorted_i)
            break
        }else{
            if(L!=n){
                FP <- max(FP,cur_k-1L+length(sorted_i)-L)
            }else{
                FP <- max(FP,cur_k-1)
            }
        }
    }
    FP/length(sorted_i)
}


bound_GW_order_general <- function(profiled_data,alpha,ri=NULL,sri = NULL,rx=NULL,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    range_type <- params$range_type
    param1 <- params$param1
    param2 <- params$param2
    statistic <- params$statistic
    m <- profile$m
    sx<- profile$sx
    cache <- profile$cache
    
    
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    rj_num <- length(sorted_i)
    ## reduce the loop number by checking the current FDR
    FDR <- 0
    for(n in rev(seq_len(m))){
        
        if(range_type=="proportion"){
            index1 <- get_index_from_proportion(n=n,param=param1)
            index2 <- get_index_from_proportion(n=n,param=param2)
        }else{
            if(!is.null(param1)&&param1[length(param1)]>n){
                FDR <- max(FDR, min(n,rj_num) / rj_num)
                next
            }
            if(!is.null(param2)&&param2[length(param2)]>n){
                FDR <- max(FDR, min(n,rj_num) / rj_num)
                next
            }
            index1 <- param1
            index2 <- param2
        }
        ## generic bound
        bound <- get_local_critical(stat = statistic, n=n, alpha=alpha,
                                    indexL=index1,indexU=index2)
        
        ## This is the true bound
        bound <- process_local_critical(bound,indexL=index1,indexU=index2)
        x_range <- get_range_by_bound(sx=sx,bound=bound)
        if(is.null(x_range)){
            next
        }
        L <- x_range$L
        H <- x_range$H
        FP <- 0L
        i<- 1L
        j <- 1L
        repeat{
            if(sorted_i[i]>=L[j]&sorted_i[i]<=H[j]){
                FP <- FP + 1L
                i <- i+1L
                j<- j+1L
            }else{
                if(sorted_i[i]<L[j]){
                    i<-i+1L
                }else{
                    if(sorted_i[i]>H[j]){
                        j<-j+1L
                    }
                }
            }
            if(i>rj_num||
               j>n){
                break
            }
        }
        FDR <- max(FDR,FP/rj_num)
    }
    FDR
}