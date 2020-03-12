bound_general_GW_general <- function(profiled_data,alpha,
                                  sorted_i,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    m <- profile$m
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
    
    
    
    cur_key <- get_set_key(sorted_i,m)
    
    FP <- get_overlapped_num(S_key,U_index,cur_key)
    
    FDR <- max(FP)/length(sorted_i)
    FDR
}

bound_general_GW_JW <- function(profiled_data,alpha,
                             sorted_i,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    pvalue_func <- params$pvalue_func
    m <- profile$m
    x_sort<- profile$x_sort
    cache <- profile$cache
    
    candidate_set <- setdiff(seq_len(m),sorted_i)
    max_candidate_num <- length(candidate_set)
    total_positive <- length(sorted_i)
    
    # sorted_i_key <- digest(sorted_i)
    
    FDR <- 0
    ## pick false positive number
    for(FP in rev(seq_len(total_positive))){
        ## pick the size of the candidate set(exclude the false positive)
        for(unreject_size in seq_len(max_candidate_num+1L)-1L){
            ## construct the entire candidate set
            if(unreject_size==0L){
                index <- sorted_i[seq(length(sorted_i)-FP + 1L,length(sorted_i))]
            }else{
                index <- c(sorted_i[seq(length(sorted_i)-FP + 1L,length(sorted_i))],
                           candidate_set[seq(max_candidate_num-unreject_size + 1L,max_candidate_num)]
                )
            }
            index <- sort(index)
            pvalue <- pvalue_func(x_sort[index])
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


## Need optimization
bound_fast_GW_kth_p_index <- function(profiled_data,alpha,
                                   sorted_i,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    max_alpha <- profile$max_alpha
    k<- params$param1
    m <- profile$m
    local_level <- profile$local_level
    
    
    if(alpha>max_alpha)
        return(min(k-1,length(sorted_i))/length(sorted_i))
    
    J_all <- which(local_level[k:m]>alpha)
    
    if(length(J_all)==0){
        J <- m + 1L
    }else{
        J <- min(J_all)+k-1
    }
    
    FP <- sum(sorted_i<k)+sum(sorted_i>=J)
    # U <- setdiff(seq_len(m),k-1+seq_len(J-k))
    FDR <- FP/length(sorted_i)
    FDR
}

bound_fast_GW_kth_p_proportion <- function(profiled_data,alpha,
                                        sorted_i,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    
    q<- params$param1
    m <- profile$m
    x_sort <- profile$x_sort
    
    k_list <- pmax(ceiling(seq_len(m)*q),1L)
    cut <- qbeta(alpha,k_list,seq_len(m)-k_list+1L)
    # browser()
    nonsig_index <- rep(m+1L,m)
    for(i in seq_along(nonsig_index)){
        ind <- which(x_sort>cut[i])
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


bound_fast_GW_order_general <- function(profiled_data,alpha,
                                   sorted_i,...){
    profile <- profiled_data$profile
    params <- profiled_data$params
    range_type <- params$range_type
    param1 <- params$param1
    param2 <- params$param2
    statistic <- params$statistic
    m <- profile$m
    x_sort<- profile$x_sort
    params_key <- profile$params_key

    preprocessed_key <- paste0("GW",alpha,params_key)
    
    rj_num <- length(sorted_i)
    ## reduce the loop number by checking the current FDR
    FDR <- 0
    for(n in rev(seq_len(m))){
        # if(n==5)
        #     browser()
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
        critical_key <- paste0(preprocessed_key,n)
        if(!exists(critical_key,envir=pkg_data$criticals)){
            bound <- get_local_critical(stat = statistic, n=n, alpha=alpha,
                                        indexL=index1,indexU=index2)
            ## This is the true bound
            bound <- process_local_critical(bound,indexL=index1,indexU=index2)
            pkg_data$criticals[[critical_key]] <- bound
        }else{
            bound <- pkg_data$criticals[[critical_key]]
        }
        
        # x_range <- get_range_by_bound(sx=x_sort,bound=bound)
        x_range <- C_get_range_by_bound(R_sx=x_sort,R_l=bound$l,R_h=bound$h)
        if(is.null(x_range)){
            next
        }
        L <- x_range$L
        H <- x_range$H
        
        FDR <- max(FDR,C_GW_compute_FDR(sorted_i,H,L,rj_num,n))
        
    }
    FDR
}

bound_combine <- function(profiled_data,alpha,
                                   sorted_i,...){
    profiles <- profiled_data$profile$profiles
    alpha_weight <- profiled_data$params$alpha_weight
    
    alphas <- alpha/sum(alpha_weight)*alpha_weight
    bounds <- lapply(seq_along(profiles), 
           function(i,profiles,alphas)
               exceedance_bound(profiled_data=profiles[[i]],
                                alpha=alphas[i],
                                sri = sorted_i),
           profiles=profiles,
           alphas=alphas)
    min(as.numeric(bounds))
}