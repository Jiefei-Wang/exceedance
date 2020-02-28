profile_pvalue<-function(x, params){
    method <- params$method
    postfix <- params$postfix
    result <- call_func(root = "profile", postfix= c(method,postfix),
                        x=x,params = params)
    result
}



#pvalue_func <- function(x)ks.test(x,punif)$p.value
profile_general_general<-function(x,params,...){
    pvalue_func <- params$pvalue_func
    m <-length(x)
    sx <- sort(x)
    
    
    
    ## The set that contains all possible combination
    ## of the data. The combination is stored in the 
    ## sorted index form.
    S <- list(1:m)
    pvalues <-data.frame(i=1,p=pvalue_func(sx))
    ## log prime number
    S_key <- list(get_set_key(1:m,m))
    
    current_S <- list()
    current_pvalue <- c()
    # Run each subset of size at least 2 through the above function
    for(size in rev(seq_len(m-1))){
        allsubsets = enum.choose(1:m, size)
        ## for each subset 
        for(i in seq_along(allsubsets)){
            cur_index <- allsubsets[[i]]
            subsetx = sx[cur_index]
            cur_pvalue <- pvalue_func(subsetx)
            ## for the sets that has larger pvalue than the current one
            ## check if the set is a super set of the current set
            ## If not, add the current set to the list
            index_higherP <- find_higherP_index(cur_pvalue,pvalues)
            current_set_id <- get_set_key(cur_index,m)
            if(length(index_higherP)!=0){
                if(any(is_subset(S_key,index_higherP,current_set_id))){
                    next
                }
            }
            relative_offset <- length(current_S)+1L
            abosolute_offset <- relative_offset+length(S)
            current_S[[relative_offset]] <- cur_index
            if(length(current_pvalue)!=0){
                current_pvalue[relative_offset,] <- c(abosolute_offset,cur_pvalue)
            }else{
                current_pvalue <- data.frame(i=abosolute_offset,p=cur_pvalue)
            }
            S_key[[abosolute_offset]]=current_set_id
        }
        ## record the current subset
        if(length(current_pvalue)!=0){
            pvalues <- rbind(pvalues,current_pvalue)
            pvalues <- pvalues[order(pvalues$p),]
        }
        S <- c(S,current_S)
        ## clear the current subset list for the next iteration
        current_S <- list()
        current_pvalue <- c()
    }
    
    x_rank <- rank(x)
    profile <- list(S=S,x=x,sx=sx,x_rank=x_rank,m=m,
                    pvalues=pvalues,
                    S_key=S_key)
    .exceedance_profile(
        list(params = params, profile =profile)
        )
}


profile_general_JW<-function(x,params,...){
    cache <- list()
    cache$search_path <- new.env()
    cache$pvalues <- new.env()
    m <-length(x)
    sx <- sort(x)
    x_rank <- rank(x)
    profile <- list(x=x,sx=sx,x_rank=x_rank,m=m,cache=cache)
    .exceedance_profile(
        list(params = params, profile =profile)
    )
}



profile_GW_k_order <- function(x, params){
    k <- params$param1
    m <- length(x)
    sx <- sort(x)
    local_level <- pbeta(sx,k,m-seq_len(m)+1)
    max_alpha <- max(local_level[k:m])
    
    x_rank <- rank(x)
    profile <- 
        list(local_level=local_level,x=x,sx=sx,x_rank=x_rank,m=m,
             max_alpha=max_alpha)
    .exceedance_profile(
        list(params = params, profile =profile)
        )
}

profile_GW_order_general<-function(x,params,...){
    cache <- list()
    cache$criticals <- new.env()
    cache$pvalues <- new.env()
    m <-length(x)
    sx <- sort(x)
    x_rank <- rank(x)
    profile <- list(x=x,sx=sx,x_rank=x_rank,m=m,cache=cache)
    .exceedance_profile(
        list(params = params, profile =profile)
    )
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
    
    is_one_sided <- grep("+",statistic,fixed=TRUE)
    
    FDR <- 0
    for(n in seq_len(m)){
        if(range_type=="proportion"){
            index1 <- get_index_from_proportion(n=n,param=param1)
            index2 <- get_index_from_proportion(n=n,param=param2)
        }else{
            
        }
        
        
        
        
        
    }
    if(is_one_sided){
        get_local_critical()
    }
    
    
    
}
