profile_pvalue<-function(x, params){
    method <- params$method
    algorithm <- params$algorithm
    result <- call_func(root = "profile", postfix= c(method,algorithm),
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
    current_S_key <- list()
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
                super_set_id<-S_key[index_higherP]
                if(any(is_subset(current_set_id,super_set_id))){
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
            current_S_key[[relative_offset]] <- current_set_id
        }
        ## record the current subset
        if(length(current_pvalue)!=0){
            pvalues <- rbind(pvalues,current_pvalue)
            pvalues <- pvalues[order(pvalues$p),]
        }
        S <- c(S,current_S)
        S_key <- c(S_key,current_S_key)
        ## clear the current subset list for the next iteration
        current_S <- list()
        current_pvalue <- c()
        current_S_key <- list()
    }
    
    x_rank <- rank(x)
    profile <- .exceedance_profile(
        list(S=S,x=x,sx=sx,x_rank=x_rank,m=m,
                    pvalues=pvalues,
             S_key=S_key)
        )
    list(params = params, profile =profile)
}










profile_GW_k_order <- function(x, params){
    k <- params$param1
    m <- length(x)
    sx <- sort(x)
    local_level <- pbeta(sx,k,m-seq_len(m)+1)
    max_alpha <- max(local_level[k:m])
    
    x_rank <- rank(x)
    profile <- .exceedance_profile(
        list(local_level=local_level,x=x,sx=sx,x_rank=x_rank,m=m,
                    max_alpha=max_alpha)
        )
    list(params = params, profile =profile)
}



