#' @export
profile_pvalue<-function(x, params,...){
    m <- length(x)
    sx <- sort(x,index.return = TRUE)
    x_rank <- rep(0,m)
    x_rank[sx$ix] <- seq_len(m)
    profile <- list(
        m = m,
        x = x,
        x_sort = as.numeric(sx$x),
        x_sort_index = sx$ix,
        x_rank = x_rank
    )
    
    profiled_data <- .exceedance_profile(
        list(params = params, profile =profile)
    )
    
    method <- params$method
    if(!is.null(profiled_data$params$postfix_profile))
        postfix <- profiled_data$params$postfix_profile
    else
        postfix <-profiled_data$params$postfix
    
    result <- call_func(root = "profile", 
                        postfix= c(method,postfix),
                        x=x,params = params,
                        profiled_data=profiled_data,...)
    result
}



#pvalue_func <- function(x)ks.test(x,punif)$p.value
profile_general_general<-function(x,params,profiled_data,...){
    pvalue_func <- params$pvalue_func
    profile <- profiled_data$profile
    m <-length(x)
    x_sort <- profile$x_sort
    
    
    ## The set that contains all possible combination
    ## of the data. The combination is stored in the 
    ## sorted index form.
    S <- list(1:m)
    pvalues <-data.frame(i=1,p=pvalue_func(x_sort))
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
            subsetx = x_sort[cur_index]
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
    
    profile <- c(profile,
                 list(S=S,
                      pvalues=pvalues,
                      S_key=S_key))
    
    profiled_data$profile <- profile
    profiled_data
}


profile_general_JW<-function(x,params,profiled_data,...){
    cache <- list()
    cache$search_path <- new.env()
    cache$pvalues <- new.env()
    
    profiled_data$profile$cache <- cache
    profiled_data
}



profile_GW_k_order_index <- function(x, params,profiled_data,...){
    profile <- profiled_data$profile
    k <- params$param1
    m <- profile$m
    x_sort <- profile$x_sort
    
    if(k>length(x)){
        stop("the order k(param1) cannot be greater than the sample size")
    }
    
    local_level <- pbeta(x_sort,k,m-seq_len(m)+1)
    max_alpha <- max(local_level[k:m])
    
    profile<-c(profile,
               list(
                   local_level=local_level,
                   max_alpha=max_alpha
               ))
    profiled_data$profile <- profile
    profiled_data
}

profile_GW_k_order_proportion <- function(x, params,profiled_data,...){
    profile <- profiled_data$profile
    k <- params$param1
    m <- profile$m
    x_sort <- profile$x_sort
    local_level <- pbeta(x_sort,k,m-seq_len(m)+1)
    
    profile<-c(profile,
               list(
                   local_level=local_level
               ))
    profiled_data$profile <- profile
    profiled_data
}





profile_GW_order_general<-function(x,params,profiled_data,...){
    range_type <- params$range_type
    param1 <- params$param1
    param2 <- params$param2
    statistic <- params$statistic
    
    profiled_data$profile$params_key <- digest::digest(list(range_type,param1,param2,statistic))
    profiled_data
}

profile_combine<-function(x,params,profiled_data,...){
    test_params <- params$test_params
    # x_sort <- profiled_data$profile$x_sort
    
    profiles <- lapply(test_params,function(param)profile_pvalue(x=x,params=param,...))
    profiled_data$profile$profiles <- profiles
    profiled_data
}


