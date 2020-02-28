## Get the ordered index for the ordered x
## such that x[index] == ordered_x[ordered_index]
## ri: rejected i
## sri: sorted rejected i
## rx: rejected x
get_ordered_index<-function(x_rank,ri=NULL,sri = NULL,rx=NULL){
    if(all.null(ri,sri,rx))
        stop("The rejection set must be provided")
    if(!is.null(sri)){
        sorted_i <- sri
    }else{
        if(!is.null(rx)){
            ri <- which(x%in%rx)
        }
        sorted_i <- x_rank[ri]
    }
    sorted_i
}

GW_k_order_tau_hat<-function(i,J,k,m){
    if(i<k)
        return(1)
    ## J>=k
    if(J<=m){
        if(i<=J-1){
            return((k-1)/i)
        }else{
            return((k-1+i-J+1)/i)
        }
    }else {
        ## J>m
        return((k-1)/i)
    }
}



find_higherP_index <-function(cur_p, pvalues){
    p<-pvalues$p
    lower_i <-1L
    upper_i <- nrow(pvalues)
    
    
    if(p[upper_i]<=cur_p){
        return(integer(0))
    }else{
        if(p[lower_i]>cur_p){
            return(pvalues$i)
        }
    }
    
    repeat{
        if(upper_i-lower_i==1){
            break
        }
        index_middle <- round((lower_i+upper_i)/2)
        p_middle <- p[index_middle]
        if(p_middle<cur_p){
            lower_i <- index_middle
        }else{
            if(p_middle>cur_p){
                upper_i <- index_middle
            }else{
                upper_i <- index_middle
                break
            }
        }
    }
    pvalues$i[upper_i:nrow(pvalues)]
}

enum.choose <- function(x, k) {
    if(k > length(x)) stop('k > length(x)')
    if(choose(length(x), k)==1){
        list(as.vector(combn(x, k)))
    } else {
        cbn <- combn(x, k)
        lapply(seq(ncol(cbn)), function(i) cbn[,i])
    }
}


######################################################
## GW general ordered statistics algorithm
######################################################
get_local_critical <-function(stat, n, alpha, index,indexL,indexU){
    func_name <- gsub("+","",stat,fixed=TRUE)
    if(func_name==stat){
        critical <- GKSCritical(alpha=alpha,n=n,indexL=indexL,indexU=indexU,statName=stat)
    }else{
        critical <- GKSCritical(alpha=alpha,n=n,index=index,statName=stat)
    }
    level <- do.call(paste0(func_name,"LocalCritical"),args = list(
        stat= critical,
        n=n
    ),
    envir = getNamespace("generalKSStat"))
    level
}

get_index_from_proportion<-function(n,param){
    if(is.null(param)) return(param)
    param <- ceiling(param*n)
    param[param==0] <- 1
    seq_len(param[2]-param[1]+1) + param[1] -1
}