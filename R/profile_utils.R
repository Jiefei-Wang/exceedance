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

#res1[j]=GW_k_order_tau_hat1(j,k,m,sx)
GW_k_order_tau_hat1<-function(i,k,m,sx){
    cut <- qbeta(alpha,k,seq_len(m-k+1))
    result <- c()
    for(l in seq_along(cut)){
        cur_cut <- cut[l]
        n <- k+l-1
        index <- k:(m-n+k)
        j_list <- which(sx[index]>cur_cut)
        if(length(j_list)==0){
            j=m+1
        }else{
            j <- min(j_list)+k-1
        }
        if(i<j){
            result=c(result,min(i,k-1)/i)
        }else{
            result=c(result,(k+min(i-j,n-k))/i)
        }
        
    }
    max(result)
}




