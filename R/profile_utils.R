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


find_lowerP_index <-function(cur_p, pvalues){
    p<-pvalues$p
    lower_i <-1L
    upper_i <- nrow(pvalues)
    
    
    if(p[upper_i]<=cur_p){
        return(pvalues$i)
    }else{
        if(p[lower_i]>cur_p){
            return(integer(0))
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
                lower_i <- index_middle
                break
            }
        }
    }
    pvalues$i[seq_len(lower_i)]
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


