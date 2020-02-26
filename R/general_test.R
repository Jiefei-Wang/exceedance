enum.choose <- function(x, k) {
    if(k > length(x)) stop('k > length(x)')
    if(choose(length(x), k)==1){
        list(as.vector(combn(x, k)))
    } else {
        cbn <- combn(x, k)
        lapply(seq(ncol(cbn)), function(i) cbn[,i])
    }
}

profile_general_exceedance<-function(x,test_func,criticals){
    m <-length(x)
    x_sort <- sort(x, index.return=TRUE)
    sx <- x_sort$x
    ix <- x_sort$ix
    rx <- rank(x)
    U = list()
    # Run each subset of size at least 2 through the above function
    for(size in 1:m){
        allsubsets = enum.choose(1:m, size)
        for(i in 1:length(allsubsets)){
            subsetx = sx[allsubsets[[i]]]
            stat <- test_func(subsetx)
            if(stat < criticals[size]){
                U = append(list(allsubsets[[i]]), U)
            }
        }
    }
    list(x=x,ix=ix,rx=rx,U=U)
}

estimate_gamma_general<-function(profile,ri=NULL,sri = NULL,rx=NULL,...){
    U <- profile$U
    x <- profile$x
    x_rank <- profile$rx
    sorted_i <- get_ordered_index(x_rank,ri,sri,rx)
    
    
    
    gamma_set <- vapply(seq_along(U),function(i,sorted_i,U){
        num = length(intersect(sorted_i, U[[i]]))
        denom = length(sorted_i)
        num / denom
    },numeric(1), sorted_i = sorted_i , U= U)
    
    gammabar=max(gamma_set,0)
    gammabar
}


