pkg_data <- new.env()

call_func <- function(root, postfix, ...){
    func_name <- paste0(c(root,postfix),collapse="_")
    do.call(func_name,args = list(...))
}

all.null<-function(...){
    args<-list(...)
    res <- vapply(args,is.null,logical(1))
    all(res)
}



get_set_key <- function(index,totalElt){
    x <- bit::bit(totalElt)
    x[index] <- TRUE
    x
}

get_overlapped_num <- function(key1, key2){
    if(!is.list(key1)&!is.list(key2)){
        return(sum(key1&key2))
    }
    
    if(is.list(key1)){
        vapply(key1,function(key1)get_overlapped_num(key1,key2),numeric(1))
    }else{
        if(is.list(key2)){
            vapply(key2,function(key2)get_overlapped_num(key1,key2),numeric(1))
        }
    }
}
get_elt_count <- function(key){
    if(!is.list(key)){
        sum(key)
    }else{
        vapply(key,sum,numeric(1))
    }
}
## is the set1 a subset of the set2(s)?
is_subset<-function(key1, key2){
    get_overlapped_num(key1,key2)==get_elt_count(key1)
}


