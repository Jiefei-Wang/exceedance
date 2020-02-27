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
    key <- get_bit_obj(totalElt)
    set_bit_obj(key,index-1L)
    key
}

## key_list must be a vector of keys
get_overlapped_num <- function(key_list,list_index,key){
    get_list_inter_number(key_list,list_index-1L,key)
}
## is the set1 a subset of the set2(s)?
is_subset<-function(key_list,list_index,key){
    get_overlapped_num(key_list,list_index,key)==get_bit_count(key)
}


