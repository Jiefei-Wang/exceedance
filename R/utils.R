


call_func <- function(root, prefix=NULL, postfix=NULL, ...){
    func_name <- paste0(c(prefix,root,postfix),collapse="")
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


combine_env <- function(e1, e2) {
    e1name = deparse(substitute(e1))
    e2name = deparse(substitute(e2))
    list1 = ls(e1)
    list2 = ls(e2)
    e <- as.environment(as.list(e2))
    for(v in list1) {
        if(v %in% list2) next
        e[[v]] <- e1[[v]]
    }
    e
}
