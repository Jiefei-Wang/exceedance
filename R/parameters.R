#######################
## Dispatching rule:
## if postfix_XX is not null, postfix_XX will first be used to dispatch
## otherwise, dispatch based on postfix
#######################

#' @export
param_GW<-function(statistic = c("k_order",
                                 "KS","HC","BJ"), param1=NULL,
                   param2=NULL,range_type=c("index","proportion")){
    statistic <- match.arg(statistic)
    range_type <- match.arg(range_type,c("index","proportion"))
    postfix <- NULL
    postfix_profile <- NULL
    postfix_bound <- NULL
    postfix_inference <- NULL
    if(statistic%in%c("KS","HC","BJ")){
        if(range_type == "proportion"){
            stopifnot(length(param1)<=2)
            stopifnot(length(param1)<=2)
        }
        if(is.null(param1)&&is.null(param2)){
            param1 <- c(0,1)
            range_type <- "proportion"
        }
        param1 <- sort(fill_range(range_type,param1))
        param2 <- sort(fill_range(range_type,param2))
        postfix <- "order_general"
    }else{
        if(range_type=="index"){
            postfix<-paste0(statistic,"_index")
            postfix_inference <- postfix
        }else{
            postfix<-paste0(statistic,"_proportion")
        }
    }
    
    parms <- list(method = "GW",
                  postfix = postfix,
                  postfix_profile = postfix_profile,
                  postfix_bound = postfix_bound,
                  postfix_inference = postfix_inference,
                  statistic = statistic,
                  param1 = param1,
                  param2 = param2,
                  range_type=range_type)
    .exceedance_parameters(parms)
}

#' @export
param_general<-function(pvalue_func,algorithm = c("general","JW")){
    algorithm <- match.arg(algorithm)
    parms <- list(method = "general",
                  pvalue_func = pvalue_func,
                  algorithm = algorithm,
                  postfix = algorithm)
    .exceedance_parameters(parms)
}

#' @export
param_combine<-function(...,alpha_weight = rep(1,length(list(...)))){
    test_params <- list(...)
    parms <- list(method = "combine",
                  test_params = test_params,
                  algorithm = "GW",
                  alpha_weight=alpha_weight)
}

