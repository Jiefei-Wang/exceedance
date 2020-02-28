param_GW<-function(statistic = c("k_order",
                                 "KS","HC","BJ",
                                 "KS+","HC+","BJ+"), param1=NULL,
                   param2=NULL,range_type=c("index","proportion")){
    statistic <- match.arg(statistic)
    if(statistic%in%c("KS","HC","BJ",
                      "KS+","HC+","BJ+")){
        range_type <- match.arg(range_type,c("index","proportion"))
        if(!is.null(param1)){
            param1 <- fill_range(range_type,param1)
        }
        if(!is.null(param2)){
            param2 <- fill_range(range_type,param2)
        }
        postfix <- "order_general"
    }else{
        stopifnot(range_type!="index")
        postfix<-statistic
    }
    
    parms <- list(method = "GW",
                  postfix = postfix,
                  statistic = statistic,
                  param1 = param1,
                  param2 = param2,
                  range_type=range_type)
    .exceedance_parameters(parms)
}


param_general<-function(pvalue_func,algorithm = c("general","JW")){
    algorithm <- match.arg(algorithm)
    parms <- list(method = "general",
                  pvalue_func = pvalue_func,
                  algorithm = algorithm)
    .exceedance_parameters(parms)
}

