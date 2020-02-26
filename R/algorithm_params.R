param_GW<-function(test_func = c("k_order","KS","HC","BJ"), param1=NULL,param2=NULL){
    test_func <- match.arg(test_func)
    parms <- list(method = "GW",
                  algorithm = test_func,
                  param1 = param1,
                  param2 = param2)
    .exceedance_parameters(parms)
}


param_general<-function(pvalue_func,algorithm = c("general","JW")){
    algorithm <- match.arg(algorithm)
    parms <- list(method = "general",
                  pvalue_func = pvalue_func,
                  algorithm = algorithm)
    .exceedance_parameters(parms)
}

