.exceedance_parameters <- setClass("exceedance_parameters",contains = "list")
.exceedance_profile <- setClass("exceedance_profile",contains = "list")

title <-function(object){
    is.null(object$title)||object$title
}

`title<-` <- function(x, value){
    x$title <- value
    x
}


truncated_print_internal <- function(x, len){
    x_len <- length(x)
    x_print_len <- min(x_len, len)
    x_diff <- x_len - x_print_len
    result <- paste0(x[seq_len(x_print_len)], collapse = ",")
    if(x_diff != 0){
        result <- paste0(result, ",...(skip ",x_diff," elements)")
    }
    result
}
truncated_print <- function(x)truncated_print_internal(x,5L)

show_method_name <- function(name){
    name["fast_GW"%in%name] <- "fast GW"
    name["general_GW"%in%name] <- "general GW"
    name["combine_GW"%in%name] <- "combined GW"
    name
}


#' @export
setMethod("show", "exceedance_parameters",function(object){
    method <- object$method
    statistic <- object$statistic
    if(method == "fast_GW"){
        show_params_fast_GW(object)
    }
    if(method == "general_GW"){
        show_params_general_GW(object)
    }
    if(method == "combine_GW"){
        show_params_combine_GW(object)
    }
    
    invisible(NULL)
})

# parms <- list(method = "fast_GW",
#               postfix = postfix,
#               postfix_profile = postfix_profile,
#               postfix_bound = postfix_bound,
#               postfix_inference = postfix_inference,
#               statistic = statistic,
#               param1 = param1,
#               param2 = param2,
#               range_type=range_type)

show_params_fast_GW <- function(object){
    param1 <- object$param1
    param2 <- object$param2
    algorithm <- object$algorithm
    range_type <- object$range_type
    method <- object$method
    
    
    if(is.null(param1))
        param1 <- "NULL"
    if(is.null(param2))
        param2 <- "NULL"
    
    
    if(title(object)){
        cat("An S4 `exceedance_parameters` object:\n") 
    }
    cat("Method:", show_method_name(method),"\n")
    cat("Statistic:", algorithm,"\n")
    cat("param1:", truncated_print(param1), "\n")
    cat("param2:", truncated_print(param2), "\n")
    cat("range type:",range_type,"\n")
}

show_params_general_GW<-function(object){
    algorithm <- object$algorithm
    method <- object$method
    
    if(title(object)){
        cat("An S4 `exceedance_parameters` object:\n") 
    }
    cat("Method:", show_method_name(method),"\n")
    cat("algorithm:", algorithm,"\n")
}

show_params_combine_GW<-function(object){
    weight <- object$alpha_weight
    method <- object$method
    test_params <- object$test_params
    test_methods <- vapply(test_params,function(x)show_method_name(x$method),character(1))
    test_algorithms <- vapply(test_params,function(x)x$algorithm,character(1))
    
    if(title(object)){
        cat("An S4 `exceedance_parameters` object:\n") 
    }
    cat("Method:", show_method_name(method),"\n")
    cat("Contained methods:", truncated_print(test_methods),"\n")
    cat("Contained algorithms:", truncated_print(test_algorithms),"\n")
    cat("Weight:", truncated_print(weight),"\n")
}

