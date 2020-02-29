


fill_range <- function(type , param){
    if(length(param)==1){
        if(type == "proportion"){
            param <- c(0,param)
        }
    }
    param
}