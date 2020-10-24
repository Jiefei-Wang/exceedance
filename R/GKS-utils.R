getGKSIndex <- function(statName, n, indexL, indexU){
    side <- substring(statName,nchar(statName))
    if(is.null(indexL)&&is.null(indexU)){
        indexL <- seq_len(n)
        indexU <- seq_len(n)
    }
    sideIndicator <- which(side==c("+","-"))
    if(length(sideIndicator)!=0){
        statName <- substr(statName,1,nchar(statName)-1)
        if(sideIndicator == 1){
            indexU <- NULL
        }else{
            indexL <- NULL
        }
    }
    list(indexL = indexL, indexU = indexU, statName = statName)
}