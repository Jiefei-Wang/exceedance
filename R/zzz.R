#' @useDynLib exceedance, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom digest digest
NULL

pkg_data <- new.env()
pkg_data$criticals <- NULL
pkg_data$use_cache <- TRUE

load_criticals <- function(){
    if(is.null(pkg_data$criticals)){
        pkg_data$criticals <- package_cached_critical
    }
}


packageCacheName <- "exceedance_critical"
.onLoad <- function(libname, pkgname){
    # pkg_data$criticals <- R.cache::loadCache(key = list(packageCacheName))
}

.onUnload <- function(libpath){
    # env = new.env()
    # capture.output(tryCatch({
    #     env$mutex <- synchronicity::boost.mutex(packageCacheName, create = FALSE)
    # },
    # error = function(e) {
    #     env$mutex <- synchronicity::boost.mutex(packageCacheName, create = TRUE)
    # }), type = "message")
    # on.exit(synchronicity::unlock(env$mutex))
    # synchronicity::lock(env$mutex)
    # old_cache <- R.cache::loadCache(key = list(packageCacheName))
    # new_cache <- combine_env(old_cache,pkg_data$criticals)
    # R.cache::saveCache(new_cache, key = list(packageCacheName))
}