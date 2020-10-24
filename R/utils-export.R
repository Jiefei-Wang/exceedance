#' @export
use_cache <- function(x = c(TRUE, FALSE)){
    x <- match.arg(x)
    pkg_data$use_cache <- x
}