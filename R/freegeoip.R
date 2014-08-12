#' Workign with HDX's Curation Process System, a system designed to curate humanitarian-relevant datasets.
#'
#' freegeoip 
#' 
#' freegeoip takes a data.frame of IP addresses (with a single column) and geocodes each IP
#' address using the freegeoip.net service. Errors (IPs not identified by the system) are
#' encoded as NAs. The output is a data.frame.
#'
#' @param df data.frame (with a single column) with IP addresses. This is the column that will be
#' geocoded.
#'
#' @keywords gis geocode mapping ip address
#'
#' @export
#'
#' @examples
#' # Geocoding a single IP address
#' # x <- data.frame('72.225.192.110')
#' # y <- freegeoip(x)

freegeoip <- function(df = NULL) { 

    if (is.data.frame(df) == FALSE) { 
      warning('This function only works with data.frames. Please provide a data.frame.') 
    }

    else {
      final <- data.frame()
      for (i in 1:nrow(df)) {
          url <- paste("http://freegeoip.net/json/", c(as.character(df[i,])), sep="")
          line <- tryCatch( 
            data.frame(fromJSON(getURLContent(url))), 
            error = function(e) e
            )
          if(inherits(line, "error")) line <- c(NA)
          final <- rbind(final, line)
        }
      return(final)
    }
}