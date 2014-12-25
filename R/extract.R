#'@title extract particular values from a retrieved set of RfC metadata
#'
#'@description foo bar
#'
#'@seealso \code{\link{retrieve_rfc}} for retrieving the data.
#'
#'@examples
#'rfc_results <- lapply(c(30,40,50), retrieve_rfc)
#'authors <- extract(rfc_results, "authors")
#'@export
extract <- function(rfcs, field){
  
  results <- unlist(lapply(rfcs, function(x,field){
    
    if(is.null(x) || is.na(x)){
      return(NULL)
    }
    
    if(is.null(x[field]) || is.na(x[field])){
      return(NULL)
    }
    
    if("" %in% x[field]){
      return(NULL)
    }
    return(x[field])
  }, field = field))
  
  return(unname(results))
}