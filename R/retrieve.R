#'@title grab an IETF RfC and extract metadata
#'
#'@description \code{\link{retrieve_rfc}} lets you get an IETF RfC and extract the associated
#'metadata, including its authors, status, title, abstract, date of publication and what
#'RfCs it rendered obsolete (if any)
#'
#'@param id an RfC ID (e.g. "2000")
#'
#'@examples
#'results <- retrieve_rfc(1138)
#'
#'@export
retrieve_rfc <- function(id){
  
  #Construct URL
  url <- paste0("http://tools.ietf.org/html/rfc",id)
  
  #Retrieve
  rfc_results <- html(url)
  
  #Parser
  output <- parser(rfc_results,id)
  
  return(output)
}