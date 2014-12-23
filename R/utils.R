find_and_strip <- function(x, regex){
  return(gsub(x = x[grepl(x = x, pattern = regex)],
              pattern = regex, replacement = ""))
}

split_and_num <- function(x){
  return(as.numeric(unlist(strsplit(x,",",TRUE))))
}

date_formatter <- function(x){
  return(strptime(paste("01",x),"%d %B, %Y", tz = "UTC"))
}