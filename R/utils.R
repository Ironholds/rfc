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

name_formatter <- function(x){
  x <- gsub(x = x, pattern = "<.*>", replacement = "")
  if(!grepl(x = x, pattern = ",", fixed = TRUE)){
    return(x)
  } else {
    x <- unlist(strsplit(x,","))
    x <- paste(gsub(x = x[2], pattern = " ", replacement = ""), x[1], collapse = "")
    return(x)
  }
}