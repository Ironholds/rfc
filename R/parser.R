parser <- function(rfc_results,id){
  
  #Construct output object
  output <- list()
  output$id <- id
  
  #Grab the meta tags and extract useful data.
  output <- c(output,meta_parser(unname(unlist(html_attrs(html_nodes(rfc_results,"meta"))))))
  
  #Links
  output <- c(output,link_parser(unname(unlist(html_attrs(html_nodes(rfc_results,"a")))),id))
  
  #Contents
  output <- c(output,content_parser(html_text(html_nodes(rfc_results,"span"),trim=T)))
  
  class(output) <- c(class(output),"rfc")
  return(output)
}

#Parse the meta tags.
meta_parser <- function(meta_nodes){
  
  output <- list()
  
  formatted_nodes <- meta_nodes[seq(2,length(meta_nodes),2)]
  names(formatted_nodes) <- meta_nodes[seq(1,length(meta_nodes),2)]
  
  output$identifier <- unname(formatted_nodes["DC.Identifier"])
  output$authors <- unname(formatted_nodes[names(formatted_nodes) == "DC.Creator"])
  output$title <- unname(formatted_nodes["DC.Title"])
  output$date <- date_formatter(unname(formatted_nodes["DC.Date.Issued"]))
  output$abstract <- gsub(x = unname(formatted_nodes["DC.Description.Abstract"]), pattern = "(\\\\n|\\n)",
                          replacement = " ")
  
  #Return
  return(output)
}

#Extract links to other RfCs
link_parser <- function(link_nodes, id){
  
  output <- list()
  rfc_links <- unique(find_and_strip(link_nodes,"^\\./rfc"))
  rfc_links <- as.numeric(gsub(x = rfc_links, pattern = "#.*", replacement = ""))
  rfc_links <- rfc_links[!rfc_links == id & !rfc_links == ""]
  
  output$links <- rfc_links
  return(output)
}

#Extract metadata about the content
content_parser <- function(title_nodes){
  
  output <- list()
  heading_regex <- "^\\d\\. "
  output$sections <- find_and_strip(title_nodes,heading_regex)
  output$page_count <- sum(grepl(x = title_nodes, pattern = "[Page ", fixed = TRUE))
  output$status <- "Unknown"
  output$obsoleted_by <- NA
  output$updated_by <- NA
  if(grepl(x = title_nodes[3], pattern = "HISTORIC$")){
    output$status <- "Historic"
  } else if(grepl(x = title_nodes[3], pattern = "EXPERIMENTAL$")){
    output$status <- "Experimental"
  } else if(grepl(x = title_nodes[3], pattern = "PROPOSED STANDARD$")){
    output$status <- "Proposed standard"
  } else if(grepl(x = title_nodes[3], pattern = "BEST CURRENT PRACTICE$")){
    output$status <- "Best current practise"
  } else if(grepl(x = title_nodes[3], pattern = "PROPOSED STANDARD")){
    output$status <- "Proposed standard"
  }
  
  obs <- title_nodes[grepl(x = title_nodes, pattern = "Obsoleted by")]
  if(length(obs) > 0){
    output$obsoleted_by <- split_and_num(gsub(x = obs[1],pattern = "[a-zA-Z :;]", 
                                              replacement = ""))
  }
  
  ups <- title_nodes[grepl(x = title_nodes, pattern = "Updated by")]
  if(length(ups) > 0){
    output$updated_by <- split_and_num(gsub(x = ups[1], pattern = "[a-zA-Z :;]", replacement = ""))
  }
  return(output)
  
}