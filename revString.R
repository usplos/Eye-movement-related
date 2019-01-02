revString <- function(text){
  paste(rev(unlist(strsplit(text,NULL))),collapse="")
}
