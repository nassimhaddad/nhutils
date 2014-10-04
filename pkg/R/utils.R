
#' similar as write.csv(), but writes a markdown table into a text file
#' 
#' @param df a data frame
#' @param file a file to write to
#' 
write.md<-function(df, file = ""){
  df.md<-knitr::kable(df, format = "markdown", output=F)
  writeLines(df.md, file)
  return(invisible(NULL))
}

#' @title reads a markdown table from a file as a data frame
#' @param file to read from
#' @export
read.md<-function(file){
  dict.md <- readChar(file, file.info(file)$size)
  read.markdown.table(dict.md)
}