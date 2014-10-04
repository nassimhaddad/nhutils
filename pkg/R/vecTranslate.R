#' Utilities linked to the translation of text in a column.
#' Typically needed when merging two datasets on columns that need to
#' be harmonized first.



#' Reads a data.frame from a markdown-formatted table in a string
#'
#' @param string contains a table formatted in markdown
#' @export
#'
#' @examples
#' \dontrun{
#' string<-"
#'
#'| Sepal.Length| Sepal.Width| Petal.Length|
#'|------------:|-----------:|------------:|
#'|          5.1|         johns|          1.4|
#'|          4.9|         3.0|          1.4|
#'|          4.7|         15.33|          1.3|
#'|          4.6|         3.1|          1.5|
#'|          5.0|         3.6|          1.4|
#'|          1115.4|         3.9|          1.7|
#'"
#'(test <- read.markdown.table(string))
#'}
read.markdown.table <- function(string){
  require(stringr)
  temp <- (strsplit(string, "\n")[[1]])
  temp <- temp[sapply(temp,nchar)>0]
  temp <- strsplit(temp, "\\|")
  temp <- lapply(temp, function(x)str_trim(x[-1]))
  title_row <- temp[[1]]
  temp[[1]]<-NULL
  temp[[1]]<-NULL
  if (length(unique(sapply(temp, length)))>1){
    stop("row length differ")
  }
  temp <- as.data.frame(do.call(rbind, temp))
  colnames(temp)<-title_row
  # turn into numeric if it contains numbers
  for (currentCol in seq(temp)){
    if (suppressWarnings(sum(is.na(as.numeric(as.character(temp[, currentCol]))))==0)){
      temp[, currentCol] <- as.numeric(as.character(temp[, currentCol]))
    }
  }
  return(temp)
}





#' Function 
#' 
#' 
#' Note: Parts of the code copied from Christopher Gandrud's DataCombine package.
#' @export
#' @examples
#' \dontrun{
#' vec<-c("France", "Luxbg", "Spain", "Gb")
#' dict <- data.frame(from = c("Luxbg", "Gb"),
#'                    to = c("Luxembourg", "Great Britain"))
#' vec.translate(vec, dict, exact=T)
#'}
vec.translate <- function(vec, dict, exact = TRUE){
  if (!(class(vec) %in% c("character", "factor"))) {
    stop("vec is not a character string or factor. Please convert to a character string or factor and then rerun.")
  }
  
  if (!(ncol(dict) == 2)&("data.frame" %in% class(dict))){
    stop("The provided dictionary must be a data.frame with 2 columns.")
  }
  
  ReplaceNRows <- nrow(dict)
  for (i in 1:ReplaceNRows) {
    if (isTRUE(exact)) {
      vec <- gsub(pattern = paste0("^", dict[i, 1], "$"), replacement = dict[i, 2], 
                  vec)
    }
    if (!isTRUE(exact)) {
      vec <- gsub(pattern = dict[i, 1], 
                  replacement = dict[i, 2], vec)
    }
  }
  return(vec)
}




#' Translate a vector using a dictionary table in a markdown string
#' 
#' @export
#' @examples
#' \dontrun{
#' vec.translate.mdstring(vector)
#'dict.md="
#'|from   |to     |
#'|:------|:------|
#'|France |France |
#'|Luxbg  |Luxbg  |
#'|Spain  |Spain  |
#'|Gb     |Great Britain     |
#'"
#'vec.translate.mdstring(vector, dict.md)
#'}
vec.translate.mdstring <- function(vec, dict.md){
  if (!(class(vec) %in% c("character", "factor"))) {
    stop("vec is not a character string or factor. Please convert to a character string or factor and then rerun.")
  }
  
  if (missing(dict.md)){
    require(knitr)
    cat("No dictionary specified. You can use the below table as a template:\n\n")
    dict.raw<-data.frame(from=unique(vec),
                         to=unique(vec))
    cat('dict.md="\n')
    dict.md <- kable(dict.raw, format = "markdown", output=F)
    for (txt in dict.md){cat(txt);cat("\n")}
    cat('"')
    call<-as.character(sys.call())
    cat(paste0("\n", call[1], "(", call[2], ', dict.md)'))
    
    return(invisible(vec))
  } else{
    
    if (!((class(dict.md) %in% c("character")))&length(dict.md)==1) {
      stop("dict.md is not a character string of length 1. Please provide a markdown table as a character string and then rerun.")
    }
    
    dict <- read.markdown.table(dict.md)
    return(vec.translate(vec, dict, exact=T))
    
  }
}



#' Translate a vector using a dictionary table in a csv file.
#'
#' @examples
#' \dontrun{
#'vec.translate.csv(vector, file="dict.csv")
#'}
vec.translate.csv <- function(vec, file){
  if (!(class(vec) %in% c("character", "factor"))) {
    stop("vec is not a character string or factor. Please convert to a character string or factor and then rerun.")
  }
  
  if (!file.exists(file)){
    cat(sprintf("Dictionary file doesn't exist. Creating template as %s.\n", file))
    dict.raw<-data.frame(from=unique(vec),
                         to=unique(vec))
    write.csv(dict.raw, file=file, row.names=F)
    return(invisible(vec))
  } else{
    dict <- read.csv(file)
    return(vec.translate(vec, dict, exact=T))
  }
}




#' Translate a vector using a dictionary table in a markdown file.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'vec.translate.md(vector, file="dict.md")
#'}
vec.translate.md <- function(vec, file){
  if (!(class(vec) %in% c("character", "factor"))) {
    stop("vec is not a character string or factor. Please convert to a character string or factor and then rerun.")
  }
  
  if (!file.exists(file)){
    require(knitr)
    cat(sprintf("Dictionary file doesn't exist. Creating template as %s.\n", file))
    dict.raw<-data.frame(from=unique(vec),
                         to=unique(vec))
    dict.md <- kable(dict.raw, format = "markdown", output=F)
    writeLines(dict.md, file)
    return(invisible(vec))
  } else{
    dict.md <- readChar(file, file.info(file)$size)
    dict <- read.markdown.table(dict.md)
    return(vec.translate(vec, dict, exact=T))
  }
}


