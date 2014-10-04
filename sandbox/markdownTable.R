#' try with knitr
library(knitr)
kable(head(iris[,rep(1:3,1)]), format = "markdown")


#'
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

string<-"

| Sepal.Length| Sepal.Width| Petal.Length|
|------------:|-----------:|------------:|
|          5.1|         johns|          1.4|
|          4.9|         3.0|          1.4|
|          4.7|         15.33|          1.3|
|          4.6|         3.1|          1.5|
|          5.0|         3.6|          1.4|
|          1115.4|         3.9|          1.7|
"
(test <- markdownTableToDf(string))
