#' @title Approximate string matching
#' 
#' Suggestions for matches of strings from a reference character vector, based on
#' approximate string distances
#'
#' @param x a character vector to be matched
#' @param reference a character vector to search for matches in
#' @param max.suggestions the maximum number of suggestions to give
#' @export
#' @return a list with 2 matrices: \code{suggestions} containing the suggestions and 
#' \code{dist} containing the respective distances
#' 
#' @examples
#' x<-c("France", "Luxbg", "Spain", "Gb")
#' reference<-c("France", "Luxembourg")
#' max.suggestions<-3 
#' amatch(x, reference, max.suggestions)
#' 
amatch<-function(x, reference, max.suggestions=3){
  a.dist<-utils::adist(x,reference)
  
  m.order<- t(apply(a.dist, 1, order))
  m.order<-m.order[, 1:min(ncol(m.order), max.suggestions), drop=FALSE] 
  
  m.dist<-t(apply(a.dist, 1, sort))
  m.dist<-m.dist[, 1:min(ncol(m.dist), max.suggestions), drop=FALSE] 
  
  m.suggestions <- apply(m.order, 2, function(t)reference[t])
  
  list(suggestions = m.suggestions,
       dist = m.dist)
}



