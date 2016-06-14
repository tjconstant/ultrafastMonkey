#' Scientific Labelling
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
label_scientific10 <- function(x) {
  
  scientific_format<-scales::scientific_format
  
  a<-gsub("e", " %*% 10^", scientific_format()(x))
  b<-gsub("\\+","",a)
  b[x==0]<-"0"
  
  return(parse(text=b))
}
