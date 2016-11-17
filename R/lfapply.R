#' Apply a Function over a long-form dataset column by variable. 
#'
#' @param df 
#' @param col_expr 
#' @param col_n 
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
#' data <- SPPdispersion
#' 
#' avg <- lfapply(data, reflection, angle, mean)
#' se <- lfapply(data, reflection, angle, sd) / sqrt(20)
#' ymin <- avg - se
#' ymax <- avg + se
#' 
#' result <- tibble(step = 1:225, avg, se, ymin, ymax)
#' ggplot(result, aes(step, avg, ymin = ymin, ymax = ymax)) + geom_line() +
#'   geom_errorbar()+theme_bw()


lfapply <- function(df, col_expr, col_n, fn = mean) {
  
  data <- eval(substitute(col_expr), df, parent.frame())
  
  if (!is.name(substitute(col_n))) {
    nrow <- col_n
  } else{
    col_n <- deparse(substitute(col_n))
    nrow <- df[[col_n]] %>% unique %>% length
  }
  
  values <-
    data %>%
    matrix(nrow = nrow) %>%
    apply(2, fn)
  
  return(values)
  
}



