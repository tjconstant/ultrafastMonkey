load <- c("tidyverse","disp.plot")

.onAttach <- function(...) {
  needed <- load[!is_attached(load)]
  
  if (length(needed) == 0)
    return()
  
  if(!("disp.plot" %in% rownames(installed.packages()))) devtools::install_github("tjconstant/disp.plot")
  
  #packageStartupMessage(paste0("Loading: ", needed, collapse = "\n"))
  lapply(needed, library, character.only = TRUE, warn.conflicts = FALSE)
  
  
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}