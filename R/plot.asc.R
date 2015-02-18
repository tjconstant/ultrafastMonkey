plot.asc <- function(filename){
  
  spectra <- read.asc(filename)
  
  p <- ggplot(spectra$data,aes(x,y))+
    scale_x_continuous(expand=c(0,0))+
    geom_line()+
    xlab(label_wavelength(si_prefix="n"))+
    theme_pm()
  
  return(p)

}