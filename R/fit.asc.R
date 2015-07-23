fit.asc <-function(asc){
  
  kable<-knitr::kable
  
  fit<-nls(asc$data$y~max(asc$data$y)*exp(-(asc$data$x - m)^2/(2 * s^2)), 
           data=asc$data, 
           start = list(s = 3, m = asc$data$x[which(asc$data$y==max(asc$data$y))]))
  
  par<-fit$m$getAllPars()
  
  plot(asc$data,pch=16,xlab=label_wavelength(si_prefix = "n"),ylab="counts")
  
  plot.function(function(x) max(asc$data$y)*exp(-(x - par[2])^2/(2 * par[1]^2)),from=350,to=950,n=800,add=T,col=2,lwd=2)
  
  abline(v=par[2],col=2,lty=2)
  
  kable(data.frame(lambda_0=par[2],sd=par[1],fwhm=2*sqrt(2*log(2))*par[1]))
  
  return(data.frame(lambda_0=par[2],sd=par[1],fwhm=2*sqrt(2*log(2))*par[1]))
}