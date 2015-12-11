fit_gaussian<-function(x,y,s=30,m=NA){
  
  if(is.na(m)){
    m<-x[y==max(y)]
  }
  
  fit<-nls(y~max(y)*exp(-(x - m)^2/(2 * s^2)),
           data=data.frame(x,y),
           start = list(s=s, m=m))
  
  par<-fit$m$getAllPars()
  
  gaussian<-function(x) max(y)*exp(-(x - par[2])^2/(2 * par[1]^2))
  
  return(list(data=data.frame(x,y,fit=gaussian(x)),sd=par[1],x_offset=par[2]))
  
}
