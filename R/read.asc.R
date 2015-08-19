read.asc<-function(file,
                   acquisition.appended=T,
                   quick_plot=T){
  
  file.in<-read.csv(file,header=F,stringsAsFactors =F)
  
  data<-file.in[1:min(which(grepl(pattern="Time",file.in[,1])))-1,]
  details<-as.character(file.in[min(which(grepl(pattern="Time",file.in[,1]))):length(file.in[,1]),1])

  scan_type<-gsub(pattern = "  ",replacement = "",strsplit(details[6],split = ":")[[1]][2])
  
  if(scan_type==" Single Scan" || scan_type==" Accumulate" || scan_type==" Real Time"){
    
    message("Scan Type: Single/Accumulate")
    
    names(data)<-c("x","y")
    
    output<-list(data=data.frame(x=as.numeric(data$x),y=as.numeric(data$y)),details=details)
    
    if(quick_plot){
      
#       library(ggplot2)
#       
#       p <- ggplot(output$data,aes(x,y))+
#         scale_x_continuous(expand=c(0,0))+
#         geom_line()+
#         xlab(label_wavelength(si_prefix="n"))+theme_bw()+
#         ylab(gsub(pattern = "  ",replacement = "",strsplit(details[5],split = ":")[[1]][2]))
#       print(p)
      
      return(fit.asc(output))
      
    }
  }else if(scan_type==" Kinetics"){
    
    message("Scan Type: Kinetics")
    
    melt<-reshape2::melt
    
    number_kinetic_steps<-as.numeric(gsub(pattern = "  ",replacement = "",strsplit(details[13],split = ":")[[1]][2]))
    cycle_time<-as.numeric(gsub(pattern = "  ",replacement = "",strsplit(details[11],split = ":")[[1]][2]))
    
    x<-as.numeric(data[,1])
    time<-seq(0,cycle_time*number_kinetic_steps,,number_kinetic_steps)
    suppressMessages(y<-melt(data[,2:(number_kinetic_steps+1)])[,2])
    
    output<-list(data=data.frame(x,time=rep(time,each = 1024),y),details=details)
  }
  
  
  return(output)
}