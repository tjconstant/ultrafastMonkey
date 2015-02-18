read.asc<-function(file,
                   acquisition.appended=T,
                   quick_plot=T){
  
  file.in<-read.csv(file,header=F,stringsAsFactors =F)
  
  data<-file.in[1:min(which(grepl(pattern="Time",file.in[,1])))-1,]
  
  names(data)<-c("x","y")
  
  details<-as.character(file.in[min(which(grepl(pattern="Time",file.in[,1]))):length(file.in[,1]),1])
  output<-list(data=data.frame(x=as.numeric(data$x),y=as.numeric(data$y)),details=details)
  
  if(quick_plot){
    p <- ggplot(output$data,aes(x,y))+
      scale_x_continuous(expand=c(0,0))+
      geom_line()+
      xlab(label_wavelength(si_prefix="n"))+theme_bw()+
      ylab(gsub(pattern = "  ",replacement = "",strsplit(a$details[5],split = ":")[[1]][2]))
    print(p)
  }
  
  return(output)
}

