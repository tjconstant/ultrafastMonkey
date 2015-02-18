read.asc<-function(file,acquisition.appended=T){
  file.in<-read.csv(file,header=F,stringsAsFactors =F)
  data<-file.in[1:min(which(grepl(pattern="Time",file.in[,1])))-1,]
  names(data)<-c("x","y")
  details<-as.character(file.in[min(which(grepl(pattern="Time",file.in[,1]))):length(file.in[,1]),1])
  output<-list(data=data.frame(x=as.numeric(data$x),y=as.numeric(data$y)),details=details)
  return(output)
}

