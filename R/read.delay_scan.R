read.delay_scan<-function(filename,
                          time_max=4,
                          time_offset=0,
                          zero_offset=1:10,
                          quick_plot=T,
                          ...){
  
  
  data<-read.table(filename)$V1
  time<-seq(0,time_max,,length(read.table(filename)$V1))-time_offset
  
  data_offset<-data-mean(data[zero_offset])
  
  if(quick_plot){
    plot(time,data_offset,xlab="time (ps)",pch=16,...);grid();lines(time,data_offset,...)
  }
  
  return(data.frame(time=time,signal=data_offset))
  
}