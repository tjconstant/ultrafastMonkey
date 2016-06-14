#' Read Averaged Time Dynamics Output
#'
#' @param filename 
#' @param time_max 
#' @param time_offset 
#' @param zero_offset 
#' @param measurement_type 
#' @param lock_in_mV 
#' @param quick_plot 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
read.delay_scan<-function(filename,
                          time_max=4,
                          time_offset=0,
                          zero_offset=1:10,
                          measurement_type="%",
                          lock_in_mV=100,
                          quick_plot=T,
                          ...){
  
  
  data<-read.table(filename)$V1
  time<-seq(0,time_max,,length(read.table(filename)$V1))-time_offset
  
  data_offset<-data-mean(data[zero_offset])
  
  if(measurement_type=="%"){
    scale_factor<-100
  }else if(measurement_type=="abs"){
    scale_factor<-1
  }else{
    stop("Unknown measurement type. Acceptable values are '%' or 'abs'")
  }
  
  data_offset<-lock_in_mV*data_offset/scale_factor
  
  if(quick_plot){
    plot(time,data_offset,xlab="time (ps)",ylab="signal (mV)",pch=16,...);grid();lines(time,data_offset,...)
  }
  
  return(data.frame(time=time,signal=data_offset))
  
}