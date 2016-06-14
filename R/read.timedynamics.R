#' Time Dynamics Plot
#' @author Tom Constant
#' @description Plot from time dynamics kit dataset, providing mean and standard error for a given number of averages
#' @param filename 
#' @param av 
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
#' 

read.timedynamics<-function(filename, av,time_max = 4, time_offset = 0, zero_offset = 1:10, 
          measurement_type = "%", lock_in_mV = 100, quick_plot = T, 
          ...) 
{
  
  data <- read.table(filename)$V1
  
  time <- seq(0, time_max, , length(data)/av) - 
    time_offset
  
  if (measurement_type == "%") {
    scale_factor <- 100
  }
  else if (measurement_type == "abs") {
    scale_factor <- 1
  }
  else {
    stop("Unknown measurement type. Acceptable values are '%' or 'abs'")
  }
  data_offset <- lock_in_mV * data/scale_factor

  data_m <- matrix(data_offset,ncol=av)
  
  data_mean <- apply(data_m,1,mean)
  data_sd <- apply(data_m,1,sd)
  data_se <- data_sd/sqrt(av)
  
  data_offset<-data_mean
  
  data_final <- data_offset - mean(data_offset[zero_offset])
  
  if (quick_plot) {
    plot(time, data_final, xlab = "time (ps)", ylab = "signal (mV)", 
         pch = 16, pty='n', ...)
    grid()
    
    
    arrows(time,
           data_final-data_se,
           time,
           data_final+data_se,
           length=0.05, angle=90, code=3,col='lightgrey')
    
    points(time,data_final,pch=16)
    lines(time, data_final, ...)
    
  }
  return(data.frame(time = time, signal = data_final, ymin= data_final-data_se,ymax= data_final+data_se))
}
