delay_scan_live <- function(file,steps,delay){
  
  par(mfcol = c(2,1),mar=c(4,4,1,1))
  
  while(1>0){
    
    data<-read.table(file)$V1
    
    complete <- floor(length(data)/steps)
    remaining <- (length(data)%%steps)
    
    plot(seq(0,remaining),
         data[(complete*steps):(complete*steps+remaining)],
         col='red',type='l',xlab="steps",ylab="signal",xlim=c(0,steps))
    points(remaining,
           data[complete*steps+remaining],
           col=2,pch=16,cex=1)
    text(x=max(steps),y = min(data[(complete*steps):(complete*steps+remaining)]),paste("Av. No.",complete),pos = 2)
    
    data_average<-rowMeans(matrix(data[1:(complete*steps)],nrow=steps))
    
    plot(data_average,pch=16,xlab="steps",ylab="average signal",cex=0.5)
    
    lines(data_average,pch=16)


    #abline(v = remaining,col=2)
    
    Sys.sleep(delay)
    
  }
  
}




