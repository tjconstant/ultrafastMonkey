four_wave_mixing <-
  
  function(lambda.1=NA,
           lambda.2=NA,
           lambda.3=NA,
           difference.frequency=NA,
           theta.1=NA,
           theta.2=NA,
           theta.3=NA) {
    
    c0<-3e8
    
    lambda.1 <- lambda.1*1e-9
    lambda.2 <- lambda.2*1e-9
    lambda.3 <- lambda.3*1e-9
    difference.frequency <- difference.frequency*1e12
    theta.1 <- theta.1*pi/180
    theta.2 <- theta.2*pi/180
    theta.3 <- theta.3*pi/180
    
    if(any(!is.na(lambda.3)&!is.na(difference.frequency)&is.na(lambda.1)&is.na(lambda.2))){
    #lambda.1 and lambda.2 calculation
        lambda.2<-c0/((c0/lambda.3)-2*difference.frequency)
        lambda.1<-c0/(difference.frequency+(c0/lambda.2))
    }
    
    if(any(!is.na(lambda.1)&!is.na(lambda.2)&is.na(lambda.3))){
      #lambda.3 calculation
      lambda.3<-(2/lambda.1-1/lambda.2)^-1
      
    }
    
    if(any(!is.na(lambda.1)&!is.na(lambda.2)&is.na(difference.frequency))){
      
      difference.frequency<-(c0/lambda.1-c0/lambda.2)
      
    }
    
    if(any(!is.na(lambda.1)&!is.na(lambda.1)&!is.na(theta.1)&!is.na(theta.2))){

    theta.3<-
      asin(
    (2/lambda.1-1/lambda.2)^-1*(
      (
        (2/lambda.1)*sin(theta.1)
      )-
        (
        (1/lambda.2)*sin(theta.2)
        )
    )
    )*180/pi
    }
    
    if(any(is.na(lambda.1) | is.na(lambda.2) | is.na(lambda.3) | is.na(difference.frequency))){
      warning("Insufficent number of parameters supplied for calculations")
    }
    
    return(
    data.frame(lambda.1=lambda.1*1e9,
               lambda.2=lambda.2*1e9,
               lambda.3=lambda.3*1e9,
               difference.frequency=difference.frequency*1e-12,
               theta.1=theta.1*180/pi,
               theta.2=theta.2*180/pi,
               theta.3=theta.3)
    )
  }

