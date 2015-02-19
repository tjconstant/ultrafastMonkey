disp.grid<-function(x,
                    y,
                    z,
                    nx=length(unique(x)),
                    ny=length(unique(y)),
                    method="bilinear"){
  
  if(x[2]-x[1]==0 & y[2]-y[1]!=0 & method=="bicubic"){
    warning("x and y values are in unexpected order. Try swapping x & y.")
  }
  
  if(method=="bilinear"){
    
    interp<-akima::interp
    int_z<-interp(x,y,z,xo=seq(min(x),max(x),length=nx),yo=seq(min(y),max(y),length=ny),linear=T,duplicate = "mean")
    
    x2<-matrix(int_z$x,nrow=nx,ncol=ny)
    y2<-matrix(int_z$y,nrow=nx,ncol=ny,byrow=T)
    z2<-int_z$z
    
    message("Interpolation method: bilinear")
    
  } else if(method=="bicubic"){
    
    bicubic.grid<-akima::bicubic.grid
    
    z.matrix <- matrix(z,ncol = length(unique(y)))
    
    int_z <- bicubic.grid(x=seq(min(x),max(x),length=length(unique(x))),
                          y=seq(min(y),max(y),length=length(unique(y))),
                          z = z.matrix,
                          xlim = c(min(x),max(x)),
                          ylim=c(min(y),max(y)),
                          dx = (max(x)-min(x))/(nx),
                          dy = (max(y)-min(y))/(ny)
    )
    
    x2<-matrix(int_z$x,nrow=nx+1,ncol=ny+1)
    y2<-matrix(int_z$y,nrow=nx+1,ncol=ny+1,byrow=T)
    z2<-int_z$z
    
    message("Interpolation method: bicubic")
    
  } else {
    
    stop("Unrecognised interpolation function. Valid methods are 'bilinear' or 'bicubic'")
    
  }
  
  return(list(x=x2,y=y2,z=z2))
}