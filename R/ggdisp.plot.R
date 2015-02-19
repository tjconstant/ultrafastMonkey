ggdisp.plot<-function(x,                     # x vector
                      y,                     # y vector
                      z,                     # z vector
                      fx=nothing_x,
                      fy=nothing_y,
                      nx=length(unique(x)),
                      ny=length(unique(y)),
                      method="bilinear",
                      midpoint=FALSE,
                      return_data=F,
                      ...){
  
  interp_grid<-disp.grid(x,y,z,nx,ny,method)
  
  x<-interp_grid$x
  y<-interp_grid$y
  z<-interp_grid$z
  
  # Function to plot non-rectangular shaped polygons for ireggular spaced data in ggplot2.
  # Adapted from field package poly.image() function to return a ggplot2 object.
  
  poly.image.regrid<-fields::poly.image.regrid
  
  x2<-fx(x,y)
  y2<-fy(x,y)
  
  Dx <- dim(x2)
  Dy <- dim(y2)
  if (any((Dx - Dy) != 0)) {
    stop(" x and y matrices should have same dimensions")
  }
  Dz <- dim(z)
  if (all((Dx - Dz) == 0) & !midpoint) {
    x <- poly.image.regrid(x2)
    y <- poly.image.regrid(y2)
  }
  
  N <- ncol(x2)
  Nm1 <- N - 1
  M <- nrow(x2)
  Mm1 <- M - 1
  
  xps<-c()
  yps<-c()
  zps<-c()
  ids<-c()
  
  for (i in (1:Mm1)) {
    xp <- cbind(x2[i, 1:Nm1], x2[i + 1, 1:Nm1], x2[i + 1, 2:N], 
                x2[i, 2:N], rep(NA, Nm1))
    yp <- cbind(y2[i, 1:Nm1], y2[i + 1, 1:Nm1], y2[i + 1, 2:N], 
                y2[i, 2:N], rep(NA, Nm1))
    
    id<-i*(length(rep(rep(1:Nm1),each=5))+1)+rep(rep(1:Nm1),each=5)
    xp <- c(t(xp))
    yp <- c(t(yp))
    
    xps<-c(xps,xp)
    yps<-c(yps,yp)
    ids<-c(ids,id)
    
    pcol <- c(z[i, 1:Nm1])
    zp<-rep(pcol,each=5)
    zps<-c(zps,zp)
    
  }
  
  ggdataframe<-data.frame(ids,xps,yps,zps)
  
  require(ggplot2)
  
  p <- ggplot(ggdataframe, aes(x=xps, y=yps))+
    geom_polygon(aes(fill=zps,color=zps,group=ids))+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
#     scale_color_gradientn(colours=col,name="R")+
#     scale_fill_gradientn(colours=col,name="R")+
#     theme_bw()+
    xlab("x")+
    ylab("y")
  
if(return_data){
  return(ggdataframe)
}else{
  return(p)
}
}

