#' Plotting of irregular spaced matrix data with aribitary transformations.
#'
#' @description Polygon 2D plotting of irregular spaced matrix data with aribitary transformations.
#' @param x 
#' @param y 
#' @param z 
#' @param fx 
#' @param fy 
#' @param nx 
#' @param ny 
#' @param method 
#' @param return_data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 

disp.plot<-function( x,
                     y,
                     z,
                     fx=nothing_x,
                     fy=nothing_y,
                     nx=length(unique(x)),
                     ny=length(unique(y)),
                     method="bilinear",
                     return_data=F,
                     ...){
  
  interp_grid<-disp.grid(x,y,z,nx,ny,method)
  
  x<-interp_grid$x
  y<-interp_grid$y
  z<-interp_grid$z
  
  if(return_data){
    return(list(fx=fx(x,y),fy=fy(x,y),z=z))
  }else{
    image.plot<-fields::image.plot
    image.plot(fx(x,y),fy(x,y),z,xaxs='i',yaxs='i',...)
  }
  
}


nothing_x<-function(x,y) return(x)
nothing_y<-function(x,y) return(y)