#' Ultrafast Fluence Calculation
#'
#' @param power_mW 
#' @param freq_khz 
#' @param spot_diameter_microns 
#' @param beam_angle_deg 
#'
#' @return
#' @export
#'
#' @examples
ultrafast_fluence<-function(power_mW=1,freq_khz=1.05, spot_diameter_microns=100,beam_angle_deg=60){
  
  projected_spot_diameter_microns <- spot_diameter_microns/cos(beam_angle_deg*pi/180)
  total_beam_area<- pi*(spot_diameter_microns*1e-6/2)*(projected_spot_diameter_microns*1e-6/2)
  
  j_m2<-(power_mW*1e-3/(freq_khz*1e3))/(total_beam_area)
  mj_cm2 <- j_m2/10
  j_cm2 <- mj_cm2/100
  
  if(mj_cm2 >=2 & mj_cm2<5) warning("Fluence is above photo-modification threshold of graphene.")
  if(mj_cm2 >=5) warning("Fluence is above damage threshold of graphene.")
  
  return(data.frame(j_m2,mj_cm2,j_cm2))
}