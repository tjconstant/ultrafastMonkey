label_in_plane_wavevector<-function(subscript="x",si_prefix="phantom()"){
parse(text=
          paste("list('in'-plane~momentum,~italic(k)[",subscript,"]~(",si_prefix,"*m^-1))",sep="")
        )
}

label_angular_frequency<-function(){
  expression(paste("angular frequency, ", 
                   omega, " (rad ", s^-1, ")"))
}

label_polar_angle<-function(unit="degree"){
  parse(text=paste("list(polar~angle,~theta~(",unit,"))"))
}

label_azimuthal_angle<-function(unit="degree"){
  parse(text=paste("list(azimuthal~angle,~phi~(",unit,"))"))
}

label_reflection<-function(subscript="pp"){
  parse(text=paste("italic(R)[",subscript,"]",sep=""))
}

label_transmission<-function(subscript="pp"){
  parse(text=paste("italic(T)[",subscript,"]",sep=""))
}

label_wavelength<-function (si_prefix = "phantom()", subscript = "phantom()") 
{
  return(parse(text = paste("list(wavelength,~lambda[", subscript, "]~(", si_prefix, 
                            "*m))", sep = "")))
}


label_frequency<-function(si_prefix="phantom()"){
  return(parse(text=paste("list(frequency,~italic(f)~~(",si_prefix,"*Hz))",sep="")))
}