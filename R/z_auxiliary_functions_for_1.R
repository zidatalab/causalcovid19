# transform relative outdoor humidity and temperature to indoor humidity
qfun <- function(humidity, temperature, P=10^5, maxq=8){
  rh <- humidity/100
  t <- temperature+273.15
  resq <- 1000*min(maxq, 5/8*1/P*rh*611.2*exp(17.62*(t-273)/(t-30)))
  return(resq)
}
