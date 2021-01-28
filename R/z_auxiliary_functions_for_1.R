# transform relative outdoor humidity and temperature to indoor humidity
qfun <- function(humidity, temperature, P=10^5, maxq=8){
  rh <- humidity/100
  t <- temperature+273.15
  resq <- min(maxq, 1000*5/8*1/P*rh*611.2*exp(17.62*(t-273.15)/(t-30.03)))
  return(resq)
}

qlevel <- function(humidity, temperature, P=10^5, cutoffq=6){
  rh <- humidity/100
  t <- temperature+273.15
  resq <- 1000*5/8*1/P*rh*611.2*exp(17.62*(t-273.15)/(t-30.03))
  rlevel <- ifelse(resq<cutoffq, 0, 1)
  return(rlevel)
}