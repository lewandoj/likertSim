#Hlper functions for analysis
getMeanFromM <- function(simIndex, timePeriod = "ut1Freq") {
  getSimNum <- m$df$simNum==simIndex
  
  d <- rep(x = m$df$bins[getSimNum], 
           times = m$df[[timePeriod]][getSimNum])
  
  mean(d)
}

