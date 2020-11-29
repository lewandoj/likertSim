#Analysis 1
#This simulation quantifies the bias created when using eqidistant Likert labels

#Process:
#1. Create a distribution ranging from 1-100, representing underlying attitudes at Time 1 (ut1)
#2. Add a 10% increase to ut1, representing a shift in attitude by 10% (ut2)
#3. Create 5 equidistant distributions (t1-t5), representing the theoretical likert distributions
#4. Compute the probability that a given value of ut1/ut2 belongs to t1-t5, representing how survey
# respondants match their underlying attitude with a label on a likert scale
#5. Tally the data assigned to each t1-t5 distribution for ut1 and ut2, called Lt1 and Lt2
#6. Compute the % difference between the original data (ut1/ut2) and the simulated data (Lt1/Lt2)
#7. Rerun step 6 N times to create a distribution of % differences. We would expect this to
# center around 0 if ther was no bias. But we find about a -30% difference between the actual effect
# between ut1/ut2 and Lt1/Lt2. 

library(ggplot2)
library(ggalt)
library(tidyr)
source("createLikertData.R")

#Estimate overall bias
#conclusion: median: -17, mean: -99
summary(m$effectSizes)
hist(m$effectSizes, breaks = 200, xlim = c(min(m$effectSizes), max(m$effectSizes)))



MeansandEffectSizebySim <- function() {
  d <- m$df[,c(1:3, 21)] #isolate relevant data cols m
  es <- data.frame(simNum = 1:length(m$effectSize), effectSize = m$effectSizes)
  
  convertBinsToVector <- function() {
    ut1Vecorized <- rep(x = d$bins, times = d$ut1Freq)
    ut1SimNumVectorized <- rep(x = d$simNum, times = d$ut1Freq)
    d1 <- cbind(ut1Vecorized, ut1SimNumVectorized, "ut1")
    
    ut2Vecorized <- rep(x = d$bins, times = d$ut2Freq)
    ut2SimNumVectorized <- rep(x = d$simNum, times = d$ut2Freq)
    d2 <- cbind(ut2Vecorized, ut2SimNumVectorized, "ut2")
    
    dlong <- rbind(d1, d2)
    dlong <- as.data.frame(dlong)
    names(dlong) <- c("data", "simNum", "timePeriod")
    dlong$data <- as.numeric(dlong$data)
    dlong$simNum <- as.numeric(dlong$simNum)
    dlong$timePeriod <- as.factor(dlong$timePeriod)
    
    
    return(dlong)
  }
  
  j <- merge(convertBinsToVector(), es, by = "simNum")
  aggregate(.~simNum+timePeriod, j, mean)
}



#Plots
plotDumbbellMeanByEffectSize <- function(df = dwide) {
  #df$effectSize <- with(df, reorder(effectSize, effectSize))
  df$effectSize <- round(as.numeric(as.character(df$effectSize)), 3)
  ggplot(df[df$simNum,], aes(x = ut1, xend = ut2, y = effectSize)) +
    geom_dumbbell(size = .1) + ylim(-200, 200) + geom_vline(xintercept = c(16, 33, 50, 66, 83)) +
    geom_hline(yintercept = 0)
  
}

plotMeanDiffbyEffectSize <- function(df = dwide) {
  df$ut2Minusut1 <- df$ut2 - df$ut1
  
  ggplot(df, aes(x = ut1, y = effectSize)) +
    geom_point(size = 1) + ylim(-500, 500)
  
}


#Main
#Setup df. 1 row per simulation. Cols: EffectSize, ut1 Mean, ut2 Mean
dlong <- MeansandEffectSizebySim()
dwide <- spread(dlong, timePeriod, data)

#get Dumbbell plot. Mean values by effect size
plotDumbbellMeanByEffectSize(dwide)
plotMeanDiffbyEffectSize(dwide)


#TODO: Should I add a count of the likert items per sim
#TODO Can I calulate the distance from the closest t dist mean? 
  #e.g., abs(ut1 - t1 mean). Maybe do this in the first function in this file.
#it's about the total distance from the nearest peak. 
