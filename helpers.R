library(psych)

#Hlper functions for analysis
getMeanFromM <- function(simIndex, timePeriod = "ut1Freq") {
  getSimNum <- m$df$simNum==simIndex
  
  d <- rep(x = m$df$bins[getSimNum], 
           times = m$df[[timePeriod]][getSimNum])
  
  mean(d)
}

#FREQ2CASE 
# Takes freqency table (bins and freqs) and elongates it to individual cases (bins x freq)

#Args:
# data: Takes m$df data frame
# sim.num: 1 or multiple integers. ex: 1; c(1, 2)
# freq.col: string of column name or multiple column names to create a long data set. Must contain values >1
freq2Case <- function(data = m$df, sim.num, freq.col) {
  df <- data[data$simNum %in% sim.num,]
  #df <- data[data$simNum == sim.num,] old, should delete
  df.long <-  setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("data", "source", "simNum"))
  
  for (each.col in freq.col) {
    get.col.index <- which(colnames(df) == each.col)
    get.simNum.length <- aggregate(df[each.col], by = list(df$simNum), FUN = sum)
    
    cases.vector <- rep(df$bins, df[, c(get.col.index)])
    df.vectorized <- data.frame(data = cases.vector, 
                                source = each.col, 
                                simNum = rep(sim.num, times = get.simNum.length[,2]))
    df.long <- rbind(df.long, df.vectorized)
  }
  
  #need to add a label from which sim this data comes from so I can aggregate by it.
  return(df.long)

}

#WIDE2LONG
# Takes multiple columns and makes them into a long format, labeling each case from its column

#Args:
# sim.num: single integer or range of integers to subset data
# freq.col: string of column name or multiple column names to create a long data set. Must contain values >1
wide2long <- function(data = m$df, sim.num, freq.col) {
  df <- data[data$simNum == sim.num,]
  df.long <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("data", "source"))
  
  for (each.col in freq.col) {
    get.col.index <- which(colnames(df) == each.col)
    df.vectorized <- data.frame(data = df[, get.col.index], source = each.col)
    df.long <- rbind(df.long, df.vectorized)
  }
  return(df.long)
}

#Can probably delete this whole function. Incorporated into main thread loop now.
testUvsLikert <- function(data = m$df, sim.num) {
  df <- data[data$simNum == sim.num,]
  
  ut1 <- data.frame(data = rep(df$bins, df$ut1Freq), cond = "ut1")
  ut2 <- data.frame(data = rep(df$bins, df$ut2Freq), cond = "ut2")
  ut1Predicted <- data.frame(data = na.omit(unlist(df$ut1Assign)), cond = "ut1Predicted")
  ut2Predicted <- data.frame(data = na.omit(unlist(df$ut2Assign)), cond = "ut2Predicted")
  l <- list(ut1, ut2, ut1Predicted, ut2Predicted)
  d <- do.call(rbind, l)
  
  #Effect size (% change): actual (ut1/ut2) vs. Predicted (ut1Predicted/ut2Predicted)
  d$source <- ifelse(grepl("Predicted", d$cond), "Likert", "Underlying")
  df.likert <- d[d$source=="Likert",]
  df.underlying <- d[d$source=="Underlying",]
  
  cohenD.underlying <- cohen.d(df.underlying[ ,1:2], group =  "cond")
  cohenD.likert <- cohen.d(df.likert[ ,1:2], group =  "cond")
  cohenD.difference <- cohenD.underlying$cohen.d - cohenD.likert$cohen.d
  combined <- rbind(cohenD.underlying$cohen.d, cohenD.likert$cohen.d, cohenD.difference)
  rownames(combined) <- c("underlying", "likert", "difference")
  
  #This method above does work with cbind but you need to label the cols so it's clear their source
  #pctDiffUActual <- (mean(d$data[d$cond=="ut2"]) - mean(d$data[d$cond=="ut1"])) / mean(d$data[d$cond=="ut1"]) * 100
  #pctDiffUPred <- (mean(d$data[d$cond=="ut2Predicted"]) - mean(d$data[d$cond=="ut1Predicted"])) / mean(d$data[d$cond=="ut1Predicted"]) *100
  #pctDiffCompared <- (pctDiffUPred - pctDiffUActual) / pctDiffUActual * 100
  
  output <- list(dataframe = d, cohenD.results = combined)
  
  return(output)
}

#Use this to plot effect size diff and the means of ut1-2
#So we can see where on the T scales the effect appears
get.df.EffectsizeAndUnderlyingMeanPerSim <- function() {
  d <- m$df[,c(1:3, 21)] #isolate relevant data cols m
  es <- m$effectSizes$diff.effect
  
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

