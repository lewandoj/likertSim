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
  ut1Likert <- data.frame(data = na.omit(unlist(df$ut1Assign)), cond = "ut1Likert")
  ut2Likert <- data.frame(data = na.omit(unlist(df$ut2Assign)), cond = "ut2Likert")
  l <- list(ut1, ut2, ut1Likert, ut2Likert)
  d <- do.call(rbind, l)
  
  #Prep data for comparisons
  d$source <- ifelse(grepl("Likert", d$cond), "Likert", "Underlying")
  df.likert <- d[d$source=="Likert",]
  df.underlying <- d[d$source=="Underlying",]
  
  #Add comparison statistics
  meansPerGroup <- aggregate(d$data, list(d$cond), mean)
  
  pctDiffUnderlying <- (mean(d$data[d$cond=="ut2"]) - mean(d$data[d$cond=="ut1"])) / mean(d$data[d$cond=="ut1"]) * 100
  pctDiffLikert <- (mean(d$data[d$cond=="ut2Likert"]) - mean(d$data[d$cond=="ut1Likert"])) / mean(d$data[d$cond=="ut1Likert"]) *100
  pctDiffCompared <- (pctDiffLikert - pctDiffUnderlying) / pctDiffUnderlying * 100
  
  cohenD.underlying <- cohen.d(d = df.underlying$data, f = df.underlying$cond)
  cohenD.likert <- cohen.d(d = df.likert$data, f = df.likert$cond)
  cohenD.difference <- cohenD.underlying$estimate - cohenD.likert$estimate
  cohenD.difference.ci <- cohenD.underlying$conf.int - cohenD.likert$conf.int
  
  #Combine data
  combined <- data.frame(ut1.mean = meansPerGroup[1,2],
                         ut2.mean = meansPerGroup[3,2],
                         ut1Likert.mean = meansPerGroup[2,2],
                         ut2Likert.mean = meansPerGroup[4,2],
                         underlying.pctdiff = pctDiffUnderlying,
                         likert.pctdiff = pctDiffLikert,
                         diff.pctdiff = pctDiffCompared,
                         underlying.cohenD.effect = cohenD.underlying$estimate,
                         underlying.cohenD.lower = cohenD.underlying$conf.int[1],
                         underlying.cohenD.upper = cohenD.underlying$conf.int[2],
                         likert.cohenD.effect = cohenD.likert$estimate,
                         likert.cohenD.lower = cohenD.likert$conf.int[1],
                         likert.cohenD.upper = cohenD.likert$conf.int[2],
                         diff.cohenD.effect = cohenD.difference,
                         diff.cohenD.lower = cohenD.difference.ci[1],
                         diff.cohenD.upper = cohenD.difference.ci[2]
  )
  rownames(combined) <- NULL
  output <- list(dataframe = d, effectSizes = combined)
  
  return(output)
}


