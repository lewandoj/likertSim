library(truncnorm)
library(effsize)

#Calls function "simulateLikertData"
#Arg "numSims": integer. Number of times the simulation should be run. 
#Output: list with 2 objects: (1) dataframe of simulations (2) vector of effect sizes
simulateLikertData <- function(numSims) {
  
  #Step 0. Make simple df with 1:100 bins
  #Data: a vector representing the number of bins of data (e.g., 1:100)
  createDf <- function(data = 1:100) {
    df <- data.frame(bins = data)
    return(df)
  }
  
  #Step 1. Make ut1 and ut2 
  #Bins, 1:100; Freq
  addU <- function (df) {
    scale_mean <- round(runif(n = 1, min = 1, max = 100))
    scale_sd <- 5 #5 number is arbitary
    sample_size <- 1000
    scale_min <- 1
    scale_max <- as.numeric(max(df))
    
    ut1 <- round(rtruncnorm(n = sample_size, 
                            a = scale_min, 
                            b = scale_max, 
                            mean = scale_mean, 
                            sd = scale_sd), 0)
    ut2 <- round(rtruncnorm(n = sample_size, 
                            a = scale_min, 
                            b = scale_max, 
                            mean = scale_mean + (scale_mean * .1),
                            sd = scale_sd), 0)
    
    ut1 <- as.data.frame(table(ut1, dnn = "bins"), responseName = "freq")
    ut2 <- as.data.frame(table(ut2, dnn = "bins"), responseName = "freq")
    
    #Merge list of bins so d is a list of bins with counts from ut1/ut2 for each bin
    d <- Reduce(function(x, y) merge(x = x, 
                                     y = y, 
                                     all.x = TRUE, 
                                     by.x = "bins", 
                                     by.y = "bins"), 
                list(df, ut1, ut2))
    colnames(d) <- c("bins", "ut1Freq", "ut2Freq")
    d[is.na(d)] <- 0
    
    return(d)
  }
  
  #Step 2. Make t1-t5. variable 1: bins. variable 2-6: freq of t distributions
  addT <- function(df) {
    data <- c()
    colLabels <- c()
    scale_num <- c(1,2,3,4,5)
    
    sampleSize = 1000
    scaleMin = 1
    scaleMax = 100
    DivideScaleIn7Parts = (100/6)
    scale_sd <- 5 #5 is arbitary
    
    for (each_scale in scale_num) {
      x <- round(rtruncnorm(n = sampleSize, 
                            a = scaleMin, 
                            b = scaleMax, 
                            mean = scale_num[each_scale] * DivideScaleIn7Parts, 
                            sd = scale_sd), 0)
      #x <- generateNormalPopulation(sizeofPopulation = 1000, 
      #                              scaleMin = 1, 
      #                              scaleMax = 100, 
      #                              scaleMean = scale_num[each_scale] * (100/6),
      #                              scaleSD = scale_sd)
      data[[each_scale]] <- as.data.frame(table(x, dnn = "bins"), 
                                          responseName = paste0("t", each_scale, "Freq"))
    }
    
    #Merge 'data' vectors with bins so d is bins with counts for each 'data' vector
    d <- Reduce(function(x, y) merge(x = x, y = y, all.x = TRUE, by = "bins"),
                list(df, data[[1]], data[[2]], data[[3]], data[[4]], data[[5]]))
    d[is.na(d)] <- 0
    
    return(d)
    
  }
  
  #Step 3-4. Calculate prob distribution and ProbAssignments per bin per t.
  addTProb <- function (df) {
    df$t1Prob <- df$t1Freq / sum(df$t1Freq)
    df$t2Prob <- df$t2Freq / sum(df$t2Freq)
    df$t3Prob <- df$t3Freq / sum(df$t3Freq)
    df$t4Prob <- df$t4Freq / sum(df$t4Freq)
    df$t5Prob <- df$t5Freq / sum(df$t5Freq)
    
    #Step 3c. Calculate the prob that a given bin will belong to a given label
    totalProb <- (df$t1Prob + df$t2Prob + df$t3Prob + df$t4Prob + df$t5Prob)
    df$t1ProbAssign <- df$t1Prob / totalProb
    df$t2ProbAssign <- df$t2Prob / totalProb
    df$t3ProbAssign <- df$t3Prob / totalProb
    df$t4ProbAssign <- df$t4Prob / totalProb
    df$t5ProbAssign <- df$t5Prob / totalProb
    
    #Replace NaN values (due to division by 0) with 0
    df$t1ProbAssign[is.nan(df$t1ProbAssign)] <- 0
    df$t2ProbAssign[is.nan(df$t2ProbAssign)] <- 0
    df$t3ProbAssign[is.nan(df$t3ProbAssign)] <- 0
    df$t4ProbAssign[is.nan(df$t4ProbAssign)] <- 0
    df$t5ProbAssign[is.nan(df$t5ProbAssign)] <- 0
    
    return(df)
  }
  
  
  #Step 5. For each bin in u1/u2, sample prob assignments and output list (from 1-5)
  convertUtoLikert <- function (df) {
    assignment1 <- list()
    assignment2 <- list()
    
    selectSamplingMethod <- function(uColName){
      tFreqSumisZero <- sum(df[each_bin, c("t1Freq", "t2Freq", "t3Freq", "t4Freq", "t5Freq")])
      
      calculateProbForFilledRow <- function() { #TODO CHECK IF THIS NEEDS uColName as arg
        sample(x = c(1,2,3,4,5),
               replace = TRUE,
               size = df[each_bin, uColName],
               prob = df[each_bin, c("t1ProbAssign", "t2ProbAssign", "t3ProbAssign", "t4ProbAssign", "t5ProbAssign")])
      }
      calculateProbForEmptyRow <- function(startingBinIndex = each_bin) { #may need to add uColName as arg
        
        distanceToNonZeroBin <- function(tColName) {
          
          isDataGreaterBelowBin <- function() {
            lowerBinSum <- sum(df[1:each_bin, tColName])
            higherBinSum <- sum(df[each_bin:length(df$t1Freq), tColName])
            if(lowerBinSum > higherBinSum) TRUE else FALSE
          }
          
          lookDownDistance <- function() { 
            dis <- 0
            while(df[each_bin - dis, tColName] == 0) {dis <- dis + 1}
            dis
          }
          
          lookUpDistance <- function() {
            dis <- 0
            while(df[each_bin + dis, tColName] == 0) {dis <- dis + 1}
            dis 
          }
          
          if(isDataGreaterBelowBin() == TRUE) lookDownDistance() else lookUpDistance()
        }
        
        distanceVector <- function() {
          tDist <- c("t1Freq", "t2Freq", "t3Freq", "t4Freq", "t5Freq")
          getTDist <- c()
          for (each_tDist in tDist) {
            getTDist[each_tDist] <- distanceToNonZeroBin(each_tDist)
          }
          getTDist
        }
        
        distanceVectorProb <- function() {
          disV <- distanceVector()
          denom <- sum((1 - disV[1]), (1 - disV[2]), (1 - disV[3]), (1 - disV[4]), (1 - disV[5]))
          ProbAssign <- c()
          ProbAssign[1] <- (1 - disV[1]) / denom
          ProbAssign[2] <- (1 - disV[2]) / denom
          ProbAssign[3] <- (1 - disV[3]) / denom
          ProbAssign[4] <- (1 - disV[4]) / denom
          ProbAssign[5] <- (1 - disV[5]) / denom
          
          ProbAssign
        }
        sampleRow <- function() {
          sample(x = c(1,2,3,4,5),
                 replace = TRUE,
                 size = df[each_bin, uColName], #"ut1Freq"
                 prob = distanceVectorProb())#df[each_bin, distanceVectorProb()])
          
        }
        sampleRow()
      }
      
      if(df[each_bin, uColName] == 0) { 
        NA 
      } else if(tFreqSumisZero == 0) {
        calculateProbForEmptyRow()
      } else 
        calculateProbForFilledRow()
    }
    
    for (each_bin in df[,"bins"]) {
      assignment1[[each_bin]] <- selectSamplingMethod("ut1Freq")
      assignment2[[each_bin]] <- selectSamplingMethod("ut2Freq")

    }
    
    df$ut1Assign <- assignment1
    df$ut2Assign <- assignment2
    return(df)
  } 
  
  #Step 6. Combine Actual U (ut1/ut2) and predicted U into single df
  testUvsLikert <- function(df) {
    ut1 <- data.frame(data = rep(df$bins, df$ut1Freq), cond = "ut1")
    ut2 <- data.frame(data = rep(df$bins, df$ut2Freq), cond = "ut2")
    ut1Predicted <- data.frame(data = na.omit(unlist(df$ut1Assign)), cond = "ut1Predicted")
    ut2Predicted <- data.frame(data = na.omit(unlist(df$ut2Assign)), cond = "ut2Predicted")
    l <- list(ut1, ut2, ut1Predicted, ut2Predicted)
    d <- do.call(rbind, l)
    
    #Effect size (% change): actual (ut1/ut2) vs. Predicted (ut1Predicted/ut2Predicted)
    #pctDiffUActual <- (mean(d$data[d$cond=="ut2"]) - mean(d$data[d$cond=="ut1"])) / mean(d$data[d$cond=="ut1"]) * 100
    #pctDiffUPred <- (mean(d$data[d$cond=="ut2Predicted"]) - mean(d$data[d$cond=="ut1Predicted"])) / mean(d$data[d$cond=="ut1Predicted"]) *100
    #pctDiffCompared <- (pctDiffUPred - pctDiffUActual) / pctDiffUActual * 100
    
    
    #Get cohen's d and CIs for likert & underlying data to compare effect sizes
    d$source <- ifelse(grepl("Predicted", d$cond), "Likert", "Underlying")
    df.likert <- d[d$source=="Likert",]
    df.underlying <- d[d$source=="Underlying",]
    
    cohenD.underlying <- cohen.d(d = df.underlying$data, f = df.underlying$cond)
    cohenD.likert <- cohen.d(d = df.likert$data, f = df.likert$cond)
    cohenD.difference <- cohenD.underlying$estimate - cohenD.likert$estimate
    cohenD.difference.ci <- cohenD.underlying$conf.int - cohenD.likert$conf.int

    combined <- cbind(cohenD.underlying$estimate, cohenD.underlying$conf.int[1], cohenD.underlying$conf.int[2], 
                      cohenD.likert$estimate, cohenD.likert$conf.int[1], cohenD.likert$conf.int[2], 
                      cohenD.difference, cohenD.difference.ci[1], cohenD.difference.ci[2])
    colnames(combined) <- c("underlying.effect", "underlying.lower", "underlying.upper", 
                            "likert.effect", "likert.lower", "likert.upper", 
                            "diff.effect", "diff.lower", "diff.upper")
    rownames(combined) <- NULL
    combined <- as.data.frame(combined)
    
    
    #return(pctDiffCompared)
    return(combined)
  }
  
  #Logic
  runMain <- function() {
    df <- createDf(1:100)
    df1 <- addU(df)
    df2 <- addT(df1)
    df3 <- addTProb(df2)
    df4 <- convertUtoLikert(df3)
    test <- testUvsLikert(df4)
    
    output <- list(df = df4, effectSize = test)
    return(output)
  }
  
  #Probably put this in the function above
  mainLoop <- function(numSims) {
    #effectSize <- c()
    dfFeed <- data.frame()
    
    for (each_sim in 1:numSims) {
      d <- runMain()
      d$df$simNum <- each_sim #Create new variable to assign simulation index
      
      #Create effectSize object
      effectSize <- if(each_sim == 1) {
        d$effectSize 
      } else {
        rbind(effectSize, d$effectSize)
      } 
      
      #effectSize[each_sim] <- d$effectSize
      dfFeed <- rbind(dfFeed, d$df)
    }
    
    combine <- list(df = dfFeed, effectSizes = effectSize)
    return(combine)
    
  }
  
  return(mainLoop(numSims))
}

#Fetch dfs and effect sizes as "m"
m <- simulateLikertData(500)
