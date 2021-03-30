source("helpers.R")
library(ggplot2)
library(ggpubr)
library(ggalt)
library(hrbrthemes)
library(viridis)

#Plot T distribtions for 5 likert labels
plot.tfreq <- function() {
        sim <- 1 #isolate a given simulation, happens to be "1"
        df <- freq2Case(sim.num = 1, freq.col = c("t1Freq", "t2Freq", "t3Freq", "t4Freq", "t5Freq"))
        p <- ggplot(df, aes(x=data, fill=source)) +
                geom_histogram(binwidth = 1, alpha=0.6, position = "identity", size = .5, color = "black") +
                theme_classic() + 
                theme(legend.position = "bottom") +
                labs(fill="")
        return(p)
}

#Plot T distribtions
plot.ufreq <- function() {
        sim <- 2 #2 is used because ut1Freq and ut2Freq have a large difference and helps illustrate
        df <- freq2Case(sim.num = sim, freq.col = c("ut1Freq", "ut2Freq"))
        p <- ggplot(df, aes(x=data, fill=source)) +
                geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", size = .5, color = "black") +
                theme_classic() +
                labs(fill="")
        return(p)
}

plot.tprob <- function() {
        sim <- 1
        df <- wide2long(sim.num = sim, freq.col = c("t1Prob", "t2Prob", "t3Prob", "t4Prob", "t5Prob"))
        df$bins <- rep(1:100, 5)
        p <- ggplot(df, aes(x = bins, y = data, group = source)) +
                geom_line(aes(color = source), stat = "identity", size = .5) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous( expand = c(0, 0)) +
                theme_classic() +
                labs(fill="")
        return(p)
}

plot.tprobassign <- function() {
        sim <- 1
        df <- wide2long(sim.num = sim, freq.col = c("t1ProbAssign", "t2ProbAssign", "t3ProbAssign", "t4ProbAssign", "t5ProbAssign"))
        df$bins <- rep(1:100, 5)
        p <- ggplot(df, aes(x = bins, y = data, fill = source)) +
                geom_bar(stat = "identity", size = .5, color = "black") +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous( expand = c(0, 0)) +
                theme_classic() +
                labs(fill="")
        return(p)
}

plot.ul <- function() { #TODO this doesn't work. use new cohen D lib and sytax
        df <- testUvsLikert(sim.num = 1)$dataframe
        df$source <- ifelse(grepl("Predicted", df$cond), "Likert", "Underlying")

        #Plot raw distribution data for U and L
        plot.hist <- ggplot(df, aes(x=data, fill=cond)) +
                geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
                theme_bw() +
                scale_fill_viridis(discrete=TRUE) +
                scale_color_viridis(discrete=TRUE) +
                facet_grid(~source, scales = "free_x") + 
                labs(fill="")
        
        #Calculate cohens d for likert and underlying data. Graph it.
        df.likert <- df[df$source=="Likert",]
        df.underlying <- df[df$source=="Underlying",]
        
        cohenD.underlying <- cohen.d(df.underlying[ ,1:2], group =  "cond")
        cohenD.likert <- cohen.d(df.likert[ ,1:2], group =  "cond")


       
        plot.means <- ggplot(d, aes(x = "Single Likert Conversion", y = cohenD.likert$cohen.d[2])) + 
                geom_errorbar(width=.1, aes(ymin=cohenD.likert$cohen.d[1], ymax=cohenD.likert$cohen.d[3])) +
                geom_point(shape=21, size=2, fill="white") +
                theme_bw() +
                ylim(-2, 2) + 
                ylab("Cohen d") +
                geom_hline(yintercept = cohenD.underlying$cohen.d[2])
        #TODO: add effect size numbers somewher on graph. 
        
        
        p <- ggarrange(plot.hist, plot.means, ncol = 2, widths = c(4,1))
        
        
        return(p)
}

plot.ul.and.es <- function() {
        d <- freq2Case(sim.num = c(1:max(m$df$simNum)), freq.col = c("ut1Freq", "ut2Freq"))
        mean.per.sim.ut1 <- aggregate(.~simNum+source, d[d$source == "ut1Freq",], mean)
        mean.per.sim.ut2 <- aggregate(.~simNum+source, d[d$source == "ut2Freq",], mean)
        
        es <- m$effectSizes$likert.effect
        d <- data.frame(simNum = mean.per.sim.ut1$simNum,
                        ut1Mean = mean.per.sim.ut1$data, 
                        ut2Mean = mean.per.sim.ut2$data,
                        effectSizeDiff = es)
        
        p <- ggplot(d[d$simNum,], aes(x = ut1Mean, xend = ut2Mean, y = effectSizeDiff)) +
                geom_dumbbell(size = .5) + geom_vline(xintercept = c(16, 33, 50, 66, 83)) +
                geom_hline(yintercept = 0)
        #I think there's an artifact to control for. the wave is declining
        #I think I need to take the log of the data somehwere to control for this
        #Something fucky might be going on. Cohen's d can't be negative. 
        #Switch around group differences in main function. 
        
        bothplots <- ggarrange(p, plot.tfreq(), nrow = 2)
        return(bothplots)
}


#Call plots
plot.tfreq()
plot.ufreq()
plot.tprob()
plot.tprobassign()
plot.ul() #doesn't work. use new cohen d syntax
plot.ul.and.es()
