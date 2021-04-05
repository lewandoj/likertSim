source("helpers.R")
library(ggplot2)
library(ggpubr)
library(ggalt)
#library(hrbrthemes)
library(viridis)

#Plot T distribtions for 5 likert labels
plot.tfreq <- function() {
        sim <- 1 #isolate a given simulation, happens to be "1"
        df <- freq2Case(sim.num = 1, freq.col = c("t1Freq", "t2Freq", "t3Freq", "t4Freq", "t5Freq"))
        
        p <- ggplot(df, aes(x=data, fill=source)) +
                geom_histogram(binwidth = 1, alpha=0.6, position = "identity", size = .5, color = "black") +
                theme_classic() + 
                labs(fill="") + 
                scale_x_continuous(expand=c(0,0)) +
                scale_y_continuous(expand=c(0,0)) + 
                xlim(c(0, 100)) + 
                theme(legend.position = "bottom")
        return(p)
}

#Plot u distributions for 1 simulation. To be used as an exmaple. 
plot.ufreq <- function() {
        sim <- 50
        df <- freq2Case(sim.num = sim, freq.col = c("ut1Freq", "ut2Freq"))
        
        p <- ggplot(df, aes(x=data, fill=source)) +
                geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", size = .5, color = "black") +
                theme_classic() +
                labs(fill="") + 
                xlim(c(0, 100))
        return(p)
}

#Plot joint probability distribution based on t distributions.
plot.tprob <- function() {
        sim <- 1
        df <- wide2long(sim.num = sim, 
                        freq.col = c("t1Prob", 
                                     "t2Prob", 
                                     "t3Prob", 
                                     "t4Prob", 
                                     "t5Prob"))
        df$bins <- rep(1:100, 5)
        
        p <- ggplot(df, aes(x = bins, y = data, group = source)) +
                geom_line(aes(color = source), stat = "identity", size = .5) +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous( expand = c(0, 0)) +
                theme_classic() +
                labs(fill="")
        return(p)
}

#Same as above but plot the probability distrbutions as stacked bars 
#This makes it clear how a single value of "u" is converted into "L" using this
#probablility distribution assignment
plot.tprobassign <- function() {
        sim <- 1
        df <- wide2long(sim.num = sim, 
                        freq.col = c("t1ProbAssign", 
                                     "t2ProbAssign", 
                                     "t3ProbAssign", 
                                     "t4ProbAssign", 
                                     "t5ProbAssign"))
        df$bins <- rep(1:100, 5)
        
        p <- ggplot(df, aes(x = bins, y = data, fill = source)) +
                geom_bar(stat = "identity", size = .3, color = "black") +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous( expand = c(0, 0)) +
                theme_classic() +
                labs(fill="") + 
                theme(legend.position = "bottom")
        return(p)
}

#Plot 1 example of a ut1/ut2 distribution along with the likert distributions
#Also plot the means of those distributions so you can examine 1 effect size
plot.ul <- function(simNum = 50) {
        df <- testUvsLikert(sim.num = simNum)$dataframe
        es <- testUvsLikert(sim.num = simNum)$effectSizes
        df$source <- ifelse(grepl("Likert", df$cond), "Likert", "Underlying")

        #Plot raw distribution data for U and L
        plot.hist <- ggplot(df, aes(x=data, fill=cond)) +
                geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
                facet_grid(~source, scales = "free_x") + 
                theme_bw() +
                scale_fill_viridis(discrete=TRUE) +
                scale_color_viridis(discrete=TRUE) +
                labs(fill="")
        #Plot the likert effect size and an hline of the underlying effect size
        plot.means <- ggplot(es, aes(x = "Single Likert Conversion", y = likert.pctdiff)) + 
                geom_point(shape=1, size=4, fill="white") +
                theme_bw() +
                ylab("Percent Difference") +
                geom_hline(yintercept = es$underlying.pctdiff, color = "red") + 
                geom_text(aes(label = paste0("(", round(likert.pctdiff, 2), ")")), nudge_y = -0.05) + 
                annotate("segment", x = 1, xend = 1, y = es$likert.pctdiff, yend = es$underlying.pctdiff, 
                         size = 1, arrow = arrow(ends = "both"))

        p <- ggarrange(plot.hist, plot.means, ncol = 2, widths = c(4,1))
        
        
        return(p)
}

#Plot effect sizes across all simulations. 
# Plot 1: x = ut1 and u2 means as dumbbells and y = effect size
# Plot t distributions as a reference.
plot.ul.and.es <- function() {
        d <- m$effectSizes
        d$simNum <- 1:nrow(d)

        #p <- ggplot(d[d$simNum,], aes(x = ut1.mean, 
        #                              xend = ut2.mean, 
        #                              y = likert.pctdiff)) +
        #        geom_dumbbell(size = .5) + 
        #        geom_vline(xintercept = c(16, 33, 50, 66, 83)) +
        #        geom_hline(yintercept = 10, color = "red", size = 2) +
        #        theme_classic() + 
        #        scale_x_continuous(expand=c(0,0)) +
        #        scale_y_continuous(expand=c(0,0)) +
        #        xlim(c(0, 100)) + ylim(c(-20, 20))
        
        p <- ggplot(d, aes(x = ut1.mean, 
                      xend = ut2.mean, 
                      y = likert.pctdiff), ) +
                geom_line(aes(x = ut1.mean, y = underlying.pctdiff), 
                          stat = "identity", color = "red", size = 1.5) +
                geom_dumbbell(size = .5) + 
                geom_vline(xintercept = c(16, 33, 50, 66, 83)) +
                theme_classic() + 
                scale_x_continuous(expand=c(0,0)) +
                scale_y_continuous(expand=c(0,0)) +
                xlim(c(0, 100)) + ylim(c(-20, 20))
        
        bothplots <- ggarrange(p, plot.tfreq(), nrow = 2, align = "v", heights = c(3,1))

        return(bothplots)
}


#Call plots
plot.tfreq()
plot.ufreq()
plot.tprob()
plot.tprobassign()
plot.ul()
plot.ul.and.es()
