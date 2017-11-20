---
title: "2018 Western States Endurance Run Lottery"
author: "Benjamin Chan (https://github.com/benjamin-chan/WSERLottery)"
---

Load packages.


```r
library(parallel)
library(doParallel)
library(data.table)
library(reshape2)
library(ggplot2)
library(xtable)
library(knitr)
library(rmarkdown)
```

Parameters to update.

> Total Tickets: 15075     Total Entrants: 4924
> 
> Last Updated: 11-19-2017 17:00:13 PST
> 
> Years (Tickets) | Entrants| Tickets
> ----------------|---------|--------
> 7 (64) |    9 |  576
> 6 (32) |   69 | 2208
> 5 (16) |  162 | 2592
> 4 ( 8) |  282 | 2256
> 3 ( 4) |  663 | 2652
> 2 ( 2) | 1052 | 2104
> 1 ( 1) | 2687 | 2687


```r
distn <- c(2687, 1052, 663, 282, 162, 69, 9)  # Number of entrants for each ticket count
probWSER <- c(2.502, 4.940, 9.649, 18.358, 33.345, 55.581, 80.203)  # Selection probabilities from WSER
                                                                    # Not necessary for simulation
                                                                    # Only used to comparison
spots <- 250  # Number of spots up for grabs
size <- 1E5  # Use 1E5 for production, 1E3 for testing
dateLottery <- as.Date("2017-12-02", format="%Y-%m-%d")  # Random number seed; use lottery date
```

# Simulation

Here, I run a simulation of the lottery process to estimate probabilities of
winning a slot for the Western States Endurance Run. The simulation does a few
things

* Use the `sample` function in R to sample without replacement using the
  number of tickets each entrant has divided by the total number of tickets in
  the *hat* as each entrant's selection probability for a single draw
* Select draws from the *hat* equal to the number of spots available
* Repeat each *lottery* a number of times
* Use the `aggregate` function to summarize the simulations and derive an
  emperical distribution of selection probabilities
* Plot the selection probability distributions

## Set up initial conditions

Here is the code to set up the lottery hat data frame at the initial state.
Print out some validation output just to make sure the initial state is set up
correctly.


```r
applicants <- sum(distn)
runner <- seq(1, applicants)
confail <- c(rep(6, distn[7]),
             rep(5, distn[6]),
             rep(4, distn[5]),
             rep(3, distn[4]),
             rep(2, distn[3]),
             rep(1, distn[2]),
             rep(0, distn[1]))
tickets <- 2^confail
frameHat <- data.frame(runner, confail, tickets)
frameHat$prob <- frameHat$tickets / sum(frameHat$tickets)
addmargins(table(factor(frameHat$tickets)))  # Check for match with `distn` vector
```

```
## 
##    1    2    4    8   16   32   64  Sum 
## 2687 1052  663  282  162   69    9 4924
```

```r
kable(aggregate(prob ~ tickets, frameHat, mean))  # Check success probabilities of an individual draw
```



| tickets|      prob|
|-------:|---------:|
|       1| 0.0000663|
|       2| 0.0001327|
|       4| 0.0002653|
|       8| 0.0005307|
|      16| 0.0010614|
|      32| 0.0021227|
|      64| 0.0042454|

## Simulate lottery

The simulation needs to account for the changing relative distribution of
tickets after a person is selected and their tickets are no longer in the pool
of eligible tickets.

The matrix `lottery` is an $I \times J$ matrix where row $i$ is the $i$-th
simulation and the column $j$ is the $j$-th lottery winner drawn. The number
of columns in the matrix is 250, variable `spots`. The number of
simulated lotteries is variable `size`. Set the random number seed as the date
of the lottery in numeric form multipied by the number of applicants.


```r
set.seed(as.numeric(dateLottery) * applicants)
cores <- min(detectCores(), 4)
cl <- makeCluster(cores)
registerDoParallel(cl)
simTime <- system.time(
  lottery <- foreach (i=1:size, .combine=rbind) %dopar% {
    sample(frameHat$runner, spots, prob=frameHat$prob)
  }
)
stopCluster(cl)
```

## One simulated lottery


```r
i <- sample(seq(1, size), 1)
sampLottery <- list(i, sort(lottery[i, ]))
names(sampLottery) <- c("lottery", "runner")
sampLottery
```

```
## $lottery
## [1] 67538
## 
## $runner
##   [1]    1    2    3    4    5    6    9   11   13   14   15   18   24   28
##  [15]   30   32   33   35   36   38   40   45   46   49   50   53   55   57
##  [29]   58   63   64   65   70   73   75   76   77   78   79   80   81   82
##  [43]   88  102  106  108  109  112  118  121  122  125  127  129  131  136
##  [57]  149  150  151  153  154  155  164  165  169  172  178  180  187  188
##  [71]  189  192  196  198  203  204  208  210  214  221  225  230  234  235
##  [85]  236  237  239  243  247  254  267  269  271  302  311  319  321  325
##  [99]  328  330  334  336  343  344  353  362  367  368  375  381  392  396
## [113]  399  414  421  424  428  435  437  453  457  458  475  480  482  490
## [127]  509  519  524  526  528  543  549  559  562  568  598  607  608  613
## [141]  614  617  636  663  673  675  682  698  719  728  731  737  741  746
## [155]  769  778  807  847  851  876  902  917  919  943  956  973  982  989
## [169]  997 1013 1015 1023 1034 1040 1058 1072 1096 1108 1113 1151 1167 1173
## [183] 1190 1226 1291 1315 1322 1337 1411 1464 1484 1503 1527 1564 1589 1679
## [197] 1683 1708 1721 1781 1848 1948 1999 2003 2037 2054 2085 2104 2117 2146
## [211] 2158 2173 2244 2315 2458 2492 2535 2661 2691 2801 2882 2962 2966 3005
## [225] 3045 3089 3112 3158 3270 3326 3422 3487 3526 3556 3650 3687 3753 3814
## [239] 3852 3890 4142 4306 4489 4515 4602 4692 4720 4739 4825 4923
```

Here's the distribution of the category of ticket holders from that random
simulated lottery.
I.e., in simulated lottery 67538, 


```r
agg1 <- data.frame(addmargins(table(frameHat$confail[sampLottery$runner])))
agg1$year <- as.numeric(agg1$Var1)
agg1$tickets <- 2 ^ agg1$year
agg1[agg1$Var1 == "Sum", ]$year <- NA
agg1[agg1$Var1 == "Sum", ]$tickets <- NA
agg1 <- agg1[, c("year", "tickets", "Freq")]
kable(agg1)
```



| year| tickets| Freq|
|----:|-------:|----:|
|    1|       2|   38|
|    2|       4|   30|
|    3|       8|   54|
|    4|      16|   41|
|    5|      32|   49|
|    6|      64|   31|
|    7|     128|    7|
|   NA|      NA|  250|

Okay... but what happened with the other 99999 simulated lotteries?

## Format lottery simulation data

I'm not really interested in which runners were selected in the lottery
simulation. What I'm really after are estimates for the probability of
selecting a runner, among the 250 available spots, with $X$ tickets in
the initial hat.

To get at this, first I'll have to match the runners selected to the number of
tickets they started out with.


```r
cl <- makeCluster(cores)
registerDoParallel(cl)
lottery2 <- foreach (i=1:size, .combine=rbind) %dopar% {
  frameHat$confail[lottery[i, ]]
}
stopCluster(cl)
```

Reformat the `lottery2` matrix to an aggregated data frame for analysis.


```r
confail <- factor(as.vector(t(lottery2)))
sim <- rep(seq(1, size), each=spots)
frameLottery <- data.frame(sim, confail)
frameLottery <- data.table(frameLottery)
aggTime <- system.time(
  aggLottery <- frameLottery[,
                             list(confail.0 = sum(confail == 0),
                                  confail.1 = sum(confail == 1),
                                  confail.2 = sum(confail == 2),
                                  confail.3 = sum(confail == 3),
                                  confail.4 = sum(confail == 4),
                                  confail.5 = sum(confail == 5),
                                  confail.6 = sum(confail == 6)),
                             sim]
)
nlev <- nlevels(frameLottery$confail)
sim <- rep(seq(1, size), each=nlev)
confail <- factor(rep(seq(1, nlev), size))
freq <- as.vector(t(aggLottery[, list(confail.0, 
                                      confail.1, 
                                      confail.2, 
                                      confail.3, 
                                      confail.4, 
                                      confail.5, 
                                      confail.6)]))
frameSummary <- data.frame(sim, confail, freq)
frameSummary <- melt(aggLottery, id.vars=c("sim"))
frameSummary$confail <- factor(gsub("confail\\.", "", frameSummary$variable))
frameSummary$freq <- frameSummary$value
frameSummary[, c("variable", "value")] <- NULL
frameSummary <- frameSummary[order(frameSummary$sim, frameSummary$confail)]
```

Save the aggregated data frame for other analysis.


```r
save(aggLottery, file="aggLottery.RData")
```

For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the
proportion of selected applicants.


```r
total <- rep(distn, size)
frameSummary$prob <- 100 * (frameSummary$freq / total)
aggFx <- function(x) {c(mean = mean(x), median = median(x), sd = sd(x))}
aggProb <- aggregate(prob ~ confail, frameSummary, aggFx)
ev <- distn * aggProb[, "prob"][, "mean"] / 100
evWSER <- distn * probWSER / 100
diffprob <- aggProb[, "prob"][, "mean"] - probWSER
diffev <- ev - evWSER
pctdiff <- 100 * diffprob / aggProb[, "prob"][, "mean"]
sqerr <- diffprob ^2
simsum <- data.frame(confail = aggProb[, "confail"], 
                     nTickets = 2 ^ (as.numeric(aggProb[, "confail"]) - 1),
                     distn, 
                     mean = aggProb[, "prob"][, "mean"], 
                     ev, 
                     probWSER, 
                     evWSER, 
                     diffprob, 
                     diffev, 
                     pctdiff, 
                     sqerr)
names(simsum) <- c("Consec. Failures", 
                   "Number of tickets",
                   "N", 
                   "Mean", 
                   "EV", 
                   "Prob (WSER)", 
                   "EV (WSER)", 
                   "Diff. prob.", 
                   "Diff. EV", 
                   "% diff.", 
                   "Sq. error")
```

# Summarize lottery simulations

Plot the distribution of probabilities from the 100,000
simulated lotteries.

## Probability of selection


```r
title <- sprintf("%.0d WSER Lottery Selection Probability Densities", as.numeric(format(dateLottery, "%Y")) + 1)
xlab <- "Probability of selection"
options(scipen=999)
ylab <- paste("Proportion of", format(size, big.mark=",", digits=0), "simulations")
filllab <- "Consecutive annual lottery failures"
annolab <- sprintf("%.2f%%", simsum$Mean)
y0 <- max(density(frameSummary$prob[frameSummary$confail == 0])$y)
y1 <- max(density(frameSummary$prob[frameSummary$confail == 1])$y)
y2 <- max(density(frameSummary$prob[frameSummary$confail == 2])$y)
y3 <- max(density(frameSummary$prob[frameSummary$confail == 3])$y)
y4 <- max(density(frameSummary$prob[frameSummary$confail == 4])$y)
y5 <- max(density(frameSummary$prob[frameSummary$confail == 5])$y)
y6 <- max(density(frameSummary$prob[frameSummary$confail == 6])$y)
y <- c(y0, y1, y2, y3, y4, y5, y6)
ggplot(frameSummary, aes(x = confail, y = prob / 100, fill = confail)) +
  geom_violin() +
  scale_fill_brewer(palette="Spectral") + 
  scale_y_continuous(trans = "logit") + 
  labs(title=title, x=filllab, y=xlab, fill=filllab) +
  annotate("text", label=annolab, y=simsum$Mean / 100, x=c(0:4+1.6, 5:6+0.5)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 3584 rows containing non-finite values (stat_ydensity).
```

![plot of chunk PlotProbabilities](figure/PlotProbabilities-1.png)

As expected, the spread of the selection probabilities increases as the number
of tickets a person has in the hat increases (the variance of a binomial
random variable increases with $p$).

## Number of entrants

Another way to think about the lottery is to plot the distribution of the
frequency of runners selected by number of tickets.


```r
title <- sprintf("%.0d WSER Lottery Selection Distribution Densities", as.numeric(format(dateLottery, "%Y")) + 1)
xlab <- "Number of entrants selected"
ylab <- paste("Proportion of", format(size, big.mark=",", digits=0), "simulations")
filllab <- "Consecutive annual lottery failures"
annolab <- sprintf("%.1f", simsum$EV)
y0 <- max(density(frameSummary$freq[frameSummary$confail == 0])$y)
y1 <- max(density(frameSummary$freq[frameSummary$confail == 1])$y)
y2 <- max(density(frameSummary$freq[frameSummary$confail == 2])$y)
y3 <- max(density(frameSummary$freq[frameSummary$confail == 3])$y)
y4 <- max(density(frameSummary$freq[frameSummary$confail == 4])$y)
y5 <- max(density(frameSummary$freq[frameSummary$confail == 5])$y)
y6 <- max(density(frameSummary$freq[frameSummary$confail == 6])$y)
y <- c(y0, y1, y2, y3, y4, y5, y6)
ggplot(frameSummary, aes(x = confail, y = freq, fill = confail)) +
  geom_violin() +
  scale_fill_brewer(palette="Spectral") + 
  scale_y_sqrt() + 
  labs(title=title, x=filllab, y=xlab, fill=filllab) +
  annotate("text", label=annolab, y=simsum$EV, x=c(0:5+1.3, 6+0.4)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

![plot of chunk PlotFrequencies](figure/PlotFrequencies-1.png)


## Probability all 64-ticket holders are selected


```r
df <- aggregate(sim ~ freq, frameSummary[confail == nlev - 1, ], length)
nsim <- df[df$freq == max(df$freq), "sim"]
```

The probability all 64-ticket holders are selected is 
3.58%.


## Probability of selection and expected number of entrants

Compare to probabilities given in the WSER 2014 [lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics/).


```r
kable(simsum[, c("Number of tickets", "N", "Mean", "EV")])
```



| Number of tickets|    N|      Mean|       EV|
|-----------------:|----:|---------:|--------:|
|                 1| 2687|  1.815056| 48.77055|
|                 2| 1052|  3.598093| 37.85194|
|                 4|  663|  7.071715| 46.88547|
|                 8|  282| 13.638018| 38.45921|
|                16|  162| 25.420451| 41.18113|
|                32|   69| 44.395696| 30.63303|
|                64|    9| 69.096333|  6.21867|

My estimates are *virtually identical* to the probabilities calculated by
[WSER](http://www.wser.org/2013/11/27/2014-lottery-statistics) (*Mean* column
versus the *Prob (WSER)* column). Percent differences of the selection
probabilities are never more than 37.8469999% and the mean
squared error of the selection probabilities is 48.922971.


# Outcome of sample of lotteries

Estimated from 100,000 simulated lotteries.

## Sample of simulated lotteries


```r
s <- 25
title <- sprintf("Simulated %.0d WSER Lotteries\nSample of %.0f Lotteries", as.numeric(format(dateLottery, "%Y")) + 1, s)
xlab <- "Simulated lottery"
ylab <- "Number of selected runners\nEach block represents 10 runners"
filllab <- "Consecutive failures"
i <- sample(seq(1, size), s)
frameSample <- frameLottery[frameLottery$sim %in% i, ]
frameSample$sim <- factor(frameSample$sim)
levels(frameSample$sim) <- rev(levels(frameSample$sim))
ggplot(frameSample, aes(x=sim, fill=confail)) +
  geom_bar(width=1) +
  geom_hline(yintercept = seq(0, spots, 10), color="white") +
  geom_vline(xintercept = seq(1, s)-0.5, color="white") +
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(expand=c(0, 0)) +
  labs(title=title, x=xlab, y=ylab, fill=filllab) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(hjust = 0.5))
```

![plot of chunk sampleOutcomes](figure/sampleOutcomes-1.png)


# Details

Go to my [WSERLottery](https://github.com/benjamin-chan/WSERLottery) repository for the gory details.


# Session info


```
## Timestamp: 2017-11-19 21:17:37
```

```
## Number of cores used in simulation: 2
```

```
## Random number seed: 86179848
```

```
## Elapsed time of simulation: 5.318 minutes
```

```
## Elapsed time of aggregation: 0.676 minutes
```

```
## R version 3.4.2 (2017-09-28)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## Matrix products: default
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] rmarkdown_1.6       knitr_1.17          xtable_1.8-2       
##  [4] ggplot2_2.2.1       reshape2_1.4.2      data.table_1.10.4-3
##  [7] doParallel_1.0.11   iterators_1.0.8     foreach_1.4.3      
## [10] checkpoint_0.4.2   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.13       magrittr_1.5       munsell_0.4.3     
##  [4] colorspace_1.3-2   rlang_0.1.2        highr_0.6         
##  [7] stringr_1.2.0      plyr_1.8.4         tools_3.4.2       
## [10] grid_3.4.2         gtable_0.2.0       htmltools_0.3.6   
## [13] rprojroot_1.2      digest_0.6.12      yaml_2.1.14       
## [16] lazyeval_0.2.1     tibble_1.3.4       RColorBrewer_1.1-2
## [19] codetools_0.2-15   evaluate_0.10.1    labeling_0.3      
## [22] stringi_1.1.5      compiler_3.4.2     backports_1.1.1   
## [25] scales_0.5.0
```

```
##        sysname        release        version       nodename        machine 
##      "Windows"     ">= 8 x64"   "build 9200"     "FAMILYPC"       "x86-64" 
##          login           user effective_user 
##          "Ben"          "Ben"          "Ben"
```
