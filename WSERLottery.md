2014 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on 2013-12-07 17:55:41 using R version 3.0.1 (2013-05-16).


Details
-------
From the [2014 WSER lottery page](http://www.wser.org/lottery2014.html). **Use this table** for input data on the number entrants for each ticket count. The index of the vector will serve as the ticket count.

> Total Tickets: 4307     Total Entrants: 2704  
>  
> Last Updated: 12/05/2013 7:00AM PST  
>  
> Ticket Count | Entrants| Tickets
> -------------|---------|--------
>            5 |      52 |     260
>            4 |     106 |     424
>            3 |     258 |     774
>            2 |     561 |    1122
>            1 |    1727 |    1727


```r
distn <- c(1727, 561, 258, 106, 52)
```


This information is different from what is shown on the [2014 lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics) page, which was posted on 12/5/2013, but shortly before the update above. I'm pretty sure any difference between my numbers and what's at the link above is that WSER runs their simulation before some entries get verified and corrected. **Use this table** for input data on the selection probabilities to compare my simulations to. 
> Tickets | # of Entrants | Probability (%) | Expected # Selected | Expected % Selected  
> --------|---------------|-----------------|---------------------|--------------------
>       1 |          1727 |             6.5 |               112.2 |                41.6  
>       2 |           561 |            12.6 |                70.6 |                26.1  
>       3 |           258 |            18.3 |                47.1 |                17.5  
>       4 |           106 |            23.6 |                25.0 |                 9.2  
>       5 |            53 |            28.5 |                15.1 |                 5.6  
>  Totals |          2705 |                 |               270.0 |               100.0


```r
probWSER <- c(6.5, 12.6, 18.3, 23.6, 28.5)
```


Here, I run a simulation of the lottery process to estimate probabilities of winning a slot for the Western States Endurance Run. The simulation does a few things
* Use the `sample` function in R to sample without replacement using the number of tickets each entrant has divided by the total number of tickets in the *hat* as each entrant's selection probability for a single draw
* Select draws from the *hat* equal to the number of spots available
* Repeat each *lottery* a number of times
* Use the `aggregate` function to summarize the simulations and derive an emperical distribution of selection probabilities
* Plot the selection probability distributions


Set up initial conditions
-------------------------
Here is the code to set up the lottery hat data frame at the initial state. Print out some validation output just to make sure the initial state is set up correctly.

```r
spots <- 270
applicants <- sum(distn)
runner <- seq(1, applicants)
tickets <- c(rep(5, distn[5]), rep(4, distn[4]), rep(3, distn[3]), rep(2, distn[2]), 
    rep(1, distn[1]))
frameHat <- data.frame(runner, tickets)
frameHat$prob <- frameHat$tickets/sum(frameHat$tickets)
addmargins(table(factor(frameHat$tickets)))
```

```
## 
##    1    2    3    4    5  Sum 
## 1727  561  258  106   52 2704
```

```r
head(frameHat)
```

```
##   runner tickets     prob
## 1      1       5 0.001161
## 2      2       5 0.001161
## 3      3       5 0.001161
## 4      4       5 0.001161
## 5      5       5 0.001161
## 6      6       5 0.001161
```

```r
tail(frameHat)
```

```
##      runner tickets      prob
## 2699   2699       1 0.0002322
## 2700   2700       1 0.0002322
## 2701   2701       1 0.0002322
## 2702   2702       1 0.0002322
## 2703   2703       1 0.0002322
## 2704   2704       1 0.0002322
```



Simulate lottery
----------------
The simulation needs to account for the changing relative distribution of tickets after a person is selected and their tickets are no longer in the pool of eligible tickets.

The matrix `lottery` is an $I \times J$ matrix where row $i$ is the $i$-th simulation and the column $j$ is the $j$-th lottery winner drawn. The number of columns in the matrix is 270, variable `spots`. The number of simulated lotteries is variable `size`. Set the random number seed as the date of the lottery in numeric form multipied by the number of applicants.

```r
size <- 1e+05
dateLottery <- as.Date("2013-12-07", format = "%Y-%m-%d")
set.seed(as.numeric(dateLottery) * applicants)
lottery <- matrix(nrow = size, ncol = spots)
system.time(for (i in 1:size) {
    lottery[i, ] <- sample(frameHat$runner, spots, prob = frameHat$prob)
})
```

```
##    user  system elapsed 
##   78.45    0.10   83.14
```


Here's an example of the selected runners drawn from a random simulated lottery.

```r
i <- sample(seq(1, size), 1)
sampLottery <- list(i, sort(lottery[i, ]))
names(sampLottery) <- c("lottery", "runner")
sampLottery
```

```
## $lottery
## [1] 50282
## 
## $runner
##   [1]    1    5    7   16   18   19   20   22   25   28   30   31   38   41
##  [15]   48   50   52   53   60   61   62   76   77   82   84   85   87   93
##  [29]   95   98   99  100  108  113  124  129  136  147  151  152  161  165
##  [43]  177  184  189  196  197  199  204  214  216  217  218  224  227  228
##  [57]  229  231  232  247  249  250  252  267  269  271  283  287  288  296
##  [71]  308  314  318  321  324  326  328  329  330  334  343  345  349  350
##  [85]  351  353  354  358  368  398  401  410  411  413  429  437  451  453
##  [99]  454  464  467  471  479  483  497  509  531  532  542  558  573  576
## [113]  580  585  591  599  614  642  654  662  663  664  665  670  677  684
## [127]  691  694  698  701  702  703  706  720  735  737  739  746  748  751
## [141]  767  771  772  773  787  788  793  799  831  834  839  842  850  867
## [155]  909  910  925  934  940  944  948  950  966  973  999 1000 1072 1098
## [169] 1130 1150 1172 1220 1234 1260 1279 1293 1304 1378 1422 1431 1434 1438
## [183] 1445 1451 1463 1472 1497 1509 1520 1548 1565 1582 1617 1630 1683 1684
## [197] 1707 1711 1717 1721 1722 1724 1726 1727 1769 1775 1776 1790 1812 1829
## [211] 1842 1843 1924 1951 1967 1970 1977 1979 2006 2010 2011 2019 2021 2023
## [225] 2031 2062 2077 2108 2131 2150 2154 2189 2192 2207 2226 2227 2233 2236
## [239] 2307 2309 2310 2313 2315 2327 2329 2333 2344 2349 2350 2381 2385 2401
## [253] 2449 2464 2476 2477 2479 2495 2519 2539 2597 2610 2617 2637 2639 2655
## [267] 2682 2690 2695 2699
```

Here's the distribution of the category of ticket holders from that random simulated lottery.

```r
addmargins(table(frameHat$tickets[sampLottery$runner]))
```

```
## 
##   1   2   3   4   5 Sum 
## 106  70  54  23  17 270
```

I.e., in simulated lottery 50282, 
* 106 applicants with 1 ticket were selected  (6.1%)
* 70 applicants with 2 tickets were selected (12%)
* 54 applicants with 3 tickets were selected (21%)
* 23 applicants with 4 tickets were selected (22%)
* 17 applicants with 5 tickets were selected (33%)

Okay... but what happened with the other 9.9999 &times; 10<sup>4</sup> simulated lotteries?


Format lottery simulation data
------------------------------
I'm not really interested in which runners were selected in the lottery simulation. What I'm really after are estimates for the probability of selecting a runner, among the 270 available spots, with $X$ tickets in the initial hat.

To get at this, first I'll have to match the runners selected to the number of tickets they started out with.

```r
lottery2 <- matrix(nrow = size, ncol = spots)
for (i in 1:size) {
    lottery2[i, ] <- frameHat$tickets[lottery[i, ]]
}
```

Reformat the `lottery2` matrix to an aggregated data frame for analysis.

```r
tickets <- factor(as.vector(t(lottery2)))
sim <- rep(seq(1, size), each = spots)
frameLottery <- data.frame(sim, tickets)
system.time(aggLottery <- aggregate(tickets ~ sim, frameLottery, table))
```

```
##    user  system elapsed 
##  251.36    2.94  400.03
```

```r
sim <- rep(seq(1, size), each = 5)
tickets <- factor(rep(seq(1, 5), size))
freq <- as.vector(t(aggLottery$tickets))
frameSummary <- data.frame(sim, tickets, freq)
```

Save the aggregated data frame for other analysis.

```r
save(aggLottery, file = "aggLottery.RData")
```


For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the proportion of selected applicants. 

```r
total <- rep(distn, size)
frameSummary$prob <- 100 * (frameSummary$freq/total)
aggFx <- function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x))
}
aggProb <- aggregate(prob ~ tickets, frameSummary, aggFx)
ev <- distn * aggProb[, "prob"][, "mean"]/100
evWSER <- distn * probWSER/100
diffprob <- aggProb[, "prob"][, "mean"] - probWSER
diffev <- ev - evWSER
pctdiff <- 100 * diffprob/aggProb[, "prob"][, "mean"]
sqerr <- diffprob^2
simsum <- data.frame(tickets = aggProb[, "tickets"], distn, mean = aggProb[, 
    "prob"][, "mean"], ev, probWSER, evWSER, diffprob, diffev, pctdiff, sqerr)
names(simsum) <- c("Tickets", "N", "Mean", "EV", "Prob (WSER)", "EV (WSER)", 
    "Diff. prob.", "Diff. EV", "% diff.", "Sq. error")
```


Summarize lottery simulations
-----------------------------
Plot the distribution of probabilities from the 1e+05 simulated lotteries. Annotate with the estimated mean selection probability.

```r
title <- "2014 WSER Lottery Selection Probability Densities"
xlab <- "Probability of selection"
ylab <- paste("Proportion of", format(size, big.mark = ","), "simulations")
filllab <- "Tickets"
annolab <- sprintf("%.2f%%", simsum$Mean)
y1 <- max(density(frameSummary$prob[frameSummary$tickets == 1])$y)
y2 <- max(density(frameSummary$prob[frameSummary$tickets == 2])$y)
y3 <- max(density(frameSummary$prob[frameSummary$tickets == 3])$y)
y4 <- max(density(frameSummary$prob[frameSummary$tickets == 4])$y)
y5 <- max(density(frameSummary$prob[frameSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
require(ggplot2, quietly = TRUE)
ggplot(frameSummary, aes(x = prob, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = title, 
    x = xlab, y = ylab, fill = filllab) + annotate("text", label = annolab, 
    x = simsum$Mean, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotProbabilities](figure/PlotProbabilities.png) 

As expected, the spread of the selection probabilities increases as the number of tickets a person has in the hat increases (the variance of a binomial random variable increases with $p$).

Another way to think about the lottery is to plot the distribution of the frequency of runners selected by number of tickets. Annotate with the estimated expected value.

```r
title <- "2014 WSER Lottery Selection Distribution Densities"
xlab <- "Number of entrants selected"
ylab <- paste("Proportion of", format(size, big.mark = ","), "simulations")
filllab <- "Tickets"
annolab <- sprintf("%.1f", simsum$EV)
y1 <- max(density(frameSummary$freq[frameSummary$tickets == 1])$y)
y2 <- max(density(frameSummary$freq[frameSummary$tickets == 2])$y)
y3 <- max(density(frameSummary$freq[frameSummary$tickets == 3])$y)
y4 <- max(density(frameSummary$freq[frameSummary$tickets == 4])$y)
y5 <- max(density(frameSummary$freq[frameSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
ggplot(frameSummary, aes(x = freq, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = title, 
    x = xlab, y = ylab, fill = filllab) + annotate("text", label = annolab, 
    x = simsum$EV, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotFrequencies](figure/PlotFrequencies.png) 


Print a table summarizing the simulated lotteries showing the mean and median selection probabilities and their standard deviations. Compare to probabilities given in the WSER 2014 [lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics/).

```r
require(xtable, quietly = TRUE)
print(xtable(simsum), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Sat Dec 07 18:04:26 2013 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> N </TH> <TH> Mean </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH> <TH> Sq. error </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 1727.00 </TD> <TD align="right"> 6.50 </TD> <TD align="right"> 112.34 </TD> <TD align="right"> 6.50 </TD> <TD align="right"> 112.25 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 561.00 </TD> <TD align="right"> 12.59 </TD> <TD align="right"> 70.65 </TD> <TD align="right"> 12.60 </TD> <TD align="right"> 70.69 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.05 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 258.00 </TD> <TD align="right"> 18.28 </TD> <TD align="right"> 47.17 </TD> <TD align="right"> 18.30 </TD> <TD align="right"> 47.21 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.09 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 106.00 </TD> <TD align="right"> 23.59 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 23.60 </TD> <TD align="right"> 25.02 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.06 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 5 </TD> <TD align="right"> 52.00 </TD> <TD align="right"> 28.54 </TD> <TD align="right"> 14.84 </TD> <TD align="right"> 28.50 </TD> <TD align="right"> 14.82 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>

My estimates are *virtually identical* to the probabilities calculated by [WSER](http://www.wser.org/2013/11/27/2014-lottery-statistics) (*Mean* column versus the *Prob (WSER)* column). Percent differences of the selection probabilities are never more than 0.1358% and the mean squared error of the selection probabilities is 0.000413.

Plot the outcomes of a random sample of the 1e+05 simulated lotteries as a [waffle plot](http://www.improving-visualisation.org/vis/id=179). Each row is a single simulated lottery. The color segment in each row represents the number of selected runners in a ticket category. Each blocks represent 10 runners.

```r
s <- 25
title <- sprintf("Simulated 2014 WSER Lotteries\nSample of %.0f Lotteries", 
    s)
xlab <- "Simulated lottery"
ylab <- "Number of selected runners\nEach block represents 10 runners"
filllab <- "Tickets"
i <- sample(seq(1, size), s)
frameSample <- frameLottery[frameLottery$sim %in% i, ]
frameSample$sim <- factor(frameSample$sim)
levels(frameSample$sim) <- rev(levels(frameSample$sim))
ggplot(frameSample, aes(x = sim, fill = tickets)) + geom_bar(width = 1) + geom_hline(y = seq(0, 
    spots, 10), color = "white") + geom_vline(x = seq(1, s) - 0.5, color = "white") + 
    scale_fill_brewer(type = "div", palette = "BrBG") + scale_y_continuous(expand = c(0, 
    0)) + labs(title = title, x = xlab, y = ylab, fill = filllab) + coord_flip() + 
    theme(legend.position = "top")
```

![plot of chunk PlotLotteryResults](figure/PlotLotteryResults.png) 



Actual results
--------------
The actual lottery was held on December 7, 2012. Here are the results, from the Western States Endurance Run [Facebook post](https://www.facebook.com/permalink.php?story_fbid=10152053026305412&id=293403870411),

> Congratulations to all those chosen in the lottery today. Here are some stats for the ticket groups of the 270 selected (and predicted by our MC simulations)  
> one ticket - 101 selected (112 predicted))  
> two tickets - 78 (70)  
> three tickets - 45 (47)  
> four tickets -27 (25)  
> five tickets - 19 (15)  

Compare the observed lottery to the expected lottery.

```r
yObs <- c(101, 78, 45, 27, 19)
yExp <- simsum$EV
frameObsExp <- data.frame(yObs, yExp, diff = yObs - yExp)
names(frameObsExp) <- c("Observed", "Expected", "Difference")
print(xtable(frameObsExp), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Sat Dec 07 18:04:29 2013 -->
<TABLE border=1>
<TR> <TH> Observed </TH> <TH> Expected </TH> <TH> Difference </TH>  </TR>
  <TR> <TD align="right"> 101.00 </TD> <TD align="right"> 112.34 </TD> <TD align="right"> -11.34 </TD> </TR>
  <TR> <TD align="right"> 78.00 </TD> <TD align="right"> 70.65 </TD> <TD align="right"> 7.35 </TD> </TR>
  <TR> <TD align="right"> 45.00 </TD> <TD align="right"> 47.17 </TD> <TD align="right"> -2.17 </TD> </TR>
  <TR> <TD align="right"> 27.00 </TD> <TD align="right"> 25.00 </TD> <TD align="right"> 2.00 </TD> </TR>
  <TR> <TD align="right"> 19.00 </TD> <TD align="right"> 14.84 </TD> <TD align="right"> 4.16 </TD> </TR>
   </TABLE>


I wonder how many times this outcome appeared in my simulated lotteries?

```r
y1 <- aggLottery$tickets[, 1] == yObs[1]
y2 <- aggLottery$tickets[, 2] == yObs[2]
y3 <- aggLottery$tickets[, 3] == yObs[3]
y4 <- aggLottery$tickets[, 4] == yObs[4]
y5 <- aggLottery$tickets[, 5] == yObs[5]
y <- y1 & y2 & y3 & y4 & y5
aggLottery[y, ]
```

```
##         sim tickets.1 tickets.2 tickets.3 tickets.4 tickets.5
## 79817 79817       101        78        45        27        19
```

Of the 1e+05 simulated lotteries, only 1, or 0.00100%, matched the exact outcome of the actual lottery.
