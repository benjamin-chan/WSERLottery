2014 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2013-11-27 22:19:40` using `R version 2.15.3 (2013-03-01)`.


From the [2014 WSER lottery page](http://www.wser.org/lottery2014.html)
> Total Tickets: 4316     Total Entrants: 2715
>  
> Last Updated: 11/27/2013 4:00PM PST
>  
> Ticket Count | Entrants | Tickets  
> -------------|----------|--------
> 5 | 52 | 260  
> 4 | 106 | 424  
> 3 | 257 | 771  
> 2 | 561 | 1122  
> 1 | 1739 | 1739  

This information is different from what is shown on the [2014 lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics/) page.
> Tickets | # of Entrants | Probability (%) | Expected # Selected | Expected % Selected  
> --------|---------------|-----------------|---------------------|--------------------
> 1 | 1749 | 6.46 | 112.9 | 41.8  
> 2 | 565 | 12.49 | 70.6 | 26.1  
> 3 | 257 | 18.16 | 46.7 | 17.3  
> 4 | 107 | 23.43 | 25.1 | 9.3  
> 5 | 52 | 28.39 | 14.8 | 5.5  
> Totals | 2730 |  | 270.0 | 100.0


Here, I run a simulation of the lottery process to estimate probabilities of winning a slot for the Western States Endurance Run. The simulation does a few things
* Accounts for the changing probability distribution of the lottery hat as runners are selected
* Once a runner is selected, their tickets are withdrawn from the hat
* For each simulated lottery, the distribution of selected 1-ticket, 2-ticket, 3-ticket, 4-ticket, adn 5-ticket runners is determined
* From this distributions of the simulated lotteries, the estimated selection probabilities are characterized
* Selection probability distributions are plotted
* Selection distributions from the simulated lotteries are also plotted

Load the required packages.

```r
require(xtable, quietly = TRUE)
require(ggplot2, quietly = TRUE)
```



Setting up initial conditions
-----------------------------
Here is the code to set up the lottery hat data frame at the initial state. Print out some validation output just to make sure the initial state is set up correctly.

```r
spots <- 270
distn <- c(1739, 561, 257, 106, 52)
applicants <- sum(distn)
runner <- seq(1, applicants)
tickets <- c(rep(5, distn[5]), rep(4, distn[4]), rep(3, distn[3]), rep(2, distn[2]), 
    rep(1, distn[1]))
dfHat <- data.frame(runner, tickets)
dfHat$prob <- dfHat$tickets/sum(dfHat$tickets)
addmargins(table(factor(dfHat$tickets)))
```

```
## 
##    1    2    3    4    5  Sum 
## 1739  561  257  106   52 2715
```

```r
head(dfHat)
```

```
##   runner tickets     prob
## 1      1       5 0.001158
## 2      2       5 0.001158
## 3      3       5 0.001158
## 4      4       5 0.001158
## 5      5       5 0.001158
## 6      6       5 0.001158
```

```r
tail(dfHat)
```

```
##      runner tickets      prob
## 2710   2710       1 0.0002317
## 2711   2711       1 0.0002317
## 2712   2712       1 0.0002317
## 2713   2713       1 0.0002317
## 2714   2714       1 0.0002317
## 2715   2715       1 0.0002317
```



Lottery simulation
------------------
The simulation needs to account for the changing relative distribution of tickets after a person is selected and their tickets are no longer in the pool of eligible tickets.

The matrix `lottery` is an $I \times J$ matrix where row $i$ is the $i$-th simulation and the column $j$ is the $j$-th lottery winner drawn. The number of columns in the matrix is `270`, variable `spots`. The number of simulated lotteries is variable `size`. Set the random number seed as the date of the lottery in numeric form multipied by the number of applicants.

```r
size <- 10000
set.seed(as.numeric(as.Date("2013-11-27", format = "%Y-%m-%d")) * applicants)
lottery <- matrix(nrow = size, ncol = spots)
system.time(for (i in 1:size) {
    lottery[i, ] <- sample(dfHat$runner, spots, prob = dfHat$prob)
})
```

```
##    user  system elapsed 
##   83.67    0.30   85.20
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
## [1] 5810
## 
## $runner
##   [1]    4    6   20   27   28   31   33   35   38   44   46   47   51   54
##  [15]   56   57   66   68   69   70   75   83   85   88   91  100  101  113
##  [29]  114  120  122  127  136  138  140  141  146  147  152  154  158  159
##  [43]  168  169  171  181  187  189  190  193  194  195  202  212  214  220
##  [57]  221  222  235  242  243  248  253  264  273  282  287  299  316  323
##  [71]  346  357  361  369  377  389  392  400  404  405  415  418  419  420
##  [85]  430  438  456  459  485  499  505  512  532  557  569  583  585  617
##  [99]  621  639  641  643  645  646  651  659  670  671  672  677  680  681
## [113]  694  697  709  712  715  716  725  742  746  748  751  756  760  769
## [127]  772  776  777  788  789  791  796  797  817  820  823  829  834  840
## [141]  844  855  856  857  861  862  870  873  887  891  898  905  906  911
## [155]  932  933  946  953  960  968  981  985  993 1001 1014 1015 1022 1029
## [169] 1044 1075 1086 1105 1108 1128 1134 1233 1253 1283 1331 1343 1351 1365
## [183] 1384 1391 1403 1404 1427 1497 1500 1502 1522 1529 1544 1548 1561 1583
## [197] 1605 1609 1615 1679 1691 1717 1757 1774 1783 1791 1808 1826 1839 1853
## [211] 1880 1899 1910 1924 1935 1977 1984 2018 2033 2041 2042 2055 2076 2080
## [225] 2086 2092 2107 2127 2142 2154 2159 2160 2170 2206 2210 2229 2286 2301
## [239] 2311 2325 2347 2352 2355 2358 2362 2380 2385 2390 2393 2408 2421 2435
## [253] 2486 2499 2524 2526 2529 2535 2552 2555 2561 2569 2571 2600 2605 2629
## [267] 2635 2671 2689 2699
```

Here's the distribution of the category of ticket holders from that random simulated lottery.

```r
addmargins(table(dfHat$tickets[sampLottery$runner]))
```

```
## 
##   1   2   3   4   5 Sum 
## 110  79  40  28  13 270
```

I.e., in simulated lottery `5810`, 
* `110` applicants with 1 ticket were selected  (`6.3`%)
* `79` applicants with 2 tickets were selected (`14`%)
* `40` applicants with 3 tickets were selected (`16`%)
* `28` applicants with 4 tickets were selected (`26`%)
* `13` applicants with 5 tickets were selected (`25`%)

Okay... but what happened with the other `9999` simulated lotteries?

Format lottery simulation data
------------------------------
I'm not really interested in which runners were selected in the lottery simulation. What I'm really after are estimates for the probability of selecting a runner, among the `270` available spots, with $X$ tickets in the initial hat.

To get at this, first I'll have to match the runners selected to the number of tickets they started out with.

```r
lottery2 <- matrix(nrow = size, ncol = spots)
for (i in 1:size) {
    lottery2[i, ] <- dfHat$tickets[lottery[i, ]]
}
```

Reformat the `lottery2` matrix to an aggregated data frame for analysis.

```r
tickets <- factor(as.vector(t(lottery2)))
sim <- rep(seq(1, size), each = spots)
dfLottery <- data.frame(sim, tickets)
system.time(aggLottery <- aggregate(tickets ~ sim, dfLottery, table))
```

```
##    user  system elapsed 
##  77.477   2.264  81.788
```

```r
sim <- rep(seq(1, size), each = 5)
tickets <- factor(rep(seq(1, 5), size))
freq <- as.vector(t(aggLottery$tickets))
dfSummary <- data.frame(sim, tickets, freq)
```

Save the aggregated data frame for other analysis.

```r
setwd("~/Copy/Sandbox/WSERLottery")
save(aggLottery, file = "aggLottery.RData")
```


For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the proportion of selected applicants. 

```r
total <- rep(distn, size)
dfSummary$prob <- 100 * (dfSummary$freq/total)
avg <- aggregate(prob ~ tickets, dfSummary, mean)
med <- aggregate(prob ~ tickets, dfSummary, median)
sd <- aggregate(prob ~ tickets, dfSummary, sd)
ev <- distn * avg[, 2]/100
probWSER <- c(6.46, 12.49, 18.16, 23.43, 28.39)
evWSER <- distn * probWSER/100
diffprob <- avg[, 2] - probWSER
diffev <- ev - evWSER
pctdiff <- 100 * diffprob/avg[, 2]
simsum <- data.frame(avg, med[, 2], sd[, 2], distn, ev, probWSER, evWSER, diffprob, 
    diffev, pctdiff)
names(simsum) <- c("Tickets", "Mean", "Median", "SD", "N", "EV", "Prob (WSER)", 
    "EV (WSER)", "Diff. prob.", "Diff. EV", "% diff.")
```


Summarize lottery simulations
-----------------------------
Plot the distribution of probabilities from the `10,000` simulated lotteries. Annotate with the estimated mean selection probability.

```r
y1 <- max(density(dfSummary$prob[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$prob[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$prob[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$prob[dfSummary$tickets == 4])$y)
y5 <- max(density(dfSummary$prob[dfSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
g <- ggplot(dfSummary, aes(x = prob, y = ..density.., fill = tickets))
g <- g + geom_density(alpha = 1/2, color = NA)
g <- g + scale_fill_brewer(type = "div", palette = "BrBG")
g <- g + labs(title = "2014 WSER Lottery Selection Probability Densities", x = "Percent", 
    y = paste("Proportion of", format(size, big.mark = ","), "simulations"), 
    fill = "Tickets")
g <- g + annotate("text", label = paste(format(simsum$Mean, digits = 2, trim = TRUE), 
    "%", sep = ""), x = simsum$Mean, y = y)
g <- g + theme(legend.position = "bottom")
g
```

![plot of chunk PlotProbabilities](figure/PlotProbabilities.png) 

As expected, the spread of the selection probabilities increases as the number of tickets a person has in the hat increases (the variance of a binomial random variable increases with $p$).

Another way to think about the lottery is to plot the distribution of the frequency of runners selected by number of tickets. Annotate with the estimated expected value.

```r
y1 <- max(density(dfSummary$freq[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$freq[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$freq[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$freq[dfSummary$tickets == 4])$y)
y5 <- max(density(dfSummary$freq[dfSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
g <- ggplot(dfSummary, aes(x = freq, y = ..density.., fill = tickets))
g <- g + geom_density(alpha = 1/2, color = NA)
g <- g + scale_fill_brewer(type = "div", palette = "BrBG")
g <- g + labs(title = "2014 WSER Lottery Selection Distribution Densities", 
    x = "Number", y = paste("Proportion of", format(size, big.mark = ","), "simulations"), 
    fill = "Tickets")
g <- g + annotate("text", label = format(simsum$EV, digits = 2, trim = TRUE), 
    x = simsum$EV, y = y)
g <- g + theme(legend.position = "bottom")
g
```

![plot of chunk PlotFrequencies](figure/PlotFrequencies.png) 


Print a table summarizing the simulated lotteries showing the mean and median selection probabilities and their standard deviations. Compare to probabilities given in the WSER 2014 [lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics/).

```r
print(xtable(simsum), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Nov 27 22:23:23 2013 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 6.49 </TD> <TD align="right"> 6.50 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 1739.00 </TD> <TD align="right"> 112.83 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 112.34 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.43 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 12.57 </TD> <TD align="right"> 12.48 </TD> <TD align="right"> 1.21 </TD> <TD align="right"> 561.00 </TD> <TD align="right"> 70.49 </TD> <TD align="right"> 12.49 </TD> <TD align="right"> 70.07 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 0.60 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 18.24 </TD> <TD align="right"> 18.29 </TD> <TD align="right"> 2.20 </TD> <TD align="right"> 257.00 </TD> <TD align="right"> 46.87 </TD> <TD align="right"> 18.16 </TD> <TD align="right"> 46.67 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> 0.42 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 23.56 </TD> <TD align="right"> 23.58 </TD> <TD align="right"> 3.94 </TD> <TD align="right"> 106.00 </TD> <TD align="right"> 24.98 </TD> <TD align="right"> 23.43 </TD> <TD align="right"> 24.84 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.57 </TD> </TR>
  <TR> <TD> 5 </TD> <TD align="right"> 28.54 </TD> <TD align="right"> 28.85 </TD> <TD align="right"> 6.19 </TD> <TD align="right"> 52.00 </TD> <TD align="right"> 14.84 </TD> <TD align="right"> 28.39 </TD> <TD align="right"> 14.76 </TD> <TD align="right"> 0.15 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.52 </TD> </TR>
   </TABLE>

My estimates and the probabilities calculated by WSER are essentially identical. Percent difference is never more than `0.5996`%.

Plot the outcomes of a random sample of the `10,000` simulated lotteries as a [waffle plot](http://www.improving-visualisation.org/vis/id=179). The width of each bar represents the number of selected runners. Blocks represent 10 runners.

```r
s <- 100
i <- sample(seq(1, size), s)
dfSample <- dfLottery[dfLottery$sim %in% i, ]
dfSample$sim <- factor(dfSample$sim)
levels(dfSample$sim) <- rev(levels(dfSample$sim))
g <- ggplot(dfSample, aes(x = sim, fill = tickets))
g <- g + geom_bar(width = 1)
g <- g + geom_hline(y = seq(0, spots, 10), color = "white")
g <- g + geom_vline(x = seq(1, s) - 0.5, color = "white")
g <- g + scale_fill_brewer(type = "div", palette = "BrBG")
g <- g + scale_y_continuous(expand = c(0, 0))
g <- g + labs(title = paste("Simulated 2014 WSER Lotteries\n", "Sample of", 
    s, "Lotteries"), x = "Simulated lottery", y = "Number of selected runners", 
    fill = "Tickets")
g <- g + coord_flip()
g <- g + theme(legend.position = "top")
g
```

![plot of chunk PlotLotteryResults](figure/PlotLotteryResults.png) 



Actual results
--------------
**Need to update after the lottery results are released**
The actual lottery was held on December 8, 2012. Here are the results.

> Percentages for tickets holders drawn in lottery today:  
> 1 Ticket - 128 8.6%  
> 2 Ticket - 68 14.1%  
> 3 Ticket - 37 17.8%  
> 4 Ticket - 42 34.4%  

These results include the 5 winners of the 
> Bonus Drawing at the end of the Lottery giv[ing] those present “one last chance” to be selected.

So I'll need to subtract out the 5 winners of the bonus drawing. Three of the bonus drawing winners were 1-ticket holders, 1 was a 3-ticket holder, and 1 was a 4-ticket holder.

Compare the observed lottery to the expected lottery.

```r
yObs <- c(128 - 3, 68, 37 - 1, 42 - 1)
yExp <- simsum$EV
dfObsExp <- data.frame(yObs, yExp, diff = yObs - yExp)
names(dfObsExp) <- c("Observed", "Expected", "Difference")
print(xtable(dfObsExp), type = "html", include.rownames = FALSE)
```


I wonder how many times this outcome appeared in my simulated lotteries. 

```r
y1 <- aggLottery$tickets[, 1] == yObs[1]
y2 <- aggLottery$tickets[, 2] == yObs[2]
y3 <- aggLottery$tickets[, 3] == yObs[3]
y4 <- aggLottery$tickets[, 4] == yObs[4]
y5 <- aggLottery$tickets[, 5] == yObs[5]
y <- y1 & y2 & y3 & y4 & y5
aggLottery[y, ]
```

Of the `10,000` simulated lotteries, only *r sum(y)*, or *r 100 * sum(y) / size*%, matched the exact outcome of the actual lottery.
