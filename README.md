2014 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on 2013-11-29 08:29:15 using R version 3.0.2 (2013-09-25).


Details
-------
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

This information is different from what is shown on the [2014 lottery statistics](http://www.wser.org/2013/11/27/2014-lottery-statistics/) page. Since this is where selection probabilities are shown, **use this table** for input data on the number of tickets and entrants.
> Tickets | # of Entrants | Probability (%) | Expected # Selected | Expected % Selected  
> --------|---------------|-----------------|---------------------|--------------------
> 1 | 1749 | 6.46 | 112.9 | 41.8  
> 2 | 565 | 12.49 | 70.6 | 26.1  
> 3 | 257 | 18.16 | 46.7 | 17.3  
> 4 | 107 | 23.43 | 25.1 | 9.3  
> 5 | 52 | 28.39 | 14.8 | 5.5  
> Totals | 2730 |  | 270.0 | 100.0


```r
distn <- c(1749, 565, 257, 107, 52)
probWSER <- c(6.46, 12.49, 18.16, 23.43, 28.39)
```



Here, I run a simulation of the lottery process to estimate probabilities of winning a slot for the Western States Endurance Run. The simulation does a few things
* Accounts for the changing probability distribution of the lottery hat as runners are selected
* Once a runner is selected, their tickets are withdrawn from the hat
* For each simulated lottery, the distribution of selected 1-ticket, 2-ticket, 3-ticket, 4-ticket, adn 5-ticket runners is determined
* From this distributions of the simulated lotteries, the estimated selection probabilities are characterized
* Selection probability distributions are plotted
* Selection distributions from the simulated lotteries are also plotted


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
## 1749  565  257  107   52 2730
```

```r
head(frameHat)
```

```
##   runner tickets     prob
## 1      1       5 0.001153
## 2      2       5 0.001153
## 3      3       5 0.001153
## 4      4       5 0.001153
## 5      5       5 0.001153
## 6      6       5 0.001153
```

```r
tail(frameHat)
```

```
##      runner tickets      prob
## 2725   2725       1 0.0002305
## 2726   2726       1 0.0002305
## 2727   2727       1 0.0002305
## 2728   2728       1 0.0002305
## 2729   2729       1 0.0002305
## 2730   2730       1 0.0002305
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
##   79.67    0.04   79.92
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
## [1] 19414
## 
## $runner
##   [1]    2    3    5    6    8    9   10   27   28   36   42   45   48   50
##  [15]   63   64   66   78   85   88   89   92   95   96  105  112  115  121
##  [29]  122  126  131  135  141  143  150  153  154  172  179  183  184  189
##  [43]  195  199  207  225  227  228  233  246  253  255  262  263  285  289
##  [57]  298  299  307  310  313  325  331  333  337  348  354  363  367  371
##  [71]  378  380  382  393  395  397  398  399  401  406  408  411  412  421
##  [85]  424  440  441  444  450  456  472  473  475  483  485  495  501  504
##  [99]  511  525  535  554  575  605  606  617  618  631  635  642  643  679
## [113]  690  703  708  722  731  735  748  752  759  762  768  787  788  790
## [127]  795  797  799  803  822  823  827  829  831  842  847  850  868  871
## [141]  880  893  894  902  909  915  918  936  941  944  946  947  952  953
## [155]  957  964 1019 1074 1077 1125 1137 1143 1152 1158 1168 1170 1172 1192
## [169] 1198 1211 1214 1227 1228 1256 1279 1280 1303 1310 1315 1323 1329 1342
## [183] 1347 1399 1404 1414 1423 1433 1434 1436 1442 1445 1447 1468 1511 1520
## [197] 1527 1537 1547 1567 1575 1599 1612 1660 1673 1702 1719 1736 1745 1749
## [211] 1753 1764 1766 1767 1774 1775 1808 1825 1838 1839 1848 1852 1891 1901
## [225] 1914 1917 1949 1959 1989 1999 2027 2033 2040 2043 2056 2059 2088 2096
## [239] 2106 2110 2126 2130 2132 2136 2140 2144 2255 2272 2276 2301 2304 2310
## [253] 2352 2377 2385 2399 2484 2504 2543 2565 2566 2574 2592 2617 2625 2643
## [267] 2660 2669 2670 2672
```

Here's the distribution of the category of ticket holders from that random simulated lottery.

```r
addmargins(table(frameHat$tickets[sampLottery$runner]))
```

```
## 
##   1   2   3   4   5 Sum 
## 114  73  46  23  14 270
```

I.e., in simulated lottery 19414, 
* 114 applicants with 1 ticket were selected  (6.5%)
* 73 applicants with 2 tickets were selected (13%)
* 46 applicants with 3 tickets were selected (18%)
* 23 applicants with 4 tickets were selected (21%)
* 14 applicants with 5 tickets were selected (27%)

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
##  264.57    1.87  268.04
```

```r
sim <- rep(seq(1, size), each = 5)
tickets <- factor(rep(seq(1, 5), size))
freq <- as.vector(t(aggLottery$tickets))
frameSummary <- data.frame(sim, tickets, freq)
```

Save the aggregated data frame for other analysis.

```r
setwd("~/Copy/Sandbox/WSERLottery")
```

```
## Error: cannot change working directory
```

```r
save(aggLottery, file = "aggLottery.RData")
```


For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the proportion of selected applicants. 

```r
total <- rep(distn, size)
frameSummary$prob <- 100 * (frameSummary$freq/total)
avg <- aggregate(prob ~ tickets, frameSummary, mean)
med <- aggregate(prob ~ tickets, frameSummary, median)
sd <- aggregate(prob ~ tickets, frameSummary, sd)
ev <- distn * avg[, 2]/100
evWSER <- distn * probWSER/100
diffprob <- avg[, 2] - probWSER
diffev <- ev - evWSER
pctdiff <- 100 * diffprob/avg[, 2]
sqerr <- diffprob^2
simsum <- data.frame(avg, med[, 2], sd[, 2], distn, ev, probWSER, evWSER, diffprob, 
    diffev, pctdiff, sqerr)
names(simsum) <- c("Tickets", "Mean", "Median", "SD", "N", "EV", "Prob (WSER)", 
    "EV (WSER)", "Diff. prob.", "Diff. EV", "% diff.", "Sq. error")
```


Summarize lottery simulations
-----------------------------
Plot the distribution of probabilities from the 1e+05 simulated lotteries. Annotate with the estimated mean selection probability.

```r
y1 <- max(density(frameSummary$prob[frameSummary$tickets == 1])$y)
y2 <- max(density(frameSummary$prob[frameSummary$tickets == 2])$y)
y3 <- max(density(frameSummary$prob[frameSummary$tickets == 3])$y)
y4 <- max(density(frameSummary$prob[frameSummary$tickets == 4])$y)
y5 <- max(density(frameSummary$prob[frameSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
require(ggplot2, quietly = TRUE)
g <- ggplot(frameSummary, aes(x = prob, y = ..density.., fill = tickets))
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
y1 <- max(density(frameSummary$freq[frameSummary$tickets == 1])$y)
y2 <- max(density(frameSummary$freq[frameSummary$tickets == 2])$y)
y3 <- max(density(frameSummary$freq[frameSummary$tickets == 3])$y)
y4 <- max(density(frameSummary$freq[frameSummary$tickets == 4])$y)
y5 <- max(density(frameSummary$freq[frameSummary$tickets == 5])$y)
y <- c(y1, y2, y3, y4, y5)
g <- ggplot(frameSummary, aes(x = freq, y = ..density.., fill = tickets))
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
require(xtable, quietly = TRUE)
print(xtable(simsum), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Fri Nov 29 08:35:49 2013 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH> <TH> Sq. error </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 1749.00 </TD> <TD align="right"> 112.95 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 112.99 </TD> <TD align="right"> -0.00 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.03 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 12.50 </TD> <TD align="right"> 12.57 </TD> <TD align="right"> 1.19 </TD> <TD align="right"> 565.00 </TD> <TD align="right"> 70.61 </TD> <TD align="right"> 12.49 </TD> <TD align="right"> 70.57 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 18.14 </TD> <TD align="right"> 18.29 </TD> <TD align="right"> 2.20 </TD> <TD align="right"> 257.00 </TD> <TD align="right"> 46.62 </TD> <TD align="right"> 18.16 </TD> <TD align="right"> 46.67 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.05 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 23.43 </TD> <TD align="right"> 23.36 </TD> <TD align="right"> 3.93 </TD> <TD align="right"> 107.00 </TD> <TD align="right"> 25.07 </TD> <TD align="right"> 23.43 </TD> <TD align="right"> 25.07 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 5 </TD> <TD align="right"> 28.37 </TD> <TD align="right"> 28.85 </TD> <TD align="right"> 6.13 </TD> <TD align="right"> 52.00 </TD> <TD align="right"> 14.75 </TD> <TD align="right"> 28.39 </TD> <TD align="right"> 14.76 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.06 </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>

My estimates and the probabilities calculated by WSER are essentially identical. Percent differences of the selection probabilities are never more than 0.1101% and the mean squared error of the selection probabilities is 0.000157.

Plot the outcomes of a random sample of the 1e+05 simulated lotteries as a [waffle plot](http://www.improving-visualisation.org/vis/id=179). The width of each bar represents the number of selected runners. Blocks represent 10 runners.

```r
s <- 25
i <- sample(seq(1, size), s)
frameSample <- frameLottery[frameLottery$sim %in% i, ]
frameSample$sim <- factor(frameSample$sim)
levels(frameSample$sim) <- rev(levels(frameSample$sim))
g <- ggplot(frameSample, aes(x = sim, fill = tickets))
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
frameObsExp <- data.frame(yObs, yExp, diff = yObs - yExp)
names(frameObsExp) <- c("Observed", "Expected", "Difference")
print(xtable(frameObsExp), type = "html", include.rownames = FALSE)
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

Of the 1e+05 simulated lotteries, only *r sum(y)*, or *r 100 * sum(y) / size*%, matched the exact outcome of the actual lottery.


Copy Markdown file
------------------
Copy the resulting Markdown file to `README.md`.

```r
file.copy("WSERLottery.md", "README.md", overwrite = TRUE)
```

```
## [1] TRUE
```

