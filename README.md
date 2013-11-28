2014 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2013-11-28 11:35:26` using `R version 2.15.3 (2013-03-01)`.


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

The matrix `lottery` is an $I \times J$ matrix where row $i$ is the $i$-th simulation and the column $j$ is the $j$-th lottery winner drawn. The number of columns in the matrix is `270`, variable `spots`. The number of simulated lotteries is variable `size`. Set the random number seed as the date of the lottery in numeric form multipied by the number of applicants.

```r
size <- 25000
dateLottery <- as.Date("2013-12-07", format = "%Y-%m-%d")
set.seed(as.numeric(dateLottery) * applicants)
lottery <- matrix(nrow = size, ncol = spots)
system.time(for (i in 1:size) {
    lottery[i, ] <- sample(frameHat$runner, spots, prob = frameHat$prob)
})
```

```
##    user  system elapsed 
##   201.0     0.2   201.8
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
## [1] 2793
## 
## $runner
##   [1]    9   12   13   16   20   27   28   36   48   56   63   68   73   81
##  [15]   90   95   96   99  103  105  106  111  117  121  128  130  131  136
##  [29]  144  152  157  158  160  161  163  168  169  173  177  190  195  199
##  [43]  204  207  210  222  225  236  248  252  258  261  268  277  294  299
##  [57]  303  304  324  328  329  331  332  336  339  348  357  359  365  368
##  [71]  374  384  386  401  402  410  411  431  440  447  460  463  480  487
##  [85]  497  509  512  524  528  531  536  537  542  543  555  569  579  596
##  [99]  606  615  626  632  663  670  672  680  683  710  720  727  730  738
## [113]  741  747  749  773  787  792  796  797  807  809  815  819  820  827
## [127]  836  845  851  859  860  861  870  875  876  881  890  891  892  905
## [141]  911  919  923  927  935  937  959  961  964  979  984 1005 1006 1040
## [155] 1050 1085 1093 1097 1108 1133 1136 1146 1187 1216 1243 1284 1287 1326
## [169] 1334 1340 1359 1365 1387 1388 1402 1405 1411 1436 1445 1476 1511 1514
## [183] 1518 1569 1574 1607 1615 1623 1632 1656 1671 1699 1703 1709 1718 1721
## [197] 1734 1735 1749 1785 1787 1803 1812 1818 1819 1850 1852 1865 1866 1883
## [211] 1884 1901 1911 1912 1955 1958 1961 1965 1974 1981 1990 2024 2031 2042
## [225] 2044 2049 2050 2060 2072 2103 2110 2112 2119 2154 2171 2172 2207 2211
## [239] 2215 2226 2238 2249 2257 2272 2273 2333 2349 2356 2358 2359 2380 2391
## [253] 2402 2410 2421 2430 2449 2504 2512 2515 2536 2541 2546 2571 2581 2587
## [267] 2609 2670 2679 2709
```

Here's the distribution of the category of ticket holders from that random simulated lottery.

```r
addmargins(table(frameHat$tickets[sampLottery$runner]))
```

```
## 
##   1   2   3   4   5 Sum 
## 120  73  45  23   9 270
```

I.e., in simulated lottery `2793`, 
* `120` applicants with 1 ticket were selected  (`6.9`%)
* `73` applicants with 2 tickets were selected (`13`%)
* `45` applicants with 3 tickets were selected (`18`%)
* `23` applicants with 4 tickets were selected (`21`%)
* `9` applicants with 5 tickets were selected (`17`%)

Okay... but what happened with the other `2.4999 &times; 10<sup>4</sup>` simulated lotteries?


Format lottery simulation data
------------------------------
I'm not really interested in which runners were selected in the lottery simulation. What I'm really after are estimates for the probability of selecting a runner, among the `270` available spots, with $X$ tickets in the initial hat.

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
## 212.037   3.316 216.085
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
Plot the distribution of probabilities from the `25,000` simulated lotteries. Annotate with the estimated mean selection probability.

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

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Thu Nov 28 11:43:49 2013 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH> <TH> Sq. error </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 6.45 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 0.43 </TD> <TD align="right"> 1749.00 </TD> <TD align="right"> 112.87 </TD> <TD align="right"> 6.46 </TD> <TD align="right"> 112.99 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> -0.10 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 12.51 </TD> <TD align="right"> 12.57 </TD> <TD align="right"> 1.20 </TD> <TD align="right"> 565.00 </TD> <TD align="right"> 70.67 </TD> <TD align="right"> 12.49 </TD> <TD align="right"> 70.57 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 18.15 </TD> <TD align="right"> 18.29 </TD> <TD align="right"> 2.19 </TD> <TD align="right"> 257.00 </TD> <TD align="right"> 46.64 </TD> <TD align="right"> 18.16 </TD> <TD align="right"> 46.67 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.03 </TD> <TD align="right"> -0.07 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 23.44 </TD> <TD align="right"> 23.36 </TD> <TD align="right"> 3.93 </TD> <TD align="right"> 107.00 </TD> <TD align="right"> 25.08 </TD> <TD align="right"> 23.43 </TD> <TD align="right"> 25.07 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD> 5 </TD> <TD align="right"> 28.35 </TD> <TD align="right"> 28.85 </TD> <TD align="right"> 6.12 </TD> <TD align="right"> 52.00 </TD> <TD align="right"> 14.74 </TD> <TD align="right"> 28.39 </TD> <TD align="right"> 14.76 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.16 </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>

My estimates and the probabilities calculated by WSER are essentially identical. Percent differences of the selection probabilities are never more than `0.1571`% and the mean squared error of the selection probabilities is `0.000533`.

Plot the outcomes of a random sample of the `25,000` simulated lotteries as a [waffle plot](http://www.improving-visualisation.org/vis/id=179). The width of each bar represents the number of selected runners. Blocks represent 10 runners.

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

Of the `25,000` simulated lotteries, only *r sum(y)*, or *r 100 * sum(y) / size*%, matched the exact outcome of the actual lottery.


Copy Markdown file
------------------
Copy the resulting Markdown file to `README.md`.

```r
file.copy("WSERLottery.md", "README.md", overwrite = TRUE)
```

```
## [1] TRUE
```

