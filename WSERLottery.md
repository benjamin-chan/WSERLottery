2013 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2012-12-05 16:19:35` using `R version 2.15.2 (2012-10-26)`.

I was a little intrigued by how the Western States Endurance Run (WSER) calculated their lottery odds estimates. Their estimates can be found in the WSER 2012 [lottery details](http://www.wser.org/2012/12/03/dec-8-lottery-details).

On 12/5/2012, WSER updated their lottery details from the version they originally posted on 12/3/2012 ([cached version](http://webcache.googleusercontent.com/search?q=cache:x6E-Gb-wm_wJ:www.wser.org/2012/12/03/dec-8-lottery-details/+&cd=2&hl=en&ct=clnk&gl=us)). The revised probabilities more closely match the results I was coming up with. The number of applicants and the number of tickets also have been revised from what was on the website on 12/3/2012.

> Updated 12/5
> 
> As posted on the [lottery applicant](http://www.wser.org/lottery2013.html) page, we have 2295 total lottery applicants for the 2013 race.
> 
> 122 applicants with four tickets = 488 tickets  
> 207 applicants with three tickets = 621 tickets  
> 480 applicants with two tickets = 960 tickets  
> 1486 applicants with one ticket = 1486 tickets
> 
> Total tickets in the hat = 3555
> 
> Here is the list of tickets ([178 page pdf](http://www.wser.org/wp-content/uploads/2012/12/2013LotteryTickets.pdf))
> 
> We are going to draw 270 unique names in the lottery and then an additional five from the lottery within the lottery. That is, the folks in the audience who have not been selected up to that point. Based on 3555 total tickets and 270 names drawn, the odds of getting selected have been updated as follows:
> 
> one ticket odds = 7.9%  
> two ticket odds = 15.2%  
> three ticket odds = 21.9%  
> four ticket odds = 28.0%
> 
> We used a Monte Carlo simulation to calculate these updated odds.

What are labeled as *odds* are really *probabilities*.

Here, I run a simulation of the lottery process to estimate probabilities of winning a slot for the Western States Endurance Run. The simulation does a few things
* Accounts for the changing probability distribution of the lottery hat as runners are selected
* Once a runner is selected, their tickets are withdrawn from the hat
* For each simulated lottery, the distribution of selected 1-ticket, 2-ticket, 3-ticket, and 4-ticket runners is determined
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
distn <- c(1486, 480, 207, 122)
applicants <- sum(distn)
runner <- seq(1, applicants)
tickets <- c(rep(4, distn[4]), rep(3, distn[3]), rep(2, distn[2]), rep(1, distn[1]))
dfHat0 <- data.frame(runner, tickets)
dfHat0$prob <- dfHat0$tickets/sum(dfHat0$tickets)
table(factor(dfHat0$tickets))
```

```
## 
##    1    2    3    4 
## 1486  480  207  122
```

```r
head(dfHat0)
```

```
##   runner tickets     prob
## 1      1       4 0.001125
## 2      2       4 0.001125
## 3      3       4 0.001125
## 4      4       4 0.001125
## 5      5       4 0.001125
## 6      6       4 0.001125
```

```r
tail(dfHat0)
```

```
##      runner tickets      prob
## 2290   2290       1 0.0002813
## 2291   2291       1 0.0002813
## 2292   2292       1 0.0002813
## 2293   2293       1 0.0002813
## 2294   2294       1 0.0002813
## 2295   2295       1 0.0002813
```



Lottery simulation
------------------
The simulation needs to account for the changing relative distribution of tickets after a person is selected and their tickets are no longer in the pool of eligible tickets.

The matrix `lottery` is an $I \times J$ matrix where row $i$ is the $i$-th simulation and the column $j$ is the $j$-th lottery winner drawn. The number of columns in the matrix is `270`, variable `spots`. The number of simulated lotteries is variable `size`. Set the random number seed as the date of the lottery in numeric form multipied by the number of applicants.

```r
size <- 1000
set.seed(as.numeric(as.Date("2012-12-08", format = "%Y-%m-%d")) * applicants)
lottery <- matrix(nrow = size, ncol = spots)
start <- Sys.time()
for (i in 1:size) {
    dfHat <- dfHat0
    for (j in 1:spots) {
        lottery[i, j] <- sample(dfHat$runner, 1, prob = dfHat$prob)
        dfHat <- subset(dfHat, runner != lottery[i, j])
        dfHat$prob <- dfHat$tickets/sum(dfHat$tickets)
    }
}
end <- Sys.time()
end - start
```

```
## Time difference of 2.815 mins
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
## [1] 778
## 
## $runner
##   [1]    2    8   11   12   14   15   16   22   24   26   28   29   32   33
##  [15]   41   46   48   50   51   52   60   66   76   83   85   92   93   96
##  [29]   98  111  113  128  138  144  147  148  156  157  163  166  167  176
##  [43]  179  180  182  183  186  189  190  193  195  196  198  207  213  214
##  [57]  228  238  240  243  244  245  252  254  274  279  282  294  303  307
##  [71]  308  319  322  329  331  335  341  343  360  364  372  377  390  395
##  [85]  402  410  413  414  418  422  445  450  453  459  463  466  482  499
##  [99]  503  513  531  533  540  544  545  556  560  561  569  584  587  595
## [113]  599  600  605  607  609  610  611  613  624  625  627  635  643  652
## [127]  654  661  670  675  679  683  691  692  713  716  724  725  728  730
## [141]  733  734  735  747  752  754  756  760  768  783  791  793  801  802
## [155]  829  851  863  912  916  936  939  940  946  990  995 1019 1031 1045
## [169] 1055 1068 1105 1108 1135 1138 1209 1229 1234 1254 1291 1299 1303 1311
## [183] 1314 1315 1323 1334 1354 1355 1367 1377 1386 1403 1407 1413 1417 1422
## [197] 1425 1447 1496 1499 1503 1518 1520 1531 1539 1558 1564 1580 1590 1610
## [211] 1629 1640 1641 1692 1711 1731 1754 1761 1765 1799 1806 1809 1814 1821
## [225] 1826 1832 1842 1853 1860 1866 1872 1883 1885 1888 1905 1916 1924 1928
## [239] 1931 1933 1951 1952 1953 1957 1959 1987 2008 2016 2041 2042 2043 2055
## [253] 2063 2065 2074 2077 2110 2119 2134 2139 2152 2168 2187 2197 2207 2239
## [267] 2255 2258 2282 2283
```



Format lottery simulation data
------------------------------
I'm not really interested in which runners were selected in the lottery simulation. What I'm really after are estimates for the probability of selecting a runner, among the `270` available spots, with $X$ tickets in the initial hat.

To get at this, first I'll have to match the runners selected to the number of tickets they started out with.

```r
lottery2 <- matrix(nrow = size, ncol = spots)
for (i in 1:size) {
    lottery2[i, ] <- dfHat0$tickets[lottery[i, ]]
}
```

Reformat the `lottery2` matrix to an aggregated data frame for analysis.

```r
tickets <- factor(as.vector(t(lottery2)))
sim <- rep(seq(1, size), each = spots)
dfLottery <- data.frame(sim, tickets)
aggLottery <- aggregate(tickets ~ sim, dfLottery, table)
sim <- rep(seq(1, size), each = 4)
tickets <- factor(rep(seq(1, 4), size))
freq <- as.vector(t(aggLottery$tickets))
dfSummary <- data.frame(sim, tickets, freq)
```

For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the proportion of selected applicants. 

```r
total <- rep(distn, size)
dfSummary$prob <- 100 * (dfSummary$freq/total)
avg <- aggregate(prob ~ tickets, dfSummary, mean)
med <- aggregate(prob ~ tickets, dfSummary, median)
sd <- aggregate(prob ~ tickets, dfSummary, sd)
ev <- distn * avg[, 2]/100
probWSER <- c(7.9, 15.2, 21.9, 28)
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
Plot the distribution of probabilities from the `1,000` simulated lotteries. Annotate with the estimated mean selection probability.

```r
y1 <- max(density(dfSummary$prob[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$prob[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$prob[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$prob[dfSummary$tickets == 4])$y)
y <- c(y1, y2, y3, y4)
ggplot(dfSummary, aes(x = prob, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = "2012 WSER Lottery Selection Probability Densities", 
    x = "Percent", y = paste("Proportion of", format(size, big.mark = ","), 
        "simulations"), fill = "Tickets") + annotate("text", label = paste(format(simsum$Mean, 
    digits = 2, trim = TRUE), "%", sep = ""), x = simsum$Mean, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotProbabilities](figure/PlotProbabilities.png) 

As expected, the spread of the selection probabilities increases as the number of tickets a person has in the hat increases (the variance of a binomial random variable increases with $p$).

Another way to think about the lottery is to plot the distribution of the frequency of runners selected by number of tickets. Annotate with the estimated expected value.

```r
y1 <- max(density(dfSummary$freq[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$freq[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$freq[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$freq[dfSummary$tickets == 4])$y)
y <- c(y1, y2, y3, y4)
ggplot(dfSummary, aes(x = freq, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = "2012 WSER Lottery Selection Distribution Densities", 
    x = "Number", y = paste("Proportion of", format(size, big.mark = ","), "simulations"), 
    fill = "Tickets") + annotate("text", label = format(simsum$EV, digits = 2, 
    trim = TRUE), x = simsum$EV, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotFrequencies](figure/PlotFrequencies.png) 


Print a table summarizing the simulated lotteries showing the mean and median selection probabilities and their standard deviations. Compare to probabilities given in the WSER 2012 [lottery details](http://www.wser.org/2012/12/03/dec-8-lottery-details).

```r
print(xtable(simsum), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 2.15.2 by xtable 1.7-0 package -->
<!-- Wed Dec 05 16:22:27 2012 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 7.92 </TD> <TD align="right"> 7.94 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 1486.00 </TD> <TD align="right"> 117.67 </TD> <TD align="right"> 7.90 </TD> <TD align="right"> 117.39 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.28 </TD> <TD align="right"> 0.23 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 15.18 </TD> <TD align="right"> 15.21 </TD> <TD align="right"> 1.39 </TD> <TD align="right"> 480.00 </TD> <TD align="right"> 72.84 </TD> <TD align="right"> 15.20 </TD> <TD align="right"> 72.96 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> -0.12 </TD> <TD align="right"> -0.16 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 21.86 </TD> <TD align="right"> 21.74 </TD> <TD align="right"> 2.58 </TD> <TD align="right"> 207.00 </TD> <TD align="right"> 45.25 </TD> <TD align="right"> 21.90 </TD> <TD align="right"> 45.33 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.08 </TD> <TD align="right"> -0.17 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 28.06 </TD> <TD align="right"> 27.87 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 122.00 </TD> <TD align="right"> 34.23 </TD> <TD align="right"> 28.00 </TD> <TD align="right"> 34.16 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.22 </TD> </TR>
   </TABLE>

My estimates and the probabilities calculated by WSER are essentially identical. Percent difference is never more than `0.2346`%.

Plot the outcomes of a random sample of the `1,000` simulated lotteries.

```r
s <- 100
i <- sample(seq(1, size), s)
dfSample <- dfLottery[dfLottery$sim %in% i, ]
dfSample$sim <- factor(dfSample$sim)
levels(dfSample$sim) <- rev(levels(dfSample$sim))
ggplot(dfSample, aes(x = sim, fill = tickets)) + geom_bar(width = 1) + scale_fill_brewer(type = "div", 
    palette = "BrBG") + labs(title = paste("Simulated 2012 WSER Lotteries\n", 
    "Sample of", s, "Lotteries"), x = "Simulated lottery", y = "Number of selected runners", 
    fill = "Tickets") + coord_flip() + theme(legend.position = "top")
```

![plot of chunk PlotLotteryResults](figure/PlotLotteryResults.png) 

