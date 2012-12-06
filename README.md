2013 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2012-12-06 10:50:27` using `R version 2.15.2 (2012-10-26)`.

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
dfHat <- data.frame(runner, tickets)
dfHat$prob <- dfHat$tickets/sum(dfHat$tickets)
table(factor(dfHat$tickets))
```

```
## 
##    1    2    3    4 
## 1486  480  207  122
```

```r
head(dfHat)
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
tail(dfHat)
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
size <- 10000
set.seed(as.numeric(as.Date("2012-12-08", format = "%Y-%m-%d")) * applicants)
lottery <- matrix(nrow = size, ncol = spots)
start <- Sys.time()
for (i in 1:size) {
    lottery[i, ] <- sample(dfHat$runner, spots, prob = dfHat$prob)
}
end <- Sys.time()
end - start
```

```
## Time difference of 6.693 secs
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
## [1] 9294
## 
## $runner
##   [1]    5    9   10   13   17   21   22   24   29   30   33   35   41   42
##  [15]   49   50   55   58   59   62   65   68   72   74   75   89   91   93
##  [29]   95   99  100  101  106  110  118  121  125  127  129  130  135  138
##  [43]  139  140  149  163  170  174  179  185  188  190  193  194  197  201
##  [57]  205  216  218  222  230  236  238  244  250  258  266  271  279  285
##  [71]  286  291  294  303  308  313  314  318  320  324  337  338  339  341
##  [85]  346  350  366  371  381  388  396  400  406  414  417  418  427  433
##  [99]  439  442  443  448  454  465  466  468  488  496  499  516  518  531
## [113]  549  564  574  575  577  588  593  602  605  612  614  620  636  637
## [127]  638  639  641  645  654  657  660  664  665  673  713  727  728  745
## [141]  749  754  757  773  775  778  780  792  800  809  838  848  864  872
## [155]  876  891  899  901  912  927  947  955  969  973  981 1002 1010 1018
## [169] 1033 1048 1072 1081 1091 1093 1094 1110 1173 1185 1193 1197 1202 1210
## [183] 1215 1234 1240 1245 1254 1303 1325 1327 1328 1340 1344 1355 1366 1375
## [197] 1381 1394 1426 1432 1459 1461 1478 1483 1496 1512 1523 1524 1530 1541
## [211] 1545 1549 1603 1616 1644 1666 1669 1690 1698 1717 1720 1725 1729 1761
## [225] 1771 1773 1776 1786 1787 1806 1807 1844 1893 1898 1899 1905 1906 1907
## [239] 1963 1970 1992 1993 1995 2012 2022 2034 2039 2046 2057 2067 2080 2095
## [253] 2097 2103 2118 2130 2161 2175 2177 2182 2204 2209 2212 2225 2228 2229
## [267] 2231 2240 2250 2281
```

Here's the distribution of the category of ticket holders from that random simulated lottery.

```r
table(dfHat$tickets[sampLottery$runner])
```

```
## 
##   1   2   3   4 
## 120  70  44  36
```

I.e., in simulated lottery `9294`, 
* `120` applicants with 1 ticket were selected  (`8.1`%)
* `70` applicants with 2 tickets were selected (`15`%)
* `44` applicants with 3 tickets were selected (`21`%)
* `36` applicants with 4 tickets were selected (`30`%)

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
start <- Sys.time()
tickets <- factor(as.vector(t(lottery2)))
sim <- rep(seq(1, size), each = spots)
dfLottery <- data.frame(sim, tickets)
aggLottery <- aggregate(tickets ~ sim, dfLottery, table)
sim <- rep(seq(1, size), each = 4)
tickets <- factor(rep(seq(1, 4), size))
freq <- as.vector(t(aggLottery$tickets))
dfSummary <- data.frame(sim, tickets, freq)
end <- Sys.time()
end - start
```

```
## Time difference of 13.8 secs
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
Plot the distribution of probabilities from the `10,000` simulated lotteries. Annotate with the estimated mean selection probability.

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
<!-- Thu Dec 06 10:50:50 2012 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> EV </TH> <TH> Prob (WSER) </TH> <TH> EV (WSER) </TH> <TH> Diff. prob. </TH> <TH> Diff. EV </TH> <TH> % diff. </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 7.91 </TD> <TD align="right"> 7.87 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 1486.00 </TD> <TD align="right"> 117.47 </TD> <TD align="right"> 7.90 </TD> <TD align="right"> 117.39 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.07 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 15.19 </TD> <TD align="right"> 15.21 </TD> <TD align="right"> 1.40 </TD> <TD align="right"> 480.00 </TD> <TD align="right"> 72.91 </TD> <TD align="right"> 15.20 </TD> <TD align="right"> 72.96 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.05 </TD> <TD align="right"> -0.06 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 21.91 </TD> <TD align="right"> 21.74 </TD> <TD align="right"> 2.65 </TD> <TD align="right"> 207.00 </TD> <TD align="right"> 45.36 </TD> <TD align="right"> 21.90 </TD> <TD align="right"> 45.33 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 28.08 </TD> <TD align="right"> 27.87 </TD> <TD align="right"> 3.81 </TD> <TD align="right"> 122.00 </TD> <TD align="right"> 34.26 </TD> <TD align="right"> 28.00 </TD> <TD align="right"> 34.16 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.29 </TD> </TR>
   </TABLE>

My estimates and the probabilities calculated by WSER are essentially identical. Percent difference is never more than `0.2913`%.

Plot the outcomes of a random sample of the `10,000` simulated lotteries as a [waffle plot](http://www.improving-visualisation.org/vis/id=179). The width of each bar represents the number of selected runners. Blocks represent 10 runners.

```r
s <- 100
i <- sample(seq(1, size), s)
dfSample <- dfLottery[dfLottery$sim %in% i, ]
dfSample$sim <- factor(dfSample$sim)
levels(dfSample$sim) <- rev(levels(dfSample$sim))
ggplot(dfSample, aes(x = sim, fill = tickets)) + geom_bar(width = 1) + geom_hline(y = seq(0, 
    spots, 10), color = "white") + geom_vline(x = seq(1, s) - 0.5, color = "white") + 
    scale_fill_brewer(type = "div", palette = "BrBG") + scale_y_continuous(expand = c(0, 
    0)) + labs(title = paste("Simulated 2012 WSER Lotteries\n", "Sample of", 
    s, "Lotteries"), x = "Simulated lottery", y = "Number of selected runners", 
    fill = "Tickets") + coord_flip() + theme(legend.position = "top")
```

![plot of chunk PlotLotteryResults](figure/PlotLotteryResults.png) 

