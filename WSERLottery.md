2013 Western States Endurance Run Lottery
=========================================
Last update by Benjamin Chan (<benjamin.ks.chan@gmail.com>) on `2012-12-05 11:01:11` using `R version 2.15.2 (2012-10-26)`.

I was a little intrigued by how the Western States Endurance Run (WSER) calculated their lottery odds estimates. Their estimates can be found on the [WSER website](http://www.wser.org/2012/12/03/dec-8-lottery-details).

> As posted on the lottery applicant page, we have 2302 total lottery applicants for the 2013 race.
>
> 122 applicants with four tickets = 488 tickets
>
> 207 applicants with three tickets = 621 tickets
>
> 482 applicants with two tickets = 964 tickets
>
> 1491 applicants with one ticket = 1491 tickets
>
> Total tickets in the hat = 3564
>
> We are going to draw 270 unique names in the lottery and then an additional five from the lottery within the lottery. That is, the folks in the audience who have not been selected up to that point. Based on 3564 total tickets and 270 names drawn, the odds of getting selected have been calculated as follows:
>
> one ticket odds = 7.6%
>
> two ticket odds = 14.6%
>
> three ticket odds = 21.1%
>
> four ticket odds = 27.1%
>
> $E(X) = \sum X P(X)$. What this is saying (in English) is "The expected value is the sum of all the gains multiplied by their individual probabilities."

First, what are labeled as *odds* appear to really be *probabilities*. Second, it is unclear how the equation for expected value was used to calculate these  probabilities.

At first, I suspected the reason might be the automatic spots. But the text of the lottery process above is clear that the 270 runners that will be drawn will be solely from the lottery applicants and will not include the automatic spots.

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



Setting up the initial condition
--------------------------------
Here is the code to set up the lottery hat data frame at the initial state. Print out some validation output just to make sure the initial state is set up correctly.

```r
applicants <- 2302
spots <- 270
runner <- seq(1, applicants)
n <- c(1491, 482, 207, 122)
tickets <- c(rep(4, n[4]), rep(3, n[3]), rep(2, n[2]), rep(1, n[1]))
dfHat0 <- data.frame(runner, tickets)
dfHat0$prob <- dfHat0$tickets/sum(dfHat0$tickets)
table(factor(dfHat0$tickets))
```

```
## 
##    1    2    3    4 
## 1491  482  207  122
```

```r
head(dfHat0)
```

```
##   runner tickets     prob
## 1      1       4 0.001122
## 2      2       4 0.001122
## 3      3       4 0.001122
## 4      4       4 0.001122
## 5      5       4 0.001122
## 6      6       4 0.001122
```

```r
tail(dfHat0)
```

```
##      runner tickets      prob
## 2297   2297       1 0.0002806
## 2298   2298       1 0.0002806
## 2299   2299       1 0.0002806
## 2300   2300       1 0.0002806
## 2301   2301       1 0.0002806
## 2302   2302       1 0.0002806
```



Simulation
----------
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
## Time difference of 2.869 mins
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
## [1] 782
## 
## $runner
##   [1]    2    5    7   11   14   18   25   30   31   32   33   39   40   42
##  [15]   48   49   53   63   78   80   84   86   89   97  100  101  107  109
##  [29]  114  115  116  117  120  123  127  133  135  144  147  152  153  157
##  [43]  164  167  168  173  178  186  192  193  196  202  212  217  219  220
##  [57]  223  226  230  231  247  248  249  257  262  265  266  269  272  277
##  [71]  280  289  290  291  293  294  297  303  308  319  325  336  340  343
##  [85]  356  358  372  380  381  387  389  406  417  422  424  433  434  469
##  [99]  471  480  492  493  510  517  518  523  533  542  548  552  553  554
## [113]  580  588  599  610  616  623  661  668  685  688  690  696  705  709
## [127]  711  720  728  731  739  747  755  757  763  765  766  796  801  804
## [141]  832  846  850  862  870  874  878  885  899  901  907  968  973  979
## [155]  983  992  997 1000 1021 1037 1059 1061 1074 1090 1106 1111 1112 1122
## [169] 1149 1154 1159 1164 1170 1204 1230 1251 1252 1262 1270 1274 1311 1313
## [183] 1317 1323 1330 1337 1349 1400 1405 1419 1440 1447 1455 1457 1459 1478
## [197] 1480 1484 1493 1499 1515 1525 1532 1546 1553 1554 1577 1580 1584 1592
## [211] 1593 1600 1603 1630 1681 1693 1695 1696 1699 1718 1730 1751 1770 1794
## [225] 1807 1849 1855 1861 1862 1863 1868 1883 1887 1892 1896 1915 1929 1936
## [239] 1940 1945 1969 1974 1977 1978 1981 1985 1995 1999 2005 2058 2072 2083
## [253] 2115 2125 2134 2137 2140 2156 2162 2175 2196 2198 2203 2213 2224 2263
## [267] 2270 2272 2275 2286
```



Summary
-------
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

For each type of lottery applicant (1 ticket, 2 tickets, etc.), calculate the proportion of selected applicants. Print a summary table of the mean and median selection probabilities and their standard deviations.

```r
total <- rep(n, size)
dfSummary$prob <- 100 * (dfSummary$freq/total)
avg <- aggregate(prob ~ tickets, dfSummary, mean)
med <- aggregate(prob ~ tickets, dfSummary, median)
sd <- aggregate(prob ~ tickets, dfSummary, sd)
ev <- n * avg[, 2]/100
probWSER <- c(7.6, 14.6, 21.1, 27.1)
evWSER <- n * probWSER/100
diffprob <- avg[, 2] - probWSER
diffev <- ev - evWSER
simsum <- data.frame(avg, med[, 2], sd[, 2], n, ev, probWSER, evWSER, diffprob, 
    diffev)
names(simsum) <- c("Tickets", "Mean", "Median", "SD", "N", "Expected", "Prob (WSER)", 
    "Expected (WSER)", "Difference, prob.", "Difference, expected")
print(xtable(simsum), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 2.15.2 by xtable 1.7-0 package -->
<!-- Wed Dec 05 11:04:06 2012 -->
<TABLE border=1>
<TR> <TH> Tickets </TH> <TH> Mean </TH> <TH> Median </TH> <TH> SD </TH> <TH> N </TH> <TH> Expected </TH> <TH> Prob (WSER) </TH> <TH> Expected (WSER) </TH> <TH> Difference, prob. </TH> <TH> Difference, expected </TH>  </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 7.90 </TD> <TD align="right"> 7.91 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 1491.00 </TD> <TD align="right"> 117.74 </TD> <TD align="right"> 7.60 </TD> <TD align="right"> 113.32 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 4.42 </TD> </TR>
  <TR> <TD> 2 </TD> <TD align="right"> 15.14 </TD> <TD align="right"> 15.15 </TD> <TD align="right"> 1.40 </TD> <TD align="right"> 482.00 </TD> <TD align="right"> 72.99 </TD> <TD align="right"> 14.60 </TD> <TD align="right"> 70.37 </TD> <TD align="right"> 0.54 </TD> <TD align="right"> 2.61 </TD> </TR>
  <TR> <TD> 3 </TD> <TD align="right"> 21.77 </TD> <TD align="right"> 21.74 </TD> <TD align="right"> 2.53 </TD> <TD align="right"> 207.00 </TD> <TD align="right"> 45.05 </TD> <TD align="right"> 21.10 </TD> <TD align="right"> 43.68 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 1.38 </TD> </TR>
  <TR> <TD> 4 </TD> <TD align="right"> 28.05 </TD> <TD align="right"> 27.87 </TD> <TD align="right"> 3.96 </TD> <TD align="right"> 122.00 </TD> <TD align="right"> 34.22 </TD> <TD align="right"> 27.10 </TD> <TD align="right"> 33.06 </TD> <TD align="right"> 0.95 </TD> <TD align="right"> 1.16 </TD> </TR>
   </TABLE>

Curiously, the probabilities given in the [WSER website](http://www.wser.org/2012/12/03/dec-8-lottery-details) would yield only `260.427` lottery winners, while my estimates would yield `270` lottery winners, the target number. **Either something might be incorrect about my thinking about the process of the lottery, or something might be incorrect with the calculation of the probability estimates by WSER.**

Plot the distribution of probabilities from the `1000` simulated lotteries. Annotate with the estimated mean selection probability.

```r
y1 <- max(density(dfSummary$prob[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$prob[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$prob[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$prob[dfSummary$tickets == 4])$y)
y <- c(y1, y2, y3, y4)
ggplot(dfSummary, aes(x = prob, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = "2012 WSER Lottery Selection Probability Densities", 
    x = "Percent", y = paste("Proportion of", size, "simulations"), fill = "Tickets") + 
    annotate("text", label = paste(format(simsum$Mean, digits = 2, trim = TRUE), 
        "%", sep = ""), x = simsum$Mean, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotProbabilities](figure/PlotProbabilities.png) 

As expected, the spread of the selection probabilities increases as the number of tickets a person has in the hat increases (the variance of a binomial random variable increases with $p$). Also, the selection probabilities I estimated from simulation are **uniformly slightly higher** than the probabilities on the [WSER website](http://www.wser.org/2012/12/03/dec-8-lottery-details). However, although the WSER estimates are **within 1 SD** of my estimates, the consequence of the apparently systematic bias is the **`9.573` more runners selected** as shown in the table.

Another way to think about the lottery is to plot the distribution of the frequency of runners selected by number of tickets. Annotate with the estimated expected value.

```r
y1 <- max(density(dfSummary$freq[dfSummary$tickets == 1])$y)
y2 <- max(density(dfSummary$freq[dfSummary$tickets == 2])$y)
y3 <- max(density(dfSummary$freq[dfSummary$tickets == 3])$y)
y4 <- max(density(dfSummary$freq[dfSummary$tickets == 4])$y)
y <- c(y1, y2, y3, y4)
ggplot(dfSummary, aes(x = freq, y = ..density.., fill = tickets)) + geom_density(alpha = 1/2, 
    color = NA) + scale_fill_brewer(type = "div", palette = "BrBG") + labs(title = "2012 WSER Lottery Selection Frequency Densities", 
    x = "Number", y = paste("Proportion of", size, "simulations"), fill = "Tickets") + 
    annotate("text", label = format(simsum$Expected, digits = 3, trim = TRUE), 
        x = simsum$Expected, y = y) + theme(legend.position = "bottom")
```

![plot of chunk PlotFrequencies](figure/PlotFrequencies.png) 


Plot the results of a random sample of the `1000` simulated lotteries.

```r
s <- 100
i <- sample(seq(1, size), s)
dfSample <- dfLottery[dfLottery$sim %in% i, ]
dfSample$sim <- factor(dfSample$sim)
ggplot(dfSample, aes(x = sim, fill = tickets)) + geom_bar(width = 1) + scale_fill_brewer(type = "div", 
    palette = "BrBG") + labs(title = paste("Simulated 2012 WSER Lotteries\n", 
    "Sample of", s, "Lotteries"), x = "Simulated lottery", y = "Number of selected runners", 
    fill = "Tickets") + coord_flip() + theme(legend.position = "top")
```

![plot of chunk PlotLotteryResults](figure/PlotLotteryResults.png) 

