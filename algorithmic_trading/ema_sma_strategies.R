#####################################################################

library(xts)
library(quantmod)
library(tseries) 
library(chron)
library(TTR)
library(caTools)
library(lubridate)
library(dplyr)
library(lattice)
library(grDevices)

Sys.setlocale("LC_TIME", "C")
setwd("...")

mySR <- function(x, # x = series of returns
                 scale) # scaling parameter = Nt
{
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} # end of definition

load("currencies.RData")

currencies_train <- currencies[day(currencies) < 17,]
currencies_test <- currencies[day(currencies) >= 17,]

source("function_positionR.R")

#######################################################################
# Exercises 2

# Exercise 2.1
# Check the performance of the same strategy on the test data.

# We just need to change the dataset to currencies_test$E6 and the rest is the same
E6 <- currencies_test$E6
E6["T18:01/T18:05"] <- NA
E6["T16:56/T17:00"] <- NA
pos_flat <- xts(rep(0, nrow(E6)), index(E6))
pos_flat["T16:46/T18:15"] <- 1
dweek_ <- wday(E6)
time_ <- substr(index(E6), 12, 19)
pos_flat[(dweek_ == 6 & times(time_) > times("17:00:00")) |   # end of Friday
           (dweek_ == 7) |                                      # whole Saturday
           (dweek_ == 1 & times(time_) <= times("18:00:00")),] <- 1 # beginning of Sunday
index_ <- index(E6)

for(signalEMA in c(10, 15, 20, 30, 45)) {
  for(slowEMA in c(60, 90, 120, 150, 180)) {
    for(volat.sd in c(60, 90, 120)) {
      for(m_ in c(1, 2, 3)) {
        message(paste0("signalEMA = ", signalEMA,
                       ", slowEMA = ", slowEMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        signalEMA.values <- EMA(na.locf(coredata(E6$E6)), 
                                signalEMA)
        slowEMA.values <- EMA(na.locf(coredata(E6$E6)), 
                              slowEMA)
        volat.sd.values <- runsd(na.locf(coredata(E6$E6)), 
                                 volat.sd, 
                                 endrule = "NA", 
                                 align = "right")
        pos.mom <- positionR(signal = signalEMA.values,
                             lower = slowEMA.values - m_ * volat.sd.values,
                             upper = slowEMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat),
                             strategy = "mom" # important !!!
        )
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(E6$E6)),
                                0, pos.mom * diff.xts(E6$E6) * 125000 # point value for E6
        )
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of E6
        ends_ <- endpoints(E6, "days")
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        summary_ <- data.frame(signalEMA = signalEMA,
                               slowEMA = slowEMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = "2013-08",
                               gross.SR.mom,
                               net.SR.mom,
                               gross.PnL.mom,
                               net.PnL.mom,
                               stringsAsFactors = FALSE
        )
        if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
          summary.all.breakout <- rbind(summary.all.breakout, summary_)
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_,
           signalEMA.values, slowEMA.values, volat.sd.values)}}}}

# Are 5 top performing combinations still profitable?
# No. Generally if the net.SR.mom is above 0, we can say that the strategy was profitable.
# However here the top strategy yields a negative net.SR.mom value. Although the net.SR.mom
# is positive but by incorporating the transaction costs we encounter a negative net.PnL.mom
# which means we lose $682.5 in net term.
summary.all.breakout %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(10)

# How test net SR differs from train net SR?
# In the training dataset the net SR was positive but here in test set it's negative

# Use plots to verify sensitivity of results to parameters.
# net.SR.mom
levelplot(net.SR.mom ~ as.factor(m) * as.factor(volat.sd), # formula
          data = summary.all.breakout %>% 
            dplyr::filter(signalEMA == 10, 
                          slowEMA == 150),
          xlab = "volatility multiplier",
          ylab = "volatility memory",
          main = "net Sharpe ratio",
          col.regions = colorRampPalette(c("red", "white", "green")))

# net.PnL.mom
levelplot(net.PnL.mom ~ as.factor(m) * as.factor(volat.sd), # formula
          data = summary.all.breakout %>% 
            dplyr::filter(signalEMA == 10, 
                          slowEMA == 150),
          xlab = "volatility multiplier",
          ylab = "volatility memory",
          main = "net Profit and Loss",
          col.regions = colorRampPalette(c("red", "white", "green")))


# **EXTRA**
# The poor momentum strategy on the test dataset might be connected 
# with the change of the pattern in next two weeks of August, which differs from
# the first two weeks that we had for the training dataset:
plot(currencies$E6,type = 'l',main = "Weighted bid-and-ask average of futures for EUR/USD")

# Let's change the strategy to "Mean Reverting"
for(signalEMA in c(10, 15, 20, 30, 45)) {
  for(slowEMA in c(60, 90, 120, 150, 180)) {
    for(volat.sd in c(60, 90, 120)) {
      for(m_ in c(1, 2, 3)) {
        message(paste0("signalEMA = ", signalEMA,
                       ", slowEMA = ", slowEMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        signalEMA.values <- EMA(na.locf(coredata(E6$E6)), 
                                signalEMA)
        slowEMA.values <- EMA(na.locf(coredata(E6$E6)), 
                              slowEMA)
        volat.sd.values <- runsd(na.locf(coredata(E6$E6)), 
                                 volat.sd, 
                                 endrule = "NA", 
                                 align = "right")
        pos.mom <- positionR(signal = signalEMA.values,
                             lower = slowEMA.values - m_ * volat.sd.values,
                             upper = slowEMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat),
                             strategy = "mr" # important !!!
        )
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(E6$E6)),
                                0, pos.mom * diff.xts(E6$E6) * 125000 # point value for E6
        )
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of E6
        ends_ <- endpoints(E6, "days")
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        summary_ <- data.frame(signalEMA = signalEMA,
                               slowEMA = slowEMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = "2013-08",
                               gross.SR.mom,
                               net.SR.mom,
                               gross.PnL.mom,
                               net.PnL.mom,
                               stringsAsFactors = FALSE
        )
        if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
          summary.all.breakout <- rbind(summary.all.breakout, summary_)
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_,
           signalEMA.values, slowEMA.values, volat.sd.values)}}}}

# And check the summary again
summary.all.breakout %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(10)

# Best net.SR.mom yields 9.572857 ration and therefore a net profit of $3220 
rm(list = ls(all.names = TRUE)) 

# Exercise 2.2
# Perform similar analyses of a momentum strategy for another currency
# (other than E6) use volatility breakout model -- check different 
# SMAs and EMAs (function SMA() has the same syntax and requirements 
# as EMA()); use MORE combinations of parameters.
# Are the conclusions on train and test data similar as for E6?

mySR <- function(x, # x = series of returns
                 scale) # scaling parameter = Nt
{sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)}
load("currencies.RData")
currencies_train <- currencies[day(currencies) < 17,]
currencies_test <- currencies[day(currencies) >= 17,]
source("function_positionR.R")

# Let's use British Pound. First for the training set
B6 <- currencies_train$B6
B6["T18:01/T18:05"] <- NA
B6["T16:56/T17:00"] <- NA
pos_flat <- xts(rep(0, nrow(B6)), index(B6))
pos_flat["T16:46/T18:15"] <- 1
dweek_ <- wday(B6)
time_ <- substr(index(B6), 12, 19)
pos_flat[(dweek_ == 6 & times(time_) > times("17:00:00")) |   # end of Friday
           (dweek_ == 7) |                                      # whole Saturday
           (dweek_ == 1 & times(time_) <= times("18:00:00")),] <- 1 # beginning of Sunday
index_ <- index(B6)

for(signalSMA in c(60,120,180)) {
  for(slowSMA in c(80,100,130,240)) {
    for(volat.sd in c(30,50,120)) {
      for(m_ in c(1,3,6,8)) {
        message(paste0("signalSMA = ", signalSMA,
                       ", slowSMA = ", slowSMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        signalSMA.values <- SMA(na.locf(coredata(B6$B6)), 
                                signalSMA)
        slowSMA.values <- SMA(na.locf(coredata(B6$B6)), 
                              slowSMA)
        volat.sd.values <- runsd(na.locf(coredata(B6$B6)), 
                                 volat.sd, 
                                 endrule = "NA", 
                                 align = "right")
        pos.mom <- positionR(signal = signalSMA.values,
                             lower = slowSMA.values - m_ * volat.sd.values,
                             upper = slowSMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat),
                             strategy = "mom" # important !!!
        )
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(B6$B6)),
                                0, pos.mom * diff.xts(B6$B6) * 62500 # point value for B6
        )
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of B6
        ends_ <- endpoints(B6, "days")
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        summary_ <- data.frame(signalSMA = signalSMA,
                               slowSMA = slowSMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = "2013-08",
                               gross.SR.mom,
                               net.SR.mom,
                               gross.PnL.mom,
                               net.PnL.mom,
                               stringsAsFactors = FALSE
        )
        if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
          summary.all.breakout <- rbind(summary.all.breakout, summary_)
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_,
           signalSMA.values, slowSMA.values, volat.sd.values)}}}}

# Let's check the summary
summary.all.breakout %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(30)

# The results for British Pound using Simple Moving Average (SMA) looks promising as
# basically all the top 24 strategies yield positive Sharpe Ratio and therefore made a 
# profit (net.PnL.mom) from $45 all the way up to $1150

# Now let's check the result on the test data
options(scipen=999, digits = 2)
rm(summary.all.breakout)
B6 <- currencies_test$B6
B6["T18:01/T18:05"] <- NA
B6["T16:56/T17:00"] <- NA
pos_flat <- xts(rep(0, nrow(B6)), index(B6))
pos_flat["T16:46/T18:15"] <- 1
dweek_ <- wday(B6)
time_ <- substr(index(B6), 12, 19)
pos_flat[(dweek_ == 6 & times(time_) > times("17:00:00")) |   # end of Friday
           (dweek_ == 7) |                                      # whole Saturday
           (dweek_ == 1 & times(time_) <= times("18:00:00")),] <- 1 # beginning of Sunday
index_ <- index(B6)

for(signalSMA in c(60,120,180)) {
  for(slowSMA in c(80,100,130,240)) {
    for(volat.sd in c(30,50,120)) {
      for(m_ in c(1,3,6,8)) {
        message(paste0("signalSMA = ", signalSMA,
                       ", slowSMA = ", slowSMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        signalSMA.values <- SMA(na.locf(coredata(B6$B6)), 
                                signalSMA)
        slowSMA.values <- SMA(na.locf(coredata(B6$B6)), 
                              slowSMA)
        volat.sd.values <- runsd(na.locf(coredata(B6$B6)), 
                                 volat.sd, 
                                 endrule = "NA", 
                                 align = "right")
        pos.mom <- positionR(signal = signalSMA.values,
                             lower = slowSMA.values - m_ * volat.sd.values,
                             upper = slowSMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat),
                             strategy = "mom" # important !!!
        )
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(B6$B6)),
                                0, pos.mom * diff.xts(B6$B6) * 62500 # point value for B6
        )
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of B6
        ends_ <- endpoints(B6, "days")
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        summary_ <- data.frame(signalSMA = signalSMA,
                               slowSMA = slowSMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = "2013-08",
                               gross.SR.mom,
                               net.SR.mom,
                               gross.PnL.mom,
                               net.PnL.mom,
                               stringsAsFactors = FALSE
        )
        if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
          summary.all.breakout <- rbind(summary.all.breakout, summary_)
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_,
           signalSMA.values, slowSMA.values, volat.sd.values)}}}}

# Checking the summary for the test data
summary.all.breakout %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(30)

# Are the conclusions on train and test data similar as for E6?
# Although the net profit from the test set is much lower that the profit from the
# training set (81 vs 1150) and the net Sharpe Ratio decreases from 8.78 to 2.68 
# but still the strategy could make some profit. 


# Exercise 2.3 (*)
# Perform analyses of a MEAN-REVERTING strategy for the SPREAD
# between Canadian dollar C6 and Australian dollar A6 
# (take the simplest form: C6-A6).
# !!! Remember to use a correct option 'strategy = "mr"
# and apply pnl calculation of both sides of the spread.
# !!! Remember that transactional costs are ALWAYS positive
# irrespectively whether you take long or short position.

currencies_train$C6A6 <- currencies_train$C6 - currencies_train$A6
A6 <- currencies_train$A6
C6 <- currencies_train$C6

C6A6 <- currencies_train$C6A6

head(C6A6)
rm(summary.all.breakout)

C6A6["T18:01/T18:05"] <- NA
C6A6["T16:56/T17:00"] <- NA
pos_flat <- xts(rep(0, nrow(C6A6)), index(C6A6))
pos_flat["T16:46/T18:15"] <- 1
dweek_ <- wday(C6A6)
time_ <- substr(index(C6A6), 12, 19)
pos_flat[(dweek_ == 6 & times(time_) > times("17:00:00")) |   # end of Friday
           (dweek_ == 7) |                                      # whole Saturday
           (dweek_ == 1 & times(time_) <= times("18:00:00")),] <- 1 # beginning of Sunday
index_ <- index(C6A6)

# Using the simple moving average
for(signalSMA in c(60,120,180)) {
  for(slowSMA in c(80,100,130,240)) {
    for(volat.sd in c(30,50,120)) {
      for(m_ in c(1,3,6,8)) {
        message(paste0("signalSMA = ", signalSMA,
                       ", slowSMA = ", slowSMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        signalSMA.values <- SMA(na.locf(coredata(C6A6$C6A6)), 
                                signalSMA)
        slowSMA.values <- SMA(na.locf(coredata(C6A6$C6A6)), 
                              slowSMA)
        volat.sd.values <- runsd(na.locf(coredata(C6A6$C6A6)), 
                                 volat.sd, 
                                 endrule = "NA", 
                                 align = "right")
        pos.mom <- positionR(signal = signalSMA.values,
                             lower = slowSMA.values - m_ * volat.sd.values,
                             upper = slowSMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat),
                             strategy = "mr" # MEAN-REVERTING
        )
        
        # Calculating PnL for A6 side
        pnl.gross.mom.A6 <- ifelse(is.na(pos.mom * diff.xts(A6$A6)),
                                0, pos.mom * diff.xts(A6$A6) * 100000) # point value for A6

        
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom.A6 <- pnl.gross.mom.A6 - ntrans * 30 # 30$ per transaction
        ends_ <- endpoints(C6A6$C6A6, "days")
        pnl.gross.mom.A6.d <- period.apply(pnl.gross.mom.A6, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.A6.d <- period.apply(pnl.net.mom.A6, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.A6.mom <- mySR(pnl.gross.mom.A6.d, scale = 252)
        net.SR.A6.mom <- mySR(pnl.net.mom.A6.d, scale = 252)
        gross.PnL.A6.mom <- sum(pnl.gross.mom.A6.d, na.rm = T)
        net.PnL.A6.mom <- sum(pnl.net.mom.A6.d, na.rm = T)
        
        # Calculating PnL for C6 side
        pnl.gross.mom.C6 <- ifelse(is.na(pos.mom * diff.xts(C6$C6)),
                                   0, pos.mom * diff.xts(C6$C6) * 100000) # point value for C6
        
        
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        pnl.net.mom.C6 <- pnl.gross.mom.C6 - ntrans * 30 # 30$ per transaction
        ends_ <- endpoints(C6A6$C6A6, "days")
        pnl.gross.mom.C6.d <- period.apply(pnl.gross.mom.C6, 
                                           INDEX = ends_, 
                                           FUN = function(x) sum(x, na.rm = TRUE))
        pnl.net.mom.C6.d <- period.apply(pnl.net.mom.C6, 
                                         INDEX = ends_,
                                         FUN = function(x) sum(x, na.rm = TRUE))
        gross.SR.C6.mom <- mySR(pnl.gross.mom.C6.d, scale = 252)
        net.SR.C6.mom <- mySR(pnl.net.mom.C6.d, scale = 252)
        gross.PnL.C6.mom <- sum(pnl.gross.mom.C6.d, na.rm = T)
        net.PnL.C6.mom <- sum(pnl.net.mom.C6.d, na.rm = T)        

        summary_ <- data.frame(signalSMA = signalSMA,
                               slowSMA = slowSMA,
                               volat.sd = volat.sd,
                               m = m_,
                               gross.SR.A6.mom,
                               net.SR.A6.mom,
                               gross.PnL.A6.mom,
                               net.PnL.A6.mom,
                               gross.SR.C6.mom,
                               net.SR.C6.mom,
                               gross.PnL.C6.mom,
                               net.PnL.C6.mom,
                               stringsAsFactors = FALSE)
        
        if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
          summary.all.breakout <- rbind(summary.all.breakout, summary_)
        rm(gross.SR.A6.mom,
           net.SR.A6.mom,
           gross.PnL.A6.mom,
           net.PnL.A6.mom,
           gross.SR.C6.mom,
           net.SR.C6.mom,
           gross.PnL.C6.mom,
           net.PnL.C6.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_,
           signalSMA.values, slowSMA.values, volat.sd.values)}}}}

# pnl has been calculatec for both sides of the spread.
# Checking the summary based on best SR for A6
# It looks like signalSMA = 60 (1 hour), slowSMA = 130, volat.sd = 50 and multiplier = 6
# gave a good result for net.SR.A6.mom (5.8)
summary.all.breakout %>% 
  arrange(desc(net.SR.A6.mom)) %>% 
  head(5)

# And then based on best SR for C6
# On the other hand signalSMA = 60 (1 hour), slowSMA = 80, volat.sd = 30 and multiplier = 8
# yields a profit for C6 net.SR.A6.mom (5.7)
summary.all.breakout %>% 
  arrange(desc(net.SR.C6.mom)) %>% 
  head(5)

