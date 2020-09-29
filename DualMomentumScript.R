## Code to download batch data of several ticket symbols.  To be used for 
## dual momentum anaylsis

library(BatchGetSymbols)
library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DataCombine)
library(scales)

#Folder
folder = "C:\\Users\\Steph\\Documents\\R\\Finance"

# Date range
first.date <- as.Date("2007-04-01")
last.date <- Sys.Date()
freq.data <- 'daily'

# Tickers
tickers <- c('SPY','VEU','VBMFX')

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(folder,'Dual_Cache'))

TimeWindow = 251
refDate = l.out$df.tickers$ref.date
tickers = l.out$df.tickers$ticker
price.adjusted = l.out$df.tickers$price.adjusted
Data = data.frame(refDate,tickers,price.adjusted)
Out <- change(Data, Var = 'price.adjusted', GroupVar = 'tickers',
              type = 'percent', NewVar = 'PercentChange',slideBy = -TimeWindow)
Out <- Out %>% group_by(tickers) %>% mutate(id = row_number())

# Prep for comparing tickers over certain dates
SPY <- Out[ which(Out$tickers == "SPY"), ]
VEU <- Out[ which(Out$tickers == "VEU"), ]
VBMFX <- Out[ which(Out$tickers == "VBMFX"), ]
TickerSymbols = c("SPY","VEU","VBMFX")

# Look at buying every 21 samples (first of the month), every 10 samples (bi-monthly), 
# every 5 samples (weekly), and only on changes in momentum 
# Save purchase price, sale price and calculate percent gain

#Setup data frames for each time segment
MonthlySale = data.frame(ticker = character(),
                         bought = double(),
                         sold = double(),
                         date = as.Date(character()),
                         period.return = double(),
                         total.return = double(),
                         switch = logical())
BiweeklySale = MonthlySale
WeeklySale = MonthlySale
DailySale = MonthlySale

#Initialize incrementing and looping variables
invest = 10000
loopStart = TimeWindow + 1

#Monthly calculation
Period.Length = 21
loopEnd = 1+round((max(Out$id)-loopStart)/Period.Length,0)
for(i in 1:loopEnd){
  buy = which.max(c(SPY$PercentChange[ which(SPY$id==(Period.Length*(i-1)+loopStart))],
             VEU$PercentChange[ which(VEU$id==(Period.Length*(i-1)+loopStart))],
             VBMFX$PercentChange[ which(VBMFX$id==(Period.Length*(i-1)+loopStart))]))
  buy = TickerSymbols[buy]   
  price = Out$price.adjusted[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]  
  purchase.date = Out$refDate[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]
    if (i == 1) {
    sell = NA
    period.return = 0
    total.return = invest
    switch = NA
  } else {
    sell = Out$price.adjusted[ which(as.character(Out$tickers)==MonthlySale$buy[i-1]&Out$id==(Period.Length*(i-1)+loopStart))]
    period.return = (sell-MonthlySale$price[i-1])/MonthlySale$price[i-1]
    total.return = MonthlySale$total.return[i-1] + MonthlySale$total.return[i-1] * period.return
    if (buy == MonthlySale$buy[i-1]) {
      switch = FALSE
    } else {
      switch = TRUE
    }
  }
  entry = data.frame(buy,price,sell,purchase.date,period.return,total.return,switch)
  MonthlySale = rbind(MonthlySale,entry)
}

# Create Data Frame with each of the points the sale of stocks occurred
switch.date = as.Date(as.character(MonthlySale$purchase.date[ which(MonthlySale$switch==TRUE)]))
switch.total.return = MonthlySale$total.return[ which(MonthlySale$switch==TRUE)]
switch.ticker = MonthlySale$buy[ which(MonthlySale$switch==TRUE)]
monthly.switchpts = cbind.data.frame(switch.date,switch.total.return,switch.ticker)
MonthlySale$type = "Monthly"
Monthly = merge(MonthlySale,monthly.switchpts,by.x = "purchase.date",by.y = "switch.date", all = TRUE)
rm(monthly.switchpts,MonthlySale)

#Biweekly calculation
Period.Length = 10
loopEnd = 1+round((max(Out$id)-loopStart)/Period.Length,0)
for(i in 1:loopEnd){
  buy = which.max(c(SPY$PercentChange[ which(SPY$id==(Period.Length*(i-1)+loopStart))],
                    VEU$PercentChange[ which(VEU$id==(Period.Length*(i-1)+loopStart))],
                    VBMFX$PercentChange[ which(VBMFX$id==(Period.Length*(i-1)+loopStart))]))
  buy = TickerSymbols[buy]   
  price = Out$price.adjusted[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]  
  purchase.date = Out$refDate[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]
  if (i == 1) {
    sell = NA
    period.return = 0
    total.return = invest
    switch = NA
  } else {
    sell = Out$price.adjusted[ which(as.character(Out$tickers)==BiweeklySale$buy[i-1]&Out$id==(Period.Length*(i-1)+loopStart))]
    period.return = (sell-BiweeklySale$price[i-1])/BiweeklySale$price[i-1]
    total.return = BiweeklySale$total.return[i-1] + BiweeklySale$total.return[i-1] * period.return
    if (buy == BiweeklySale$buy[i-1]) {
      switch = FALSE
    } else {
      switch = TRUE
    }
  }
  entry = data.frame(buy,price,sell,purchase.date,period.return,total.return,switch)
  BiweeklySale = rbind(BiweeklySale,entry)
}

# Create Data Frame with each of the points the sale of stocks occurred
switch.date = as.Date(as.character(BiweeklySale$purchase.date[ which(BiweeklySale$switch==TRUE)]))
switch.total.return = BiweeklySale$total.return[ which(BiweeklySale$switch==TRUE)]
switch.ticker = BiweeklySale$buy[ which(BiweeklySale$switch==TRUE)]
Biweekly.switchpts = cbind.data.frame(switch.date,switch.total.return,switch.ticker)
BiweeklySale$type = "Biweekly"
Biweekly = merge(BiweeklySale,Biweekly.switchpts,by.x = "purchase.date",by.y = "switch.date", all = TRUE)
rm(Biweekly.switchpts,BiweeklySale)

#Weekly calculation
Period.Length = 5
loopEnd = 1+round((max(Out$id)-loopStart)/Period.Length,0)
for(i in 1:loopEnd){
  buy = which.max(c(SPY$PercentChange[ which(SPY$id==(Period.Length*(i-1)+loopStart))],
                    VEU$PercentChange[ which(VEU$id==(Period.Length*(i-1)+loopStart))],
                    VBMFX$PercentChange[ which(VBMFX$id==(Period.Length*(i-1)+loopStart))]))
  buy = TickerSymbols[buy]   
  price = Out$price.adjusted[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]  
  purchase.date = Out$refDate[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]
  if (i == 1) {
    sell = NA
    period.return = 0
    total.return = invest
    switch = NA
  } else {
    sell = Out$price.adjusted[ which(as.character(Out$tickers)==WeeklySale$buy[i-1]&Out$id==(Period.Length*(i-1)+loopStart))]
    period.return = (sell-WeeklySale$price[i-1])/WeeklySale$price[i-1]
    total.return = WeeklySale$total.return[i-1] + WeeklySale$total.return[i-1] * period.return
    if (buy == WeeklySale$buy[i-1]) {
      switch = FALSE
    } else {
      switch = TRUE
    }
  }
  entry = data.frame(buy,price,sell,purchase.date,period.return,total.return,switch)
  WeeklySale = rbind(WeeklySale,entry)
}

# Create Data Frame with each of the points the sale of stocks occurred
switch.date = as.Date(as.character(WeeklySale$purchase.date[ which(WeeklySale$switch==TRUE)]))
switch.total.return = WeeklySale$total.return[ which(WeeklySale$switch==TRUE)]
switch.ticker = WeeklySale$buy[ which(WeeklySale$switch==TRUE)]
Weekly.switchpts = cbind.data.frame(switch.date,switch.total.return,switch.ticker)
WeeklySale$type = "Weekly"
Weekly = merge(WeeklySale,Weekly.switchpts,by.x = "purchase.date",by.y = "switch.date", all = TRUE)
rm(Weekly.switchpts,WeeklySale)

#Daily calculation
Period.Length = 1
loopEnd = 1+round((max(Out$id)-loopStart)/Period.Length,0)
for(i in 1:loopEnd){
  buy = which.max(c(SPY$PercentChange[ which(SPY$id==(Period.Length*(i-1)+loopStart))],
                    VEU$PercentChange[ which(VEU$id==(Period.Length*(i-1)+loopStart))],
                    VBMFX$PercentChange[ which(VBMFX$id==(Period.Length*(i-1)+loopStart))]))
  buy = TickerSymbols[buy]   
  price = Out$price.adjusted[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]  
  purchase.date = Out$refDate[ which(Out$tickers==buy&Out$id==(Period.Length*(i-1)+loopStart))]
  if (i == 1) {
    sell = NA
    period.return = 0
    total.return = invest
    switch = NA
  } else {
    sell = Out$price.adjusted[ which(as.character(Out$tickers)==DailySale$buy[i-1]&Out$id==(Period.Length*(i-1)+loopStart))]
    period.return = (sell-DailySale$price[i-1])/DailySale$price[i-1]
    total.return = DailySale$total.return[i-1] + DailySale$total.return[i-1] * period.return
    if (buy == DailySale$buy[i-1]) {
      switch = FALSE
    } else {
      switch = TRUE
    }
  }
  entry = data.frame(buy,price,sell,purchase.date,period.return,total.return,switch)
  DailySale = rbind(DailySale,entry)
}

# Create Data Frame with each of the points the sale of stocks occurred
switch.date = as.Date(as.character(DailySale$purchase.date[ which(DailySale$switch==TRUE)]))
switch.total.return = DailySale$total.return[ which(DailySale$switch==TRUE)]
switch.ticker = DailySale$buy[ which(DailySale$switch==TRUE)]
Daily.switchpts = cbind.data.frame(switch.date,switch.total.return,switch.ticker)
DailySale$type = "Daily"
Daily = merge(DailySale,Daily.switchpts,by.x = "purchase.date",by.y = "switch.date", all = TRUE)
rm(Daily.switchpts,DailySale)

TotalData = rbind(Monthly,Biweekly,Weekly,Daily)

t <- ggplot(TotalData,aes(x = purchase.date, y = total.return, col = type)) + geom_line()
t <- t + geom_point(data=TotalData, aes(x=purchase.date,
                                                y=switch.total.return,
                                                colour = switch.ticker))
t + theme_light()
plot(t)
# 
# m <- ggplot(MonthlySale,aes(x = purchase.date, y = total.return)) + geom_line(col = "black")
# m <- m + geom_point(data=monthly.switchpts, aes(x=monthly.switchpts$switch.date,
#                                                 y=monthly.switchpts$switch.total.return,
#                                                 colour = monthly.switchpts$switch.ticker))
# 
# m <- m + labs(title = "Total Return Trading Monthly", x = ("Trade Date"), y = ("Total Return"))
# m <- m + theme_light()
# plot(m)
# 
# 
# b <- ggplot(BiweeklySale,aes(x = purchase.date, y = total.return)) + geom_line(col = "black")
# b <- b + geom_point(data=Biweekly.switchpts, aes(x=Biweekly.switchpts$switch.date,
#                                                 y=Biweekly.switchpts$switch.total.return,
#                                                 colour = Biweekly.switchpts$switch.ticker))
# b <- b + labs(title = "Total Return Trading Biweekly", x = ("Trade Date"), y = ("Total Return"))
# b <- b + theme_light()
# plot(b)
# 
# w <- ggplot(WeeklySale,aes(x = purchase.date, y = total.return)) + geom_line(col = "black")
# w <- w + geom_point(data=Weekly.switchpts, aes(x=Weekly.switchpts$switch.date,
#                                                  y=Weekly.switchpts$switch.total.return,
#                                                  colour = Weekly.switchpts$switch.ticker))
# w <- w + labs(title = "Total Return Trading Weekly", x = ("Trade Date"), y = ("Total Return"))
# w <- w + theme_light()
# plot(w)
# 
# d <- ggplot(DailySale,aes(x = purchase.date, y = total.return)) + geom_line(col = "black")
# d <- d + geom_point(data=Daily.switchpts, aes(x=Daily.switchpts$switch.date,
#                                                y=Daily.switchpts$switch.total.return,
#                                                colour = Daily.switchpts$switch.ticker))
# d <- d + labs(title = "Total Return Trading Daily", x = ("Trade Date"), y = ("Total Return"))
# d <- d + theme_light()
# plot(d)

#p <- ggplot(Out,aes(x = refDate, y = PercentChange, col = tickers)) + geom_line()
#p <- p + labs(title="1 year return",x=("Date"),y=("% Return"))
#p <- p + scale_x_date(date_breaks = "1 month", 
#                      labels=date_format("%b-%Y"),
#                      limits = as.Date(c('2018-01-16','2019-01-16')))
#p <- p + scale_y_continuous(limits = c(-20,60))
#p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#print(p)