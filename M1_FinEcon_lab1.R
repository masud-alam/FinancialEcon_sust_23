### FinEcon_Lab One

## Financial Economics, SUST, Sylhet, Bangladesh, Spring 2023

## Importing stock price data

## Step I:  Required packages----

#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
#install.packages("xts")
#install.packages("ggplot2")


#Step II: Calling packages on your working environment------

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)

##Importing historical data from yahoo finance
options(scipen = 999)



#Method-I

dt <- "2015-3-1"
aapl <- getSymbols.yahoo("AAPL",from = dt, auto.assign=F)

aapl.adj <- getSymbols.yahoo("AAPL",from = dt, auto.assign=F)[,6]

#Calculating log returns

aaplReturn <- dailyReturn(aapl.adj, type = "log")


chartSeries(aapl)



##Importing more than one stock price series

#Method-I

getSymbols(c("^GSPC","AAPL","INTC","MSFT","META"),
           src = "yahoo",from=as.Date("2015-03-01"),
           to=as.Date("2023-03-07"),periodicity="daily")


### Other public sources to get free historical data

##google
##Oanda
##investing.com
# Fama-French factors data (https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
#FRED, Federal Reserve Economic Data, https://fred.stluoisfed.org
#Real Estate Investment Trust Data (https://www.reit.com/data-research)

## Proprietary data

#Bloomberg, Capital IQ, Kitco: Precious Metals, and Thompson Reuters
#Center for Research in Security Prices, LLC (CRSP)
#Wharton Research Data Services (https://wrds.wharton.upenn.edu/)

##Global data vendors: https://wrds-www.wharton.upenn.edu/pages/about/data-vendors/


### Method III-----

ENV.stock <- new.env()

getSymbols(c("^GSPC","AAPL","INTC","MSFT","META"),
           src = "yahoo",from=as.Date("2015-03-01"),
           to=as.Date("2023-03-07"),periodicity="daily",
           env = ENV.stock)
ENV.stock$GSPC
ENV.stock$AAPL$AAPL.Adjusted
Cl(ENV.stock$AAPL)
Ad(ENV.stock$AAPL)


plot(Ad(ENV.stock$AAPL), col = "blue")


## Produce/create a single xts data frame for the adjusted prices for all the series
stock.adj <- do.call(merge,eapply(ENV.stock,Ad))

plot(stock.adj)

#### Calculating daily log returns 

xts.log.ret <- diff(log(stock.adj),lag = 1)
xts.log.ret <- na.omit(xts.log.ret)

###Annualized returns


vec.mean.ret <- sapply(xts.log.ret, FUN = mean)

vec.mean.ret


vec.annual.ret <- exp(vec.mean.ret*252)-1
vec.annual.ret


#### Calculating daily volatility

vec.daily.vol <- sapply(xts.log.ret, FUN = sd)
vec.annual.vol <- vec.daily.vol*sqrt(252)
vec.annual.vol
vec.annual.var <- vec.annual.vol**2

hist(xts.log.ret$GSPC.Adjusted, breaks = 45, col = c("blue","red","green"))


plot( vec.annual.vol, vec.annual.ret)

vec.names <- names(vec.annual.ret)


ggplot(data = data.frame(vec.annual.ret,vec.annual.vol),aes(x=vec.annual.vol,y=vec.annual.ret))+
  geom_point()+
  geom_text(aes(label=vec.names),vjust=2, size=2)


##Portfolio returns and risk------


# Portfolio weight


vec.weight <- c(rep(0.2,each=5))

vec.weight
sum(vec.weight)


## Portfolio return

var.portfol.ret <- crossprod(vec.weight,vec.annual.ret)
var.portfol.ret


var.portfol.ret1 <- t(vec.weight)%*%vec.annual.ret
var.portfol.ret1

## Variance and covariance matrix

mat.vcov <- cov(xts.log.ret)##Daily
mat.vcov

mat.vcovAn <- cov(xts.log.ret)*252 ## Annual
mat.vcovAn

## Portfolio risk

var.portfol.var <- vec.weight%*%mat.vcov%*%matrix(t(vec.weight))
var.portfol.var
portfolio.risk <- sqrt(var.portfol.var)
portfolio.risk



### Efficient frontier for two risky assets#####

mu_a <- 14
mu_b <- 8

sigma_a <-6
sigma_b <- 3



rho <- -0.6


w <- seq(from=0,to=1,by=0.1)

ret <- w*mu_a+(1-w)*mu_b

vol <- sqrt(w^2*sigma_a^2+(1-w)^2*sigma_b^2+2*w*(1-w)*rho*sigma_a*sigma_b)

library(data.table)

df <- data.table(ret=ret,vol=vol)
ggplot(df,aes(x=vol,y=ret))+
  geom_point()+
  geom_text(data=df, aes(x=vol, y=ret+0.09),label=w*100)+
  theme_bw()+
  xlab("Risk")+ylab("Return")
  #annotate("point",x=2.9, y=10.4, colour="blue", size=2)





### Efficient frontier for two risky assets with short sales#####

mu_a <- 14
mu_b <- 8

sigma_a <-6
sigma_b <- 3



rho <- -0.5


ws <- seq(from=-1,to=2,by=0.1)

ret <- ws*mu_a+(1-ws)*mu_b

vol <- sqrt(ws^2*sigma_a^2+(1-ws)^2*sigma_b^2+2*ws*(1-ws)*rho*sigma_a*sigma_b)

library(data.table)

df <- data.table(ret=ret,vol=vol)
g1 <- ggplot(df,aes(x=vol,y=ret))+
  geom_point()+
  geom_text(data=df, aes(x=vol, y=ret+0.03),label=ws*100)+
  theme_bw()+
  xlab("Risk")+ylab("Return")
#annotate("point",x=2.9, y=10.4, colour="blue", size=2)

###Identifying the segment for short sales

g2 <- g1+geom_segment(x=5.95,y=14.2,xend=13.75,yend=20.55, col="red")
g2

g3 <- g2+geom_segment(x=1.95,y=11.2,xend=5.85,yend=14.65, col="green", size=2.5, linetype="dashed")


g3



g4 <- g3+geom_label(label="Short Sales Zone:Borrowing", x=8.45,y=19.2,
                    label.padding = unit(0.15,"lines"),label.size = 0.05, color="red", nudge_x = 0.75)
g4
