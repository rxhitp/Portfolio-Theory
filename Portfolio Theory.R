##Introduction to Portfolio Theory###
library(quantmod)
library(PerformanceAnalytics)


#######################################
##Computing returns given PRICE data###
#######################################

aapl=getSymbols("AAPL",auto.assign=F)
aapl.cl=Cl(aapl)
aapl.return=Return.calculate(aapl.cl)
aapl.logreturn=Return.calculate(aapl.cl,method ='log')

head(aapl.return)
head(aapl.logreturn)

aapl.return=aapl.return[-1,]
aapl.logreturn=aapl.return[-1,]

##Get data for another company

ibm=getSymbols("IBM",auto.assign=F)
ibm.cl=Cl(ibm)
ibm.return=Return.calculate(ibm.cl)

#Join aapl and ibm data
aapl_ibm=merge(aapl.cl,ibm.cl,fill=NA,join='outer')

#outer: all rows
# inner: common rows
# left: all rows of left object
# right: all rows of right object

head(aapl_ibm)
aapl_ibm_ret=Return.calculate(aapl_ibm)
head(aapl_ibm_ret)
aapl_ibm_ret=aapl_ibm_ret[-1,]

####Computing Portfolio Returns###
args(Return.portfolio)

# In verbose mode, the function returns a list of intermediary calculations
# that users may find helpful, including both asset contribution
# and asset value through time. 

port_ret=Return.portfolio(aapl_ibm_ret)
head(port_ret)

#If the user does not specify weights, an equal weight portfolio is assumed.
head(Return.portfolio(aapl_ibm_ret,weights=c(0.6,0.4)))

plot.zoo(port_ret)

plot.zoo(cumprod(c(1,1+port_ret)))


port_monthly_ret=Return.portfolio(aapl_ibm_ret,rebalance_on="months")

head(port_monthly_ret)


###################################
###Converting to monthly values####
###################################

apple_monthly=to.monthly(aapl)
head(apple_monthly)

head(Return.calculate(apple_monthly))
plot.zoo(Return.calculate(apple_monthly)) #This gives 3 rows and 2 columns
plot.zoo(Cl(Return.calculate(apple_monthly))) #This gives only the closing returns chart

###############################################
#####Obtaining annualized returns and risk ####
###############################################

StdDev.annualized(aapl.return)

StdDev.annualized(aapl.return,scale=240) #Figures are slightly different

Return.annualized(aapl.return)

SharpeRatio.annualized(aapl.return)

#Both these functions belong to Performance Analytics package
table.AnnualizedReturns(aapl.return) #assumes rf to be 0

#Rf is daily Rf. Hence it is OK to assume 0 Rf
table.AnnualizedReturns(aapl.return,Rf=0.05/250)

######################
###Creating Charts ###
######################

chart.Histogram(aapl.return,methods=c('add.density','add.normal'))

#The methods argument is not default. 

#Non-normality of returns
skewness(aapl.return)

kurtosis(aapl.return)

SemiDeviation(aapl.return)

shapiro.test(coredata(aapl.return))

#H0 is data normally distributed. 
#Low 'p' means data not normally distributed. 

VaR(aapl.return,p=0.025)
#VaR refers to value at risk

#ES: expected shortfall
ES(aapl.return,p=0.025) #5% ES s the average of the 5% of the 
#most negative returns


chart.RollingCorrelation(aapl.return,ibm.return,width=24)
chart.Correlation(aapl_ibm_ret) #This uses one argument. So use merge first.


##Cluster Analysis on Stock Returns
library(tidyverse)
#library(lubridate)

returns=read_csv("sensex returns.csv")

names(returns)
summary(returns)
anyNA(returns)

# returns1=na.omit(returns)
returns.use = returns[,-1]

##Dendogram

returns.dist = dist(returns.use)
returns.hclust = hclust(returns.dist)
returns.hclust$merge

plot(returns.hclust,labels=returns$coname,main='Default from hclust')

#Creating Optimum Portfolios
library(tseries)
head(returns)
class(returns)

returns_t=t(returns[,-1])
head(returns_t)
class(returns_t)
colnames(returns_t)=returns$coname
head(returns_t)

opt=portfolio.optim(returns_t)
opt

# Backtesting
head(returns_t) #From Jan 13
tail(returns_t) #Till June 17
nrow(returns_t) #54

class(returns_t) #a Matrix

# Convert reurns_t into an xts object

row.names(returns_t)

paste("1-",row.names(returns_t),sep="")

row.names(returns_t)= paste("1-",row.names(returns_t),sep="")

head(returns_t)

returns_xts = xts(returns_t,order.by = dmy(row.names(returns_t)))
head(returns_xts)

returns_train = returns_xts[1:45,]
returns_test=returns_xts[46:54,]

pf_estim=portfolio.optim(returns_train)

predicted_test_returns = Return.portfolio(returns_test,pf_estim$pw)
actual_test_returns = Return.portfolio(returns_test)

table.AnnualizedReturns(predicted_test_returns)
table.AnnualizedReturns(actual_test_returns)

