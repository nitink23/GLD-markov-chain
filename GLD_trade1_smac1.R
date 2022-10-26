
#install.packages("tidyquant")
library(tidyverse)
library(tidyquant)
library(farver)





load(file="symbolsData.Rdata")

symbol0<-list_symbols[2]

symbolsData_symbol0<-symbolsData %>% filter(symbol==symbol0) 

# Define logReturn as forward return
symbolsData_symbol0$logReturn<- c(diff(log(symbolsData_symbol0$close)),0)

head(data.frame(symbolsData_symbol0))
symbolsData_symbol0$SMA50<- runMean(symbolsData_symbol0$close,n=50)

fcn.dot0<-function(x){
  x0<-ifelse(is.na(x)==TRUE,0,x)
  return(x0)
}

symbolsData_symbol0$SMA50<- fcn.dot0(runMean(symbolsData_symbol0$close,n=50))
symbolsData_symbol0$SMA100<-fcn.dot0(runMean(symbolsData_symbol0$close,n=100))


date0<-symbolsData_symbol0$date

y<-symbolsData_symbol0$logReturn
xclose<-symbolsData_symbol0$close
xSMA50<- runMean(xclose, n=20)
xSMA100<- runMean(xclose, n=40)

head(y)
tail(y)

length(xSMA50)
state0<-100*(xclose>xSMA50) + 10*(xSMA50>xSMA100)
state0.tb<-tibble(behind=lag(state0), state0, ahead=lead(state0))

df00<-rbind(data.frame(
  date=date0,
  value=xclose,
  type="Asset"),
  data.frame(
    date=date0,
    value=xSMA50,
    type="SMA50"),
  data.frame(
    date=date0,
    value=xSMA100,
    type="SMA100")
)

df00 %>% filter(date >=as.Date("2020-01-01")) %>%
ggplot(., aes(x=date, y=value, col=type)) +
  geom_line()

#
# States to define:
# Indicator of close larger than lag 1 close
# and lags of this indicator


ind.smac1<-ifelse(xclose>lag(xclose,n=1), 1,0)

smac0.tb<-tibble(
  smac1=ind.smac1,
  smac1.lag1=lag(ind.smac1),
  smac1.lag2=lag(lag(ind.smac1)))

tmp3<-split(y, f=smac0.tb)
df3<-data.frame(
  sum=sapply(tmp3,sum),
  n=sapply(tmp3,length)
)
df3

df3$return=df3$sum/df3$n
df3

code_smac1<-100*(ind.smac1>0) + 10*(lag(ind.smac1,1)>0) +
  1*(lag(lag(ind.smac1,1),1)>0)

table(code_smac1)

# One step transitions -- Markov Chain 

# Table of one-step transitions:
smac_onestep<-table(lag(code_smac1,1), code_smac1)
smac_onestep
round(prop.table(smac_onestep,margin =1), digits=4)


# Create ymat with columns corresonding to state-dependent returns
# 
ymat<-cbind(
  y*(code_smac1==0),
  y*(code_smac1==1),
  y*(code_smac1==10),
  y*(code_smac1==11),
  y*(code_smac1==100),
  y*(code_smac1==101),
  y*(code_smac1==110),
  y*(code_smac1==111)
  )

ymat0<-apply(ymat,2,fcn.dot0)
ymat0_cumsum<-apply(ymat0,2,cumsum)

plot(x=date0,y=ymat0_cumsum[,1],ylim=c(min(as.matrix(ymat0_cumsum)),
                                  max(as.matrix(ymat0_cumsum))),type="l")

for (j in c(2:ncol(ymat0_cumsum))){
  lines(date0, ymat0_cumsum[,j], col=j)
}

# Do same plot with ggplot

df0<-rbind( 
  data.frame(date=date0, value=ymat0_cumsum[,1], state=0),
  data.frame(date=date0, value=ymat0_cumsum[,2], state=1),
  data.frame(date=date0, value=ymat0_cumsum[,3], state=10),
  data.frame(date=date0, value=ymat0_cumsum[,4], state=11),
  data.frame(date=date0, value=ymat0_cumsum[,5], state=100),
  data.frame(date=date0, value=ymat0_cumsum[,6], state=101),
  data.frame(date=date0, value=ymat0_cumsum[,7], state=110),
  data.frame(date=date0, value=ymat0_cumsum[,8], state=111)
)

df0$state<-as.factor(df0$state)  
ggplot(df0, aes(x=date,y=value, col=state)) + geom_line()

# Each col of ymat0 corresponds to a trading  long
# only on days when the state condition holds
#

vec_means=apply(ymat0,2,mean)*252
vec_vol = sqrt(apply(ymat0,2,var))*sqrt(252)
vec_sharpe=vec_means/vec_vol

tab_perf<-cbind(
  mean=vec_means,
  vol=vec_vol,
  sharpe=vec_sharpe)

dimnames(tab_perf)[[1]]<- sort(unique(code_smac1))
tab_perf
# This gives annualized means/volatilities/share ratios of 
# trades

round(cor(ymat0), digits=5)

# Define wts vectors corresponding to trading strategies
# that are long on days with the respective states

wts0<-as.matrix(c(1:ncol(ymat0))*0 +1)
wts0
port_wts0_cumsum<-cumsum(ymat0 %*% wts0)
plot(date0, port_wts0_cumsum,type="l")
# wts1 which zeros cases 1 and 5

wts1<-wts0
wts1[1]<-0
wts1[5]<-0
port_wts1_cumsum<-cumsum(ymat0 %*% wts1)
plot(date0, port_wts1_cumsum,type="l")
lines(date0,port_wts0_cumsum,col='red')
