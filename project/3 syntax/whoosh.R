dt <- read.csv("D:\\IPB\\5\\MPDW\\project\\5 data\\kereta_2022-2024.csv")
head(dt)
names(dt)
dt$Tanggal <- as.Date(dt$Tanggal, format = "%Y-%m-%d")
dt %>% filter(ID.Prasarana=="KCJB-HLM") %>% 
  ggplot()+
  geom_line(aes(x=Tanggal, y=Penumpang.Berangkat, color="red"))+
  geom_line(aes(x=Tanggal, y=Penumpang.Datang, color="blue"))
  

head(dt)

dt.whoosh <- dt %>% filter(ID.Prasarana=="KCJB-HLM") %>%
  select(Tanggal, Penumpang.Berangkat, Penumpang.Datang) %>% 
  rename(brgkt=Penumpang.Berangkat, dtg=Penumpang.Datang)

head(dt.whoosh)

dim(dt.whoosh)

dt.whoosh %>% 
  ggplot()+
  geom_line(aes(x=Tanggal, y=brgkt+dtg, color="red"))

dt.ts <- ts(dt.whoosh$brgkt+dt.whoosh$dtg)
plot(dt.ts)

train.ts <- dt.ts[1:round(length(dt.ts)*0.80)+1] %>% ts()
test.ts <- dt.ts[(round(length(dt.ts)*0.80)+2):length(dt.ts)] %>% ts()

## plot data train and test
ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")

plot(train.ts)

train.diff1 <- diff(train.ts)

acf(train.diff1,lag.max=60)

plot(train.diff1)

train.diff2 <- diff(train.diff1,lag=7)

acf(train.diff2,lag.max=60) #cutoff after 2, ma  1 7 
pacf(train.diff2,lag.max=60) ## cutoof 2 ar 1
eacf(train.diff2)

# model tentatif
# (0,1,2)(0,1,1)7
# (2,1,0)(0,1,1)7
# (2,1,2)(0,1,1)7
# 
# (0,1,2)(5,1,0)7
# (2,1,0)(5,1,0)7
# (2,1,2)(5,1,0)7

model1 <- Arima(train.ts, order=c(0,1,2), seasonal = list(order = c(0,1,1), period = 7))
summary(model1)
lmtest::coeftest(model1) #4704

model2 <- Arima(train.ts, order=c(2,1,0), seasonal = list(order = c(0,1,1), period = 7))
summary(model2)
lmtest::coeftest(model2) #4714 

model3 <- Arima(train.ts, order=c(2,1,2), seasonal = list(order = c(0,1,1), period = 7))
summary(model3)
lmtest::coeftest(model3) #4714 not sig

model4 <- Arima(train.ts, order=c(0,1,2), seasonal = list(order = c(5,1,0), period = 7))
summary(model4)
lmtest::coeftest(model4) #4710

model5 <- Arima(train.ts, order=c(2,1,0), seasonal = list(order = c(5,1,0), period = 7))
summary(model5)
lmtest::coeftest(model5) #4718

model6 <- Arima(train.ts, order=c(2,1,2), seasonal = list(order = c(5,1,0), period = 7))
summary(model6)
lmtest::coeftest(model6) #4718

## Overfitting Model 4
model7 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(5,1,0), period = 7))
summary(model7)
lmtest::coeftest(model7) #4706 sig

model8 <- Arima(train.ts, order=c(1,1,2), seasonal = list(order = c(5,1,0), period = 7))
summary(model8)
lmtest::coeftest(model8) # not sig

model9 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(5,1,1), period = 7))
summary(model9)
lmtest::coeftest(model9) #not sig

model10 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(6,1,0), period = 7))
summary(model10)
lmtest::coeftest(model10) #not sig

## model 7 fix

sisaan <- model7$residuals
jarque.bera.test(sisaan)
Box.test(sisaan,type="Ljung-Box")
Box.test(sisaan^2,type="Ljung-Box")

## accuracy train
accuracy(model7$fitted,train.ts)

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=1:length(train.ts), y=model7$fitted), color="red")

## forecast
forecast1 <- forecast::forecast(model7, h=length(test.ts))
plot(forecast1)

## accuracy test
accuracy(ts(forecast1$mean),ts(test.ts))

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=forecast1$mean), color="green")


## SSA
library(Rssa)
s <- ssa(train.ts)
plot(s,type="vectors")
plot(s,type="paired")

r <- reconstruct(s, groups = 1:5)
plot(r)

r1 <- r$F1+r$F2

plot(r1)

predict.ssa <- rforecast(s,groups=1:2,len=length(test.ts))
predict.ssa.const <- predict.ssa$F1+predict.ssa$F2
plot(predict.ssa.const)

accuracy(ts(predict.ssa.const),ts(test.ts))

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=predict.ssa.const), color="green")


## MSTL
mstl <- mstl(train.ts, seasonal.window = 7, robust = TRUE, method = "ets")
plot(mstl)


## MA Decompose
library(forecast)
ma.train <- ts(train.ts,frequency=7)
ma.decompose <- stats::decompose(ma.train)
plot(ma.decompose)

### Get trend
trend.ma.dc <- ma.decompose$trend

write.csv(trend.ma.dc,"D:\\IPB\\5\\MPDW\\project\\9 whoosh\\trend_ma_dc.csv")

trend.ma.test <- read.csv("D:\\IPB\\5\\MPDW\\project\\9 whoosh\\trend_ma_dc_test.csv")

hybrid.ma <- trend.ma.test$X0 + ma.decompose$seasonal[2:67]

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=hybrid.ma), color="green")

ggplot()+
  geom_line(aes(x=1:length(test.ts), y=test.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)), y=hybrid.ma), color="red")

accuracy(ts(hybrid.ma),ts(test.ts))
accuracy(ts(trend.ma.test$X0),ts(test.ts))
accuracy(ts(forecast1$mean),ts(test.ts))




