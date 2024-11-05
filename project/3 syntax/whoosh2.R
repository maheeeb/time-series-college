# dt <- read.csv("D:\\IPB\\5\\MPDW\\project\\5 data\\kereta_2022-2024.csv")
# head(dt)
# names(dt)
# dt$Tanggal <- as.Date(dt$Tanggal, format = "%Y-%m-%d")
# dt %>% filter(ID.Prasarana=="KCJB-HLM") %>% 
#   ggplot()+
#   geom_line(aes(x=Tanggal, y=Penumpang.Berangkat, color="red"))+
#   geom_line(aes(x=Tanggal, y=Penumpang.Datang, color="blue"))
# 
# 
# head(dt)
# 
# dt.whoosh <- dt %>% filter(ID.Prasarana=="KCJB-HLM") %>%
#   select(Tanggal, Penumpang.Berangkat, Penumpang.Datang) %>% 
#   rename(brgkt=Penumpang.Berangkat, dtg=Penumpang.Datang)
# 
# head(dt.whoosh)
# tail(dt.whoosh)
# 
# dim(dt.whoosh)
# 
# dt.whoosh %>% 
#   ggplot()+
#   geom_line(aes(x=Tanggal, y=brgkt+dtg, color="red"))
# 
# dt.whoosh$passenger <- dt.whoosh$brgkt+dt.whoosh$dtg
# 
# write.csv(dt.whoosh,"D:\\IPB\\5\\MPDW\\project\\9 whoosh\\whoosh_data.csv")

dt <- read.csv("D:\\IPB\\5\\MPDW\\project\\9 whoosh\\whoosh_data.csv")

dt.ts <- ts(dt$passenger)
plot(dt.ts)

index.train <- c(1:length(train.ts))
bc.train <- boxcox(train.ts~ index.train, lambda = seq(-2, 2, 0.1))

train.ts <- dt.ts[1:(round(length(dt.ts)*0.80)+1)] %>% ts()
test.ts <- dt.ts[(round(length(dt.ts)*0.80)+2):length(dt.ts)] %>% ts()

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  labs(title="Pembagian Data Skenario 1",
       x = "Periode",
       y = "Penumpang")

plot(train.ts)

## plot data train and test
ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="#262626")+
  labs(title="Data Latih Skenario 2",
       x = "Periode",
       y = "Penumpang")+
  theme_minimal()
  #geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")

plot(train.ts)



acf(train.ts,main="ACF Data Latih",lag.max=50)

train.diff1 <- diff(train.ts)

acf(train.diff1,lag.max=60)

plot(train.diff1)

train.diff2 <- diff(train.diff1,lag=7)
plot(train.diff2)

index.diff2 <- c(1:length(train.diff2))
bc.diff2 <- boxcox((train.diff2 - min(train.diff2)+1) ~ index.diff2
                   , lambda = seq(-2, 2, 0.1))
lambda <- bc.diff2$x[which.max(bc.diff2$y)]
lambda

acf(train.diff2,lag.max=60,main="ACF") #cutoff after 2, ma  1 7 
pacf(train.diff2,lag.max=60,main="PACF") ## cutoof 2 ar 1
eacf(train.diff2,ar.max=10,ma.max=10)

# model tentatif
# (0,1,2)(0,1,1)7
# (2,1,0)(0,1,1)7
# (2,1,2)(0,1,1)7
# 
# (0,1,2)(5,1,0)7
# (2,1,0)(5,1,0)7
# (2,1,2)(5,1,0)7

model1 <- Arima(train.ts, order=c(1,1,2), seasonal = list(order = c(0,1,1), period = 7))
summary(model1)
lmtest::coeftest(model1) # 4719.48 all sig

model2 <- Arima(train.ts, order=c(2,1,2), seasonal = list(order = c(0,1,1), period = 7))
summary(model2)
lmtest::coeftest(model2) # 4721.15 not sig

model3 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(0,1,1), period = 7))
summary(model3)
lmtest::coeftest(model3) #  4719.96

model4 <- Arima(train.ts, order=c(1,1,2), seasonal = list(order = c(5,1,0), period = 7))
summary(model4)
lmtest::coeftest(model4) #4724 not sig

model5 <- Arima(train.ts, order=c(2,1,2), seasonal = list(order = c(5,1,0), period = 7))
summary(model5)
lmtest::coeftest(model5) #4725.54

model6 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(5,1,0), period = 7))
summary(model6)
lmtest::coeftest(model6) #4723 

## Overfitting Model 3
model7 <- Arima(train.ts, order=c(2,1,1), seasonal = list(order = c(0,1,1), period = 7))
summary(model7)
lmtest::coeftest(model7) #4720 not sig

model8 <- Arima(train.ts, order=c(1,1,2), seasonal = list(order = c(0,1,1), period = 7))
summary(model8)
lmtest::coeftest(model8) # 4719.48 

model9 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))
summary(model9)
lmtest::coeftest(model9) #4716.01

model10 <- Arima(train.ts, order=c(1,1,1), seasonal = list(order = c(0,1,2), period = 7))
summary(model10)
lmtest::coeftest(model10) # 4716.28

## model 9 terkecil AIC

sisaan <- model9$residuals
jarque.bera.test(sisaan)
Box.test(sisaan,type="Ljung-Box")
Box.test(sisaan^2,type="Ljung-Box")

## model 9 terpenuhi

## accuracy train
accuracy(model9$fitted,train.ts)

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=1:length(train.ts), y=model7$fitted), color="red")

## forecast
forecast1 <- forecast::forecast(model9, h=length(test.ts))
plot(forecast1)

## accuracy test
accuracy(ts(forecast1$mean),ts(test.ts))

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=forecast1$mean), color="green")


## SSA
library(Rssa)

## Function for Building Signals
build_c <- function(r,groups){
  hasil <- r[[1]]
  for(i in 2:groups){
    hasil <- hasil + r[[i]]
  }
  return(hasil)
}

## L 10 train 2, test 10.8

s <- ssa(train.ts,L=21)
plot(s,type="vectors")
plot(s,type="paired")
plot(s,type="wcor",groups = 1:10)

r <- reconstruct(s, groups = list(c(1),c(2:4),c(8:10)))
plot(r)


r1 <- build_c(r,3)

plot(r1,main="Rekonstruksi Sinyal",xlab="Periode",ylab="Penumpang")

accuracy(ts(r1),ts(train.ts))

predict.ssa <- rforecast(s,groups = list(c(1),c(2:4),c(8:10)),len=length(test.ts))
predict.ssa.const <- build_c(predict.ssa,3)
plot(predict.ssa.const)


accuracy(ts(predict.ssa.const),ts(test.ts))

ggplot()+
  geom_line(aes(x=1:length(train.ts), y=train.ts), color="blue")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=test.ts), color="red")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=predict.ssa.const), color="green")+
  geom_line(aes(x=(1:length(test.ts)+length(train.ts)), y=forecast1$mean), color="purple")


## Skenario 1 

train.sk1 <- dt.ts[1:(round(length(dt.ts)*0.70)+3)] %>% ts()
test.sk1 <- dt.ts[(round(length(dt.ts)*0.70)+4):length(dt.ts)] %>% ts()

model9.sk1 <- Arima(train.sk1, order=c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))

accuracy(model9.sk1$fitted,train.sk1)

sisaan.sk1 <- model9.sk1$residuals
jarque.bera.test(sisaan.sk1)
Box.test(sisaan.sk1,type="Ljung-Box")
Box.test(sisaan.sk1^2,type="Ljung-Box")

forecast1.sk1 <- forecast::forecast(model9.sk1, h=length(test.sk1))
accuracy(ts(forecast1.sk1$mean),ts(test.sk1))

ssa.sk1 <- ssa(train.sk1,L=21)
#r.sk1 <- reconstruct(ssa.sk1, groups = list(c(1),c(2:4),c(8:10)))
r.sk1 <- reconstruct(ssa.sk1, groups = g)
r1.sk1 <- build_c(r.sk1,2)

accuracy(ts(r1.sk1),ts(train.sk1))

# predict.ssa.sk1 <- rforecast(ssa.sk1,groups = list(c(1),c(2:4),c(8:10)),len=length(test.sk1))
predict.ssa.sk1 <- rforecast(ssa.sk1,groups = g,len=length(test.sk1))
predict.ssa.const.sk1 <- build_c(predict.ssa.sk1,2)

accuracy(ts(predict.ssa.const.sk1),ts(test.sk1))

ggplot()+
  geom_line(aes(x=1:length(train.sk1), y=train.sk1), color="blue")+
  geom_line(aes(x=(1:length(test.sk1)+length(train.sk1)), y=test.sk1), color="red")+
  geom_line(aes(x=(1:length(test.sk1)+length(train.sk1)), y=predict.ssa.const.sk1), color="green")+
  geom_line(aes(x=(1:length(test.sk1)+length(train.sk1)), y=forecast1.sk1$mean), color="purple")

## Skenario 3

train.sk3 <- dt.ts[1:(round(length(dt.ts)*0.90)-2)] %>% ts()
test.sk3 <- dt.ts[(round(length(dt.ts)*0.90)-1):length(dt.ts)] %>% ts()

ggplot()+
  geom_line(aes(x=1:length(train.sk3), y=train.sk3), color="blue")+
  geom_line(aes(x=(1:length(test.sk3)+length(train.sk3)), y=test.sk3), color="red")

model9.sk3 <- Arima(train.sk3, order=c(1,1,1), seasonal = list(order = c(1,1,1), period = 7))

sisaan.sk3 <- model9.sk3$residuals
jarque.bera.test(sisaan.sk3)
Box.test(sisaan.sk3,type="Ljung-Box")
Box.test(sisaan.sk3^2,type="Ljung-Box")

accuracy(model9.sk3$fitted,train.sk3)

forecast1.sk3 <- forecast::forecast(model9.sk3, h=length(test.sk3))
accuracy(ts(forecast1.sk3$mean),ts(test.sk3))

ssa.sk3 <- ssa(train.sk3,L=21)
r.sk3 <- reconstruct(ssa.sk3, groups = list(c(1),c(2:4),c(8:10)))
r1.sk3 <- build_c(r.sk3,3)

accuracy(ts(r1.sk3),ts(train.sk3))

predict.ssa.sk3 <- rforecast(ssa.sk3,groups = list(c(1),c(2:4),c(8:10)),len=length(test.sk3))
predict.ssa.const.sk3 <- build_c(predict.ssa.sk3,3)

accuracy(ts(predict.ssa.const.sk3),ts(test.sk3))

ggplot()+
  geom_line(aes(x=1:length(train.sk3), y=train.sk3), color="blue")+
  geom_line(aes(x=(1:length(test.sk3)+length(train.sk3)), y=test.sk3), color="red")+
  geom_line(aes(x=(1:length(test.sk3)+length(train.sk3)), y=predict.ssa.const.sk3), color="green")+
  geom_line(aes(x=(1:length(test.sk3)+length(train.sk3)), y=forecast1.sk3$mean), color="purple")



## Test AUTOSSA

g <- grouping.auto.pgram(s, base="series")
plot(reconstruct(s, groups = g))

rauto <- reconstruct(s, groups = g)
rauto1 <- build_c(rauto,2)

plot(rauto1)

accuracy(ts(rauto1),ts(train.ts))

predict.ssa.auto <- rforecast(s,groups = g,len=length(test.ts))
predict.ssa.const.auto <- build_c(predict.ssa.auto,2)
plot(predict.ssa.const.auto)

accuracy(ts(predict.ssa.const.auto),ts(test.ts))
