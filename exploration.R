library(TSA)
dt <- read.csv("D:\\IPB\\5\\MPDW\\data\\dt_BPN_impute.csv")

str(dt)
names(dt) <- tolower(names(dt))

head(dt)
str(dt)

dt$tanggal <- as.Date(dt$tanggal,format="%Y-%m-%d")

dt.ts <- ts(dt$penumpang.berangkat)
plot(dt.ts)

# ACF
acf(dt.ts, lag.max=30)

library(tseries)
adf.test(dt.ts)


### dt utama <-
library(dplyr)
dt <- read.csv("D:\\IPB\\5\\MPDW\\data\\data_0101-3107_mpdw _csv.csv")
str(dt)

dt_cgk <- dt %>% filter(ID.Prasarana == "CGK")
str(dt_cgk)

dt_cgk <- dt_cgk %>% select(Tanggal, Penumpang.Berangkat)

dt_cgk$Tanggal <- as.Date(dt_cgk$Tanggal,format="%Y-%m-%d")

dt_cgk.ts <- ts(dt_cgk$Penumpang.Berangkat)
plot(dt_cgk.ts)
adf.test(dt_cgk.ts)

airport_kal <- c("BPN","BDJ","PKN","PKY","PNK", "AAP","TRK","BEJ")

dt_kal <- dt %>% filter(ID.Prasarana %in% airport_kal)
dt_kal <- dt_kal %>% select(Tanggal, Penumpang.Berangkat, ID.Prasarana)
unique(dt_kal$ID.Prasarana) %>% length()

dt_kal_sum <- dt_kal %>% group_by(Tanggal) %>% 
  summarise(Penumpang.Berangkat = sum(Penumpang.Berangkat))
dim(dt_kal_sum)
summary(dt_kal_sum)

dt_kal_sum$Tanggal <- as.Date(dt_kal_sum$Tanggal,format="%Y-%m-%d")
summary(dt_kal_sum)
dt_kal_sum[duplicated(dt_kal_sum$Tanggal),]

kalsum.ts <- ts(dt_kal_sum$Penumpang.Berangkat)
plot(kalsum.ts)  
adf.test(kalsum.ts)


