library(TSA)
dt <- read.csv("D:\\IPB\\5\\MPDW\\data\\dt_BPN_impute.csv")

str(dt)
names(dt) <- tolower(names(dt))

head(dt)
str(dt)

dt$tanggal <- as.Date(dt$tanggal,format="%Y-%m-%d")

dt.ts <- ts(dt$penumpang.berangkat)
plot(dt.ts)


str(dt)

# ACF
acf(dt.ts, lag.max=30)

library(tseries)
adf.test(dt.ts)


### dt utama <-
library(dplyr)
dt <- read.csv("D:\\IPB\\5\\MPDW\\data\\data_0101-3107_mpdw _csv.csv")
str(dt)

names(dt) <- tolower(names(dt))

dt$tanggal <- as.Date(dt$tanggal,format="%Y-%m-%d")



dt_cgk <- dt %>% filter(ID.Prasarana == "CGK")
str(dt_cgk)

dt_cgk <- dt_cgk %>% select(Tanggal, Penumpang.Berangkat)

dt_cgk$Tanggal <- as.Date(dt_cgk$Tanggal,format="%Y-%m-%d")

dt_cgk.ts <- ts(dt_cgk$Penumpang.Berangkat)
plot(dt_cgk.ts)
adf.test(dt_cgk.ts)

airport_kal <- c("BPN","BDJ","PKN","PKY","PNK", "AAP","TRK","BEJ")

dt_kal <- dt %>% filter(id.prasarana %in% airport_kal)
dt_kal <- dt_kal %>% select(tanggal, penumpang.berangkat, ID.Prasarana)
unique(dt_kal$ID.Prasarana) %>% length()

str(dt_kal)

ggplot(dt_kal,aes(x=tanggal, y=penumpang.berangkat,col=as.factor(id.prasarana)))+
  geom_line()


dt_BPN <- dt_kal %>% filter(id.prasarana == "BPN")

ggplot(dt_BPN,aes(x=tanggal))+
  geom_line(aes(y=penumpang.berangkat),col="blue")+
  geom_line(aes(y=penumpang.tiba),col="red")




tom <- read.csv("D:\\IPB\\5\\MPDW\\data\\tomtom_20_27.csv")
str(tom)
tom$Time1 <- as.Date(tom$Time)
str(tom)

datetime <- "2024-08-28 14:35:00"
tom$Time1 <- as.POSIXct(tom$Time, format="%Y-%m-%d %H:%M:%S")
str(tom)

ggplot(tom, aes(x=Time1, y=Travel.Time)) + geom_line()

tom2 <- read.csv("D:\\IPB\\5\\MPDW\\data\\traffic_data-03-14-8-to-03-21-8.csv")

View(tom2)
str(tom2)
tom2$Time1 <- as.POSIXct(tom2$Time, format="%Y-%m-%d %H:%M:%S")
str(tom2)

ggplot(tom2, aes(x=Time1, y=Jams.Length)) + geom_line()

View(tom)
tomawal <- tom2 %>% filter(Time1 < "2024-08-28 13:00:00")
View(tomawal)

tom2[1:154,] %>% View()

gabung <- rbind(tom2[1:154,], tom)
dim(gabung)

ggplot(gabung) + 
  geom_line(aes(x=Time1, y=Jams.Length, color="jamsLen")) +
  geom_line(aes(x=Time1, y=Travel.Time, color="travelTime")) +
  geom_line(aes(x=Time1, y=Jams.Count, color="jamsCount"))+
  geom_line(aes(x=Time1, y=Live.Traffic, color="index")) +
  scale_color_manual(values=c("jamsLen"="red",
                              "travelTime"="blue",
                              "jamsCount"="green",
                              "index"="black"))

write.csv(gabung, "D:\\IPB\\5\\MPDW\\data\\gabung.csv")


  
