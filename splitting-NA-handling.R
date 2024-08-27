test <- read.csv("D:\\IPB\\5\\MPDW\\data\\dt_BPN.csv")
test <- test %>% select(Tanggal,Penumpang.Berangkat)
test$Tanggal <- as.Date(test$Tanggal, format = "%Y-%m-%d")
dim(test)
test.ts <- ts(test$Penumpang.Berangkat,start=c(2023,1),end=c(2024,7),frequency=1)

plot(test.ts)



### impute 0 value
test[test$Penumpang.Berangkat == 0,]

### 5 data 0

# impute data with the average between the previous value and the next value
test$Penumpang.Berangkat[test$Penumpang.Berangkat == 0] <- NA
test$Penumpang.Berangkat <- na.approx(test$Penumpang.Berangkat)

test.ts <- ts(test$Penumpang.Berangkat)
plot(test.ts)
summary(test.ts)

spline  

## spliting data
dim(test)

dt_pt1 <- test[1:115,]
dt_pt2 <- test[116:230,]
dt_pt3 <- test[231:345,]
dt_pt4 <- test[346:460,]
dt_pt5 <- test[461:571,]

## export 
write.csv(dt_pt1, "D:\\IPB\\5\\MPDW\\data\\dt_pt1.csv", row.names = FALSE)
write.csv(dt_pt2, "D:\\IPB\\5\\MPDW\\data\\dt_pt2.csv", row.names = FALSE)
write.csv(dt_pt3, "D:\\IPB\\5\\MPDW\\data\\dt_pt3.csv", row.names = FALSE)
write.csv(dt_pt4, "D:\\IPB\\5\\MPDW\\data\\dt_pt4.csv", row.names = FALSE)
write.csv(dt_pt5, "D:\\IPB\\5\\MPDW\\data\\dt_pt5.csv", row.names = FALSE)
write.csv(test, "D:\\IPB\\5\\MPDW\\data\\dt_BPN_impute.csv", row.names = FALSE)
