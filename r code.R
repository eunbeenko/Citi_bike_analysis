##time peak for weekdays and weekends in 2017 (daily average)

#file name: citibikedf_2017qu01_hour_avg_weekday.csv
qr1wd<-read.csv(file.choose(),header=T)
qr1wd.ts<-ts(qr1wd$Bike,start=0,freq=1)
#file name: citibikedf_2017qu01_hour_avg_weekend.csv
qr1we<-read.csv(file.choose(),header=T)
qr1we.ts<-ts(qr1we$Bike,start=0,freq=1)
plot(qr1we.ts, xlab="hour", ylab="bike amount", main="2017 quarter 1 bike usage")
lines(qr1wd.ts, col='blue')
legend("topleft",lty=1,col=c("black","blue"),legend=c("weekends","weekdays"))
#file name: citibikedf_2017qu02_hour_avg_weekday.csv
qr2wd<-read.csv(file.choose(),header=T)
qr2wd.ts<-ts(qr2wd$Bike,start=0,freq=1)
#file name: citibikedf_2017qu02_hour_avg_weekend.csv
qr2we<-read.csv(file.choose(),header=T)
qr2we.ts<-ts(qr2we$Bike,start=0,freq=1)
plot(qr2we.ts, xlab="hour", ylab="bike amount", main="2017 quarter 2 bike usage")
lines(qr2wd.ts, col='blue')
legend("topleft",lty=1,col=c("black","blue"),legend=c("weekends","weekdays"))
#file name: citibikedf_2017qu03_hour_avg_weekday.csv
qr3wd<-read.csv(file.choose(),header=T)
qr3wd.ts<-ts(qr3wd$Bike,start=0,freq=1)
#file name: citibikedf_2017qu03_hour_avg_weekend.csv
qr3we<-read.csv(file.choose(),header=T)
qr3we.ts<-ts(qr3we$Bike,start=0,freq=1)
plot(qr3we.ts, xlab="hour", ylab="bike amount", main="2017 quarter 3 bike usage")
lines(qr3wd.ts, col='blue')
legend("topleft",lty=1,col=c("black","blue"),legend=c("weekends","weekdays"))
#file name: citibikedf_2017qu04_hour_avg_weekday.csv
qr4wd<-read.csv(file.choose(),header=T)
qr4wd.ts<-ts(qr4wd$Bike,start=0,freq=1)
#file name: citibikedf_2017qu04_hour_avg_weekend.csv
qr4we<-read.csv(file.choose(),header=T)
qr4we.ts<-ts(qr4we$Bike,start=0,freq=1)
plot(qr4we.ts, xlab="hour", ylab="bike amount", main="2017 quarter 4 bike usage")
lines(qr4wd.ts, col='blue')
legend("topleft",lty=1,col=c("black","blue"),legend=c("weekends","weekdays"))


##how weather affects customers on bike usage

#file name: citibikeAndWeather_date_average_Customer.csv
weathercus<-read.csv(file.choose(),header=T)
weathercus.lm<-lm(Bike~AWND+PRCP+TAVG, data=weathercus)
summary(weathercus.lm)
plot(weathercus.lm, main="Customer residual plot")
hist(weathersub$Bike,breaks =30)
weatherlogcus.lm<-lm(log(Bike)~AWND+PRCP+TAVG, data=weathercus)
summary(weatherlogcus.lm)
plot(weatherlogcus.lm, main="log Customer residual plot")


##how weather affects subscriber on bike usage

#file name: citibikeAndWeather_date_average_Subscriber.csv
weathersub<-read.csv(file.choose(),header=T)
weathersub.lm<-lm(Bike~AWND+PRCP+TAVG, data=weathersub)
summary(weathersub.lm)
plot(weathersub.lm, main="Subscriber residual plot")


##weather analysis with user type as Indicator variable(subscriber=1, customer=0)

#file name: combine s and c.csv
weathersc<-read.csv(file.choose(),header=T)
head(weathersc)
weathersc.lm<-lm(Bike~AWND+PRCP+TAVG+I+I*AWND+I*PRCP+I*TAVG+I*AWND*PRCP+I*AWND*TAVG+I*PRCP*TAVG+I*AWND*PRCP*TAVG, data=weathersc)
summary(weathersc.lm)
weathersc.lm2<-lm(Bike~AWND+TAVG+I+I*AWND+I*TAVG+I*AWND*TAVG, data=weathersc)
summary(weathersc.lm2)
weathersc.lm3<-lm(Bike~TAVG+I+I*TAVG, data=weathersc)
summary(weathersc.lm3)
plot(weathersc.lm, main="Subscriber and customer residual plot")


##how temperature gives affect to gender

#file name: citibike_Subscriber_male_female.csv
weathergen<-read.csv(file.choose(),header=T)
head(weathergen)
weathergen.lm<-lm(Bike~TAVG+I+I*TAVG, data=weathergen)
summary(weathergen.lm)
plot(weathergen$TAVG,weathergen$Bike, main="bike vs. temperature")
female<-subset(weathergen,I==1)
weathergen.lm2<-lm(Bike~TAVG+I+I*TAVG, data=female)
abline(weathergen.lm2,col='red')
male<-subset(weathergen,I==0)
weathergen.lm3<-lm(Bike~TAVG+I+I*TAVG, data=male)
abline(weathergen.lm3,col='blue')
plot(weathergen.lm, main="gender temperature residual plot")


##how precipitation gives affect to gender

weathergen2.lm<-lm(Bike~PRCP+I+I*PRCP, data=weathergen)
summary(weathergen2.lm)
plot(weathergen$PRCP,weathergen$Bike, main="bike vs. precipitation")
weathergen2.lm2<-lm(Bike~PRCP+I+I*PRCP, data=female)
abline(weathergen2.lm2,col='red')
weathergen2.lm3<-lm(Bike~PRCP+I+I*PRCP, data=male)
abline(weathergen2.lm3,col='blue')
plot(weathergen2.lm, main="gender precipitation residual plot")

