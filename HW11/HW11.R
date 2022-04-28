#1-a
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto_log <- with(auto, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), log(acceleration), 
                                  model_year, origin))
log_regr<-lm(log.mpg.~log.cylinders.+log.displacement.+log.horsepower.+log.weight.+log.acceleration.+model_year+factor(origin),data = auto_log)
summary(log_regr)
#1-a-i 
#p-value<0.1:log.horsepower. log.weight. log.acceleration. model_year factor(origin)2 factor(origin)3 
#1-a-ii
#originally, 'horsepower','acceleration' don't have sinificant effect on mpg(with p val>0.1), while after log transformation, it now have significant effect on it.
#1-a-iii

#1-b-i
regr_wt<-lm(mpg~weight,data=auto)
#1-b-ii
log_regr_wt<-lm(log.mpg.~log.weight.,data=auto_log)
#1-b-iii
#density plot
plot(density(regr_wt$residuals),main="Distribution of regr_wt$residuals",sub=NULL,ylab="residuals")
abline(v=0,col="red",lwd=1)
plot(density(log_regr_wt$residuals),main="Distribution of Log_regr_wt$residuals",sub=NULL,ylab="residuals")
abline(v=0,col="red",lwd=1)
#scatterplot
plot(auto_log$log.weight.,log_regr_wt$residuals,cex=0.55,lwd=1.7,col=2)
#1-b-iv
#1-b-v
#1-c
#bootstrap

#2-1
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
               log.weight. + log.acceleration. + model_year +
               factor(origin), data=auto_log)





