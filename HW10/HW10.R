#2-a
sal<-read.csv("programmer_salaries.txt", sep="\t")
reg<-lm(Salary~Experience+Score+Degree,data = sal)
head(sal,)
summary(reg)
head(reg$residual,n=5)
head(reg$fitted.values,n=5)
#2-b-i
salx<-data.matrix(sal[,1:3])
salx<-cbind(c(1),salx)
colnames(salx)[1]<-"intercept"
#2-b-ii
saly<-sal$Salary
#2-b-iii
beta_h<-(solve(t(salx)%*%salx))%*%t(salx)%*%saly
beta_h
#2-b-iv  
saly_h<-salx%*%beta_h  
head(saly_h,n=5)  
res<-saly-saly_h
head(res,n=5)
#2-b-v
SST<-sum((saly-mean(saly))^2)
SSR<-sum((saly_h-mean(saly))^2)
SSE<-sum((saly-saly_h)^2)
data.frame(SST,SSR,SSE)  
#2-c-i
R_square_1<-SSR/SST
R_square_1
#2-c-ii
R_square_2<-cor(saly,saly_h)^2
R_square_2
#3-a-i
library(dplyr)
library(ggplot2)
#install.packages("cowplot")
library(cowplot)
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto$brand <- as.character(lapply(auto$car_name,function(x){unlist(strsplit(x," "))[1]}))
my_summary <- auto %>%
  count(brand, sort = TRUE) 
my_summary #too many categories of car_name and brand, not valuable for linear regression model
auto['car_name']<-NULL #abandon car_name column
auto['brand']<-NULL
# Scatter plot
cylinders <- ggplot(auto, aes(x = cylinders, y = mpg))+ 
  geom_point(size=0.8)
displacement <- ggplot(auto, aes(x = displacement, y = mpg))+ 
  geom_point(size=0.8)
horsepower <- ggplot(auto, aes(x = horsepower, y = mpg))+ 
  geom_point(size=0.8)
weight <- ggplot(auto, aes(x = weight, y = mpg))+ 
  geom_point(size=0.8)
acceleration <- ggplot(auto, aes(x = acceleration, y = mpg))+ 
  geom_point(size=0.8)
model_year <- ggplot(auto, aes(x = model_year, y = mpg))+ 
  geom_point(size=0.8)
origin <- ggplot(auto, aes(x = origin, y = mpg))+ 
  geom_point(size=0.8)
ggdraw() +
  draw_plot(cylinders, 0, .6, .33, .35) +
  draw_plot(displacement, .33, .6, .33, .35) +
  draw_plot(horsepower, .66, .6, .33, .35) +
  draw_plot(weight, 0, .3, .4, .3) +
  draw_plot(acceleration, .5, .3, .4, .3) +
  draw_plot(model_year, 0, 0, .4, .3) +
  draw_plot(origin, .5, 0, .4, .3) 
#3-a-ii
cor_table<-round(cor(auto[,c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin")], use = "pairwise.complete.obs"),digits=2)
cor_table
#3-a-iii
#correlation of mpg and weight seem to be -0.8317409(highest)
#3-a-iv
#relationship between mpg and car_name,origin,acceleration are not linear
plot(auto$mpg,auto[,2:8])
plot(auto, pch='.')
#3-a-v
library(reshape2)
diag(cor_table) <- 0
cor_melt <- melt(cor_table)
hight_cor <- cor_melt[order(abs(cor_melt$value),decreasing = T) & abs(cor_melt$value) >0.7,]

#### eliminate the same combination of variable 
#sort two variable by first character order
hight_cor[1:2] <- t( apply(hight_cor[1:2], 1, sort) )
#eliminate the same variable combination
hight_cor[!duplicated(hight_cor[1:2]),]
#3-b
regr <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+model_year+factor(origin)
           ,data = auto)
summary(regr)

mpg_reg<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+model_year+factor(origin),data = auto)
summary(mpg_reg)
factor(auto$origin)
#significant var:displacement,weight,model_year,factor(origin)2,factor(origin)3
#3-c-i
auto_std <- cbind(scale(auto[1:7]),auto$origin)#origin is categorical variable
colnames(auto_std) <- colnames(auto[1:8])
auto_std <- data.frame(auto_std)
mpg_stdreg<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+model_year+factor(origin),data = auto_std)
summary(mpg_stdreg)
#3-c-ii
#unsignificant var:cylinders,horsepower,acceleration
cy<-lm(mpg~cylinders,data=auto)
summary(cy)
hp<-lm(mpg~horsepower,data=auto)
summary(hp)
ac<-lm(mpg~acceleration,data=auto)
summary(ac)
#3-c-iii
plot(density(cy$residuals),main = "cylinders$residuals")
plot(density(hp$residuals),main = "horsepower$residuals")
plot(density(ac$residuals),main = "acceleration$residuals")

