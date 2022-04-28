#匯入資料
data<-data.frame(read.csv("customers.txt"))
#1
data[5,1]
#2
data2<-unique(data)
data2
data2<-sort(data2[1:399,1])
data2
data2[5]
#3
data3<-sort(data[1:399,1])
data3[1:5]
#4
data4<-unique(data)
data4<-sort(data[1:399,1], decreasing = TRUE)
data4<-unique(data4)
data4[1:5]
#5
mean<-mean(data[1:399,1])
mean

#6
sd<-sd(data[1:399,1])
sd
#7
age_diff<-data[1:399,1]-mean
age_diff
#8
mean(age_diff)
#9-1
hist(data[1:399,1], main=NULL,xlab="customers age")
#9-2
?density
d <- density(data[1:399,1]) # returns the density data
plot(d) # plots the results
#9-3
boxplot(data[1:399,1],horizontal = TRUE)
stripchart(data[1:399,1], method = "stack", add = TRUE)

