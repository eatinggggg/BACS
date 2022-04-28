#Q1
#1-(b)
#install.packages("reshape2")
library(reshape2)
verizon<-read.csv("verizon_wide.csv")
verizon_long<-melt(verizon,na.rm=TRUE,variable.name = "company",value.name ="response_time")
verizon_long
#1-(c)
head(verizon_long)
tail(verizon_long)
#1-(d)
verizon_sep<-split(verizon_long,f=verizon_long$company)
plot(density(verizon_sep$ILEC$response_time),col="blue",lwd=2,xlim=c(0,200))
lines(density(verizon_sep$CLEC$response_time),col="red",lwd=2)
legend(x=110,y=0.12,legend=c("alentus","hostmonster"),col=c("blue","red"),lty=1)

#2-(a)
#null-hypothesis:the mean response time for ILEC and CLEC customers are the same same
#2-(b)
t.test(verizon_sep$ILEC$response_time,verizon_sep$CLEC$response_time,alt="greater",var.equal=TRUE)
t.test(verizon_sep$ILEC$response_time,verizon_sep$CLEC$response_time,alt="greater",var.equal=FALSE)
#2-(c)-(i)
observed_diff<-mean(verizon_sep$ILEC$response_time)-mean(verizon_sep$CLEC$response_time)
observed_diff
permute_diff<-function(value,group){
  permuted<-sample(value,replace = FALSE)
  grouped<-split(permuted,group)
  permute_diff<-mean(grouped$ILEC)-mean(grouped$CLEC)
}
permuted_diffs<-replicate(10000,permute_diff(verizon_long$response_time,verizon_long$company))
hist(permuted_diffs,probability = TRUE,breaks = "fd",xlim=c(-20,10))
lines(density(permuted_diffs),lwd=1.5)
abline(v=observed_diff,col="blue",lwd=1.5)
#2-(c)-(ii)
p_onetail<-sum(permuted_diffs>observed_diff)/10000
p_twotail<-sum(abs(permuted_diffs)>observed_diff)/10000
p_onetail
p_twotail
#2-(c)-iii
#no

#3-a
time_ranks<-rank(verizon_long$response_time)
ranked_groups<-split(time_ranks,verizon_long$company)
U1<-sum(ranked_groups$CLEC)
n1<-length(verizon_sep$CLEC)
w<-U1-(n1*(n1+1))/2
w
#3-b
n2<-length(verizon_sep$ILEC)
wilcox_p_1tail<-1-pwilcox(w,n1,n2)
wilcox_p_2tail<-2*wilcox_p_1tail
wilcox_p_1tail
wilcox_p_2tail
#3-c
wilcox.test(verizon$CLEC,verizon$ILEC,alternative = "greater")

#4-a
norm_qq_plot <- function(values){
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values,prob=probs1000)
  q_norm <- qnorm(probs1000,mean(values),sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles",ylim=c(min(values)-5,max(values)+5))
  abline( a=0,b=1, col="red", lwd=2)
  
}
#4-b
set.seed(978234)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

plot(density(d123))
norm_qq_plot(d123)

#4-c
plot(density(verizon$ILEC))
norm_qq_plot(verizon$ILEC)
plot(density(verizon$CLEC[!is.na(verizon$CLEC)]))
norm_qq_plot(verizon$CLEC[!is.na(verizon$CLEC)])
