#Q1
#1-a
pnorm(-3.7)
#1-b
2500000*pnorm(-3.7)

#Q2
#2-a
#(i)
data<-read.csv("verizon.csv")
data<-data$Time
plot(density(data),lwd=2,col="blue")
abline(v=mean(data),lty="dashed")
#(ii)
#To test if the 7.6 minutes claimed by Verizon is wrong.
#(iii)
mean<-mean(data)
mean
se<-sd(data)/sqrt(length(data))
interval_99<-mean+2.58*c(-se,se)
interval_99
#(iv)
ver_hyp<-7.6
mean_sample<-mean(data)
mean_sample
sd_sample<-sd(data)
t_stat<-(mean_sample-ver_hyp)/(sd_sample/sqrt(length(data)))
t_stat
p_val<-1-pt(t_stat,df=length(data)-1)
p_val
#(vi)
#Since the p-value,which means the probability that the distribution of mean lay outside the 95% confidence interval,
#is lower than 5%,we can't say the claim by Verizon is wrong
#By T-statistics, we can only prove something is not right

#2-b
num_boots<-2000
#(i)
boot<-function(sample){
  resample<-sample(sample,length(sample),replace = TRUE)
  return (mean(resample))
  }
boot<-replicate(num_boots,boot(data))
interval_99<-quantile(boot,c(0.025,0.975))
interval_99
#(ii)
ver_hyp<-7.6
boot<-function(sample,mean_hyp){
  resample<-sample(sample,length(sample),replace = TRUE)
  return (mean(resample)-mean_hyp)
}
boot_diff<-replicate(num_boots,boot(data,ver_hyp))
diff_interval_99<-quantile(boot_diff,prob=c(0.025,0.975))
diff_interval_99
#(iii)
boot_t<-function(sample,mean_hyp){
  resample<-sample(sample,length(sample),replace = TRUE)
  diff<-mean(resample)-mean_hyp
  se<-sd(resample)/sqrt(length(resample))
  return(diff/se)
}
boot_t<-replicate(num_boots,boot_t(data,ver_hyp))
t_interval_99<-quantile(boot_t,probs=c(0.025,0.975))
t_interval_99
#(iv)
plot(density(boot),col="blue",lwd=2)
abline(v=mean(boot))
abline(v=interval_99,lty="dashed")

plot(density(boot_diff),col="blue",lwd=2)
abline(v=mean(boot_diff))
abline(v=diff_interval_99,lty="dashed")

plot(density(boot_t),col="blue",lwd=2)
abline(v=mean(boot_t))
abline(v=t_interval_99,lty="dashed")

#2-c

