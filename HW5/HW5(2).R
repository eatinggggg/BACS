
#2
#2-a-i
data<-read.csv("verizon.csv")$Time
hyp_mean<-7.6
t.test(data,mu=hyp_mean,alternative="less",conf.level=0.99)
#2-a-ii
power.t.test(n=length(data),delta=mean(data)-hyp_mean,sd=sd(data),sig.level = 0.01,alternative ="one.sided")
#2-b-i
t_val<-(mean(data)-hyp_mean)/(sd(data)/sqrt(length(data)))
t_val
#2-b-ii
bootstrap_null_alt<-function(sample0,hyp_mean){
  resample<-sample(sample0,length(sample0),replace = T)
  resample_se<-sd(resample)/sqrt(length(resample))
  alt<-(mean(resample)-hyp_mean)/resample_se
  null<-(mean(resample)-mean(sample0))/resample_se
  c(alt,null)
}
bootstrap_data<-replicate(15000,bootstrap_null_alt(data,hyp_mean))
t_alt<-bootstrap_data[1,]
t_null<-bootstrap_data[2,]
plot(density(t_alt),col="blue",xlim=range(t_null,t_alt))
lines(density(t_null),lty="dashed")
#2-a-iii
cutoff_99<-quantile(t_null,probs=0.99)#用數據集去取95%值,而非ecdf(quantile函數可用於numeric vector)
cutoff_99
#2-b-iv
#compute null probability distribution
null_prob<-ecdf(t_null)#the cumulative prob dist for t_null data
p_val<-1-null_prob(t_val)
p_val
#compute alternative probability distribution and 
alt_prob<-ecdf(t_alt)
power<-1-alt_prob(cutoff_99)
power

