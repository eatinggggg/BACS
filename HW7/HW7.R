#1-(a)
m1<-read.csv("pls-media1.csv")$INTEND.0
m2<-read.csv("pls-media2.csv")$INTEND.0
m3<-read.csv("pls-media3.csv")$INTEND.0
m4<-read.csv("pls-media4.csv")$INTEND.0
mean1<-mean(m1)
mean2<-mean(m2)
mean3<-mean(m3)
mean4<-mean(m4)
mean_grand<-mean(c(mean1,mean2,mean3,mean4))
mean1
mean2
mean3
mean4


#1-(b)
par(mar = c(2, 2, 2, 2))
par(mfrow=c(2,2))    # set the plotting area into a 1*2 array
plot(density(m1),main ="media 1",lwd=2)
abline(v=mean(m1),col="blue",lwd=2)
plot(density(m2),main ="media 2",lwd=2)
abline(v=mean(m2),col="blue",lwd=2)
plot(density(m3),main ="media 3",lwd=2)
abline(v=mean(m3),col="blue",lwd=2)
plot(density(m4),main ="media 4",lwd=2)
abline(v=mean(m4),col="blue",lwd=2)
#Q1-c
#From the figure above,we can see that means of these media are quite close to each other
#while the distributions look quite different.Therefore, I will conclude that media do make difference on ontention to share


#Q2-a
#null:the means of four media are the same
#hypo:the means of four media are not the same
#2-(b)-i
length(m1)
SSTR<-length(m1)*((mean1-mean_grand)^2)+length(m2)*((mean2-mean_grand)^2)+length(m3)*((mean3-mean_grand)^2)+length(m4)*((mean4-mean_grand)^2)
df_MSTR<-4-1
MSTR<-SSTR/df_MSTR
MSTR
SSE<-(length(m1)-1)*var(m1)+(length(m2)-1)*var(m2)+(length(m3)-1)*var(m3)+(length(m4)-1)*var(m4)
df_MSE<-length(m1)+length(m2)+length(m3)+length(m4)-4
MSE<-SSE/df_MSE
MSE
F<-MSTR/MSE
F
#2-(b)-ii
qf(p=0.95,df_MSTR,df_MSE)
p_val<-pf(F,df_MSTR,df_MSE,lower.tail = FALSE)
p_val
#2-(c)
m1<-data.frame(m1)
m2<-data.frame(m2)
m3<-data.frame(m3)
m4<-data.frame(m4)
library(reshape2)
media<-melt(c(m1,m2,m3,m4),id.vars=NULL,variable.name="media",value.name="score")
media
anova_model<-aov(media$score~factor(media$L1))
summary(anova_model)
#2-(d)
TukeyHSD(anova_model,conf.level = 0.05)
#2-(e)
#No,by the results shown in 1-a,we can clearly see that the variance of the four group is quite different.Also, by the visualized figure,the curve in the destribution of media 1,3,4 do not follow the normal distribution.

#3-(a)
#null:numbers of for group are the same
#hypo:numbers of for group are not the same
#3-(b)-i
c(m1,m2,m3,m4)
score_rank<-rank(media$score)
score_rank
ranked_groups<-split(score_rank,media$L1)
R1<-sum(ranked_groups$m1)
R2<-sum(ranked_groups$m2)
R3<-sum(ranked_groups$m3)
R4<-sum(ranked_groups$m4)
length(m1)
N<-length(m1)+length(m2)+length(m3)+length(m4)
N
H<-(12*((R1^2/length(m1))+(R2^2/length(m2))+(R3^2/length(m3))+(R4^2/length(m4)))/(N*(N+1)))-3*(N+1)
H
#3-(b)-ii
kw_p<-1-pchisq(H,df=3)
kw_p
#since p_value<0.05,therefore we can reject H0.Numbers in the four groups are not the same
#3-(c)
kruskal.test(score~factor(L1),data=media)
#3-(d)
#install.packages("FSA")
library(FSA)
dunnTest(score~factor(L1),data=media)
c(var(m1),var(m2),var(m3),var(m4))
