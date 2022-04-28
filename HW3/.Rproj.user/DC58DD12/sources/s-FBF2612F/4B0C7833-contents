#1-a
standardized<-function(numbers){
  numbers<-(numbers- mean(numbers))/sd(numbers)
  return (numbers)
}
rnorm<-rnorm(n=1000,mean=940,sd=190)
rnorm_std<-standardized(rnorm)
plot(density(rnorm_std),col="blue",lwd=2)
#1-b
minday_std<-standardized(minday)
plot(density(minday_std), col="green", lwd=2)
mean(minday_std)
sd(minday_std)
#2
visualize_sample_ci <- function(num_samples=100, sample_size=100, pop_size=10000,distr_func=rnorm,...) {
  # Simulate a large population
  population_data <- rnorm(n=pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, 
                       sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                 sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                 sample_means[bad], bad, good=FALSE)
  
  # Draw true population mean
  abline(v=mean(population_data))
}
add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                         c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}
#2-a-ii
visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, distr_func = rnorm)
#2-b
visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000,distr_func = rnorm)

#3-a-i
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)
mean(minday)
sd(minday)
interval<-mean(minday)+c(-1.96,1.96)*sd(minday)/sqrt(length(minday))
interval
#3-a-ii
minday_size=300
minday2=sample(minday,minday_size)
minday_mean=mean(minday)
resamples<-replicate(2000,sample(minday2 ,length(minday2), replace=TRUE))
resamples
#3-a-iii
plot(density(minday),lwd=0,ylim=c(0, 0.009), main="population vs. bootstrapped samples")
plot_resample_density<-function(sample_i) {
    lines(density(sample_i), col=rgb(0.0, 0.4, 0.0, 0.01))
    return(mean(sample_i))
}    
sample_means<-apply(resamples, 2, FUN=plot_resample_density)
lines(density(minday2),lwd=3)
lines(density(minday),lwd=2,lty="dashed")
#3-a-iv
interval<-function(data){
  return (c(mean(data)-1.96*sd(data),mean(data)+1.96*sd(data)))
}
bootstrap_interval<-sapply(resamples,interval)
bootstrap_interval                                    
#3-b-i
median(minday)
#3-b-ii
boostrap<-sapply(resamples,median)
boostrap
#3-b-iii
interval<-function(data){
  return (c(mean(data)-1.96*sd(data),mean(data)+1.96*sd(data)))
}
bootstrap_interval<-sapply(boostrap,interval)
bootstrap_interval

