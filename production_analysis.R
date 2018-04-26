rm(list = ls(all = TRUE))

library(lme4)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(R.utils)

theme_set(theme_bw())

data <- as.data.frame(read.delim(file='blockPLPRo_acoustic_analysis.txt', sep='\t'))

# create distribution figures
# individual subject mean
ggplot(data, aes(x=data$mean,fill=data$cond))+
    geom_density(alpha=.5,position="identity") +
    facet_wrap(~subject)
    
# mean
cdat <- ddply(data, "cond2", summarise, rating.mean=mean(mean))

ggplot(data, aes(x=data$mean,fill=data$cond2))+
  geom_density(alpha=.5,position="identity") + 
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=cond2),
             linetype="dashed", size=1)
# sd
ggplot(data, aes(x=data$sd,fill=data$cond2))+
  geom_density(alpha=.5,position="identity")

# kurtosis
ggplot(data, aes(x=data$kurtosis,fill=data$cond2))+
  geom_density(alpha=.5,position="identity")

# dur
ggplot(data, aes(x=data$dur,fill=data$cond2))+
  geom_density(alpha=.5,position="identity")

# skew
ggplot(data, aes(x=data$skew,fill=data$cond2))+
  geom_density(alpha=.5,position="identity")

####################################################

# calculate average of four cues across productions for each participant for /s/ and /sh/
subjectaverages <- aggregate(data, by=list(data$subject,data$cond),FUN=mean, na.rm=TRUE, drop=TRUE)

# subset data to remove variables we're not interested in
sub.exp <- select(data,"subject","cond","mean","sd","kurtosis","skew")

# loop that performs t-test on means for /s/ and /sh/ by individual subject
p = 1 
test.output.mean <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.output.mean) <- paste(c('subject','tstatistic','pvalue'))
for (i in unique(sub.exp$subject)){
    ttest <- t.test(mean ~ cond, data = sub.exp[sub.exp$subject == i,])
    test.output.mean[p,1] <- i
    test.output.mean[p,2] <- ttest$statistic
    test.output.mean[p,3] <- ttest$p.value
    p = p + 1
}

# loop that performs t-test on standard deviation for /s/ and /sh/ by individual subject
p = 1 
test.output.sd <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.output.sd) <- paste(c('subject','tstatistic','pvalue'))
for (i in unique(sub.exp$subject)){
  ttest <- t.test(sd ~ cond, data = sub.exp[sub.exp$subject == i,])
  test.output.sd[p,1] <- i
  test.output.sd[p,2] <- ttest$statistic
  test.output.sd[p,3] <- ttest$p.value
  p = p + 1
}

# loop that performs t-test on kurtosis for /s/ and /sh/ by individual subject
p = 1 
test.output.kurtosis <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.output.kurtosis) <- paste(c('subject','tstatistic','pvalue'))
for (i in unique(sub.exp$subject)){
  ttest <- t.test(kurtosis ~ cond, data = sub.exp[sub.exp$subject == i,])
  test.output.kurtosis[p,1] <- i
  test.output.kurtosis[p,2] <- ttest$statistic
  test.output.kurtosis[p,3] <- ttest$p.value
  p = p + 1
}


# loop that performs t-test on skew for /s/ and /sh/ by individual subject
p = 1 
test.output.skew <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.output.skew) <- paste(c('subject','tstatistic','pvalue'))
for (i in unique(sub.exp$subject)){
  ttest <- t.test(skew ~ cond, data = sub.exp[sub.exp$subject == i,])
  test.output.skew[p,1] <- i
  test.output.skew[p,2] <- ttest$statistic
  test.output.skew[p,3] <- ttest$p.value
  p = p + 1
}

# t-test to see if centroid mean changes as function of order
ttest <- t.test(mean ~ cond2, data = )