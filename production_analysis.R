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
sub.exp <- select(data,"subject","cond","mean")

# create loop that performs t-test on means for /s/ and /sh/ by individual subject
p = 1 
test.output <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(test.output) <- paste(c('subject','tstatistic','pvalue'))
for (i in unique(sub.exp$subject)){
    ttest <- t.test(mean ~ cond, data = sub.exp[sub.exp$subject == i,])
    test.output[p,1] <- i
    test.output[p,2] <- ttest$statistic
    test.output[p,3] <- ttest$p.value
    p = p + 1
    }