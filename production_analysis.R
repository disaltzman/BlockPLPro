rm(list = ls(all = TRUE))

library(lme4)
library(ggplot2)
library(plyr)
library(dplyr)
library(R.utils)

theme_set(theme_bw())

data <- data <- as.data.frame(read.delim(file='blockPLPRo_acoustic_analysis.txt', sep='\t'))

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
