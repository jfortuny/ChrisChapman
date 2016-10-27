# 9.1.1 #######################################################################
cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

# blind approach
spend.m1 <- lm(online.spend ~ ., data = subset(cust.df[,-1], online.spend > 0))
summary(spend.m1)

library(gpairs)
gpairs(cust.df[,-1])

library(car)
scatterplotMatrix(cust.df[,-1], diagonal = "histogram")
