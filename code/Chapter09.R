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

# auto transform variables
autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}
# select complete cases and drop customer Id
cust.df.bc <- cust.df[complete.cases(cust.df), -1]
# select cases with online spend
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
# create a vector to index columns except email
numcols <- which(colnames(cust.df.bc) != "email")
