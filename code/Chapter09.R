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
# apply the transformation to all columns
cust.df.bc[, numcols] <- lapply(cust.df.bc[, numcols], autoTransform)
summary(cust.df.bc)

library(car)
scatterplotMatrix(cust.df.bc, diagonal = "histogram")

spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2)

# 9.1.2 #######################################################################
library(car)
vif(spend.m2)

# eliminate highly correlated variables
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans, data = cust.df.bc)
summary(spend.m4)
vif(spend.m4)

# use PCA to replace correlated variables (use first PC)
pc.online <- prcomp(cust.df.bc[, c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[, 1]
pc.store <- prcomp((cust.df.bc[, c("store.trans", "store.spend")]))
cust.df.bc$store <- pc.store$x[, 1]
head(cust.df.bc)
spend.m5 <- lm(online.spend ~ email + age + credit.score + distance.to.store +
                 sat.service + sat.selection + online + store,
               data =  cust.df.bc)
summary(spend.m5)
vif(spend.m5)
