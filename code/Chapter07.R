# 7.1 #########################################################################
# use data from book's web site
sat.df<-read.csv("http://goo.gl/HKnl74")
str(sat.df)

# 7.1.1 #######################################################################
# Simulating the amusement park data


# 7.2 #########################################################################
summary(sat.df)

# 7.2.1 #######################################################################
library(gpairs)
gpairs(sat.df)

library(car)
scatterplotMatrix(sat.df, diagonal = "histogram")

# correct skewed distance
sat.df$logdist<-log(sat.df$distance)
scatterplotMatrix(
  formula = ~ weekend + num.child + logdist + rides + games + wait + clean + overall,
  data = sat.df,
  diagonal = "histogram"
)

# verify correlation, particularly amongst ratings
library(corrplot)
corrplot.mixed(cor(sat.df[,c(2, 4:9)]), upper = "ellipse")

# 7.2.3 #######################################################################
lm(overall ~ rides, data = sat.df)

# 7.2.4 #######################################################################
m1<-lm(overall ~ rides, data = sat.df)
plot(overall~rides, data = sat.df)
abline(m1, col="blue")

str(m1)
m1$coefficients
summary(m1)
confint(m1)

# 7.2.5 #######################################################################
# Checking Model Fit:
# 1. The relationship between outcome and predictors is linear
# 2. Errors are normally distributed with a zero mean
plot(m1$fitted.values, m1$residuals)
#    If there's a pattern, transforms the predictors
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

# 7.3 #########################################################################
m2<-lm(overall ~ rides + games + wait + clean, data = sat.df)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

library(coefplot)
coefplot(
  m2,
  intercept = FALSE,
  outerCI = 1.96,
  lwdOuter = 1.5,
  ylab = "Rating of Feature",
  xlab = "Association with Overall Satisfaction"
)

# 7.3.1 #######################################################################
# compare models
summary(m1)$r.squared
summary(m2)$r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

plot(
  sat.df$overall,
  fitted(m1),
  col = "red",
  xlim = c(0, 100),
  ylim = c(0, 100),
  xlab = "Actual Overall Satisfaction",
  ylab = "Fitted Overall Satisfaction"
)
points(sat.df$overall, fitted(m2), col = "blue")
legend(
  "topleft",
  legend = c("model 1", "model 2"),
  col = c("red", "blue"),
  pch = 1
)

anova(m1, m2)
