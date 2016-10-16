# 4.1.1 #######################################################################
set.seed(21821)
ncust<-1000
cust.df<-data.frame(cust.id=as.factor(c(1:ncust)))
cust.df$age<-rnorm(n = ncust, mean = 35, sd = 5)
cust.df$credit.score<-rnorm(n = ncust, mean = 3*cust.df$age+620, sd = 50)
cust.df$email<-factor(sample(x = c("yes","no"), size = ncust, replace = TRUE, prob = c(0.8, 0.2)))
cust.df$distance.to.store<-exp(rnorm(n = ncust, mean = 2, sd = 1.2))
summary(cust.df)

# 4.1.2 #######################################################################
cust.df$online.visits<-rnbinom(ncust, size = 0.3, mu = 15 + ifelse(cust.df$email=="yes", 15, 0)
                               - 0.7 * (cust.df$age - median(cust.df$age)))
cust.df$online.trans<-rbinom(ncust, size = cust.df$online.visits, prob = 0.3)
cust.df$online.spend<-exp(rnorm(ncust, mean = 3, sd = 0.1)) * cust.df$online.trans
cust.df$store.trans<-rnbinom(ncust, size = 5, mu = 3 / sqrt(cust.df$distance.to.store))
cust.df$store.spend<-exp(rnorm(ncust, mean = 3.5, sd = 0.4)) * cust.df$store.trans

# 4.1.3 #######################################################################
sat.overall<-rnorm(ncust, mean = 3.1, sd = 0.7)
summary(sat.overall)
sat.service<-floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection<-floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))
summary(cbind(sat.service, sat.selection))
sat.service[sat.service > 5]<-5
sat.service[sat.service < 1]<-1
sat.selection[sat.selection > 5]<-5
sat.selection[sat.selection < 1]<-1
summary(cbind(sat.service, sat.selection))

# 4.1.3 #######################################################################
no.response<-as.logical(rbinom(ncust, size = 1, prob = cust.df$age/100))
sat.service[no.response]<-NA
sat.selection[no.response]<-NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service<-sat.service
cust.df$sat.selection<-sat.selection
summary(cust.df)

# 4.2.1 #######################################################################
str(cust.df)
plot(cust.df$age, cust.df$credit.score)
plot(cust.df$age, cust.df$credit.score,
     col="blue",
     xlim=c(15,55),
     ylim=c(500,900),
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)",
     ylab="Customer Credit Score")
abline(h=mean(cust.df$credit.score),
       col="dark blue",
       lty="dotted")
abline(v=mean(cust.df$age),
       col="dark blue",
       lty="dotted")

plot(cust.df$store.spend, cust.df$online.spend,
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales($)",
     ylab = "Prior 12 months online sales($)",
     cex = 0.7)
hist(
  cust.df$store.spend,
  breaks = (0:ceiling(max(cust.df$store.spend) / 10)) * 10,
  main = "Customers as of June 2014",
  xlab = "Prior 12 months in-store sales($)",
  ylab = "Count of Customers"
)

# 4.2.2 #######################################################################
my.col<-c("black", "green3")
my.pch<-c(1, 19)
plot(cust.df$store.spend, cust.df$online.spend,
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales($)",
     ylab = "Prior 12 months online sales($)",
     cex = 0.7,
     col = my.col[cust.df$email],
     pch = my.pch[cust.df$email])

# 4.2.3 #######################################################################
legend(x="topright",
       legend = paste("email on file: ", levels(cust.df$email)),
       col = my.col,
       pch =  my.pch)

# 4.2.4 #######################################################################
plot(cust.df$store.spend + 1, cust.df$online.spend + 1,
     log = "xy",
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales($)",
     ylab = "Prior 12 months online sales($)",
     cex = 0.7,
     col = my.col[cust.df$email],
     pch = my.pch[cust.df$email])
legend(x="topright",
       legend = paste("email on file: ", levels(cust.df$email)),
       col = my.col,
       pch =  my.pch)

# 4.3 #########################################################################
par(mfrow=c(2,2))
plot(cust.df$distance.to.store, cust.df$store.spend, main = "Store")
plot(cust.df$distance.to.store, cust.df$online.spend, main = "Online")
plot(cust.df$distance.to.store+1, cust.df$store.spend+1, log="xy", main = "Store (log)")
plot(cust.df$distance.to.store+1, cust.df$online.spend+1, log="xy", main = "Online (log)")
par(mfrow=c(1,1))

# 4.4.1 #######################################################################
pairs(
  formula =  ~ age + credit.score + email + distance.to.store + online.visits +
    online.trans + online.spend + store.trans + store.spend,
  data = cust.df
)
pairs(cust.df[,2:10])

# 4.4.2 #######################################################################
library(car)
scatterplotMatrix(
  formula =  ~ age + credit.score + email + distance.to.store + online.visits +
    online.trans + online.spend + store.trans + store.spend,
  data = cust.df,
  diagonal = "histogram"
)

library(gpairs)
gpairs(cust.df[,2:10])

# 4.5 #########################################################################
cov(cust.df$age, cust.df$credit.score)
cor(cust.df$age, cust.df$credit.score)
# Cohen's rules of thumb for "people's variables" correlation (assuming normality of the variables:
# r <= 0.1  weak
# r = 0.3   medium
# r >= 0.5  high

# 4.5.1 #######################################################################
cor.test(cust.df$age, cust.df$credit.score)

# 4.5.2 #######################################################################
cor(cust.df[, c(2, 3, 5:12)])
library(corrplot)
library(gplots)
corrplot.mixed(
  corr = cor(cust.df[, c(2, 3, 5:12)],
             use = "complete.obs"),
  upper = "ellipse",
  tl.pos = "lt",
  col = colorpanel(50, "red", "gray60", "blue4")
)

# 4.5.3 #######################################################################
plot(cust.df$distance.to.store, cust.df$store.trans)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.trans)

# 4.5.5 #######################################################################
library(car)
powerTransform(cust.df$distance.to.store)

lambda<-coef(powerTransform(1/cust.df$distance.to.store))
bcPower(cust.df$distance.to.store,lambda)

par(mfrow=c(1,2))
hist(cust.df$distance.to.store,
     xlab = "Distance to Nearest Store",
     ylab = "Count of Customers",
     main = "Original Distribution")
hist(bcPower(cust.df$distance.to.store,lambda),
     xlab = "Box-Cox transform of Distance to Nearest Store",
     ylab = "Count of Customers",
     main = "Transformed Distribution")
