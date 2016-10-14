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

