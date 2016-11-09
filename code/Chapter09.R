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

# 9.2.2 #######################################################################
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))
summary(pass.df)

# 9.2.6 #######################################################################
pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = "binomial")
summary(pass.m1)

# odds ratio
plogis(0.38879) / (1 - plogis(0.38879))
exp(0.38879)
# the odds ratio is 1.475 or
# we're increasing the likelihood of purchase by 47.5%
exp(coef(pass.m1))
exp(confint(pass.m1))

# 9.2.7 #######################################################################
table(pass.df$Pass, pass.df$Channel)

library(vcd)
doubledecker(table(pass.df))

pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = "binomial")
summary(pass.m2)
exp(coef(pass.m2))
exp(confint(pass.m2))

pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel,
               data = pass.df, family = "binomial")
summary(pass.m3)
exp(coef(pass.m3))
exp(confint(pass.m3))

# 9.3.2 #######################################################################
conjoint.df <- read.csv("http://goo.gl/G8knGV")
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
summary(conjoint.df)

# 9.3.4 #######################################################################
table(conjoint.df$rating)
by(conjoint.df$rating, conjoint.df$height, mean)


ride.lm <- lm(rating ~ speed + height + const + theme, data = conjoint.df)
summary(ride.lm)

# 9.3.5 #######################################################################
library(lme4)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id),
                  data = conjoint.df)
summary(ride.hlm1)

fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

# 9.3.6 #######################################################################
ride.hlm2 <- lmer(rating ~ speed + height + const + theme +
                    (speed + height + const + theme | resp.id),
                  data = conjoint.df,
                  control = lmerControl(optCtrl = list(maxfun=100000)))
summary(ride.hlm2)

fixef(ride.hlm2)
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)
# sanity check
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196,]
