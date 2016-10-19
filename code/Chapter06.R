# 6.1 #########################################################################
# use data from chapter 5
load("U:\\My Documents\\R Projects\\ChrisChapman\\data\\Chapter05.RData")
#load("C:\\Users\\Jose\\Documents\\Rwork\\ChrisChapman\\data\\Chapter05.RData")
summary(seg.df)

# 6.2 #########################################################################
# Group Frequencies - chisqr.test
chisq.test(table(seg.df$Segment)) # are segment sizes significantly different?
# is Subscription status independent of Home ownership?
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim = TRUE, B = 10000)

# 6.3.1 #######################################################################
# Proportions - binom.test

# 6.4 #########################################################################
hist(seg.df$income)
with(seg.df, hist(income[ownHome=="ownYes"]))
with(seg.df, hist(income[ownHome=="ownNo"]))
t.test(income~ownHome, data = seg.df)
t.test(income~ownHome, data = subset(seg.df, Segment=="Travelers"))

# 6.5 #########################################################################
seg.aov.own<-aov(income~ownHome, data = seg.df)
anova(seg.aov.own)
seg.aov.own<-aov(income~Segment, data = seg.df)
anova(seg.aov.own)
# with both descriptors
anova(aov(income~Segment+ownHome, data = seg.df))
# with both descriptors and interaction
anova(aov(income~Segment*ownHome, data = seg.df))

# 6.5.1 #######################################################################
# comparing models
anova(aov(income~Segment, data = seg.df),
      aov(income~Segment+ownHome, data = seg.df))

# 6.5.1 #######################################################################
library(multcomp)
seg.aov<-aov(income~Segment, data = seg.df)
glht(seg.aov)

# 6.5.2 #######################################################################
seg.aov<-aov(income~-1+Segment, data = seg.df)
glht(seg.aov)
par(mar=c(6,10,2,2))
plot(glht(seg.aov),
     xlab="Income",
     main="Average Income by Segment (95% CI)")

# 6.5.3 #######################################################################
seg.aov.step<-step(aov(income~., data = seg.df))
anova(seg.aov.step)

# 6.6.2 #######################################################################
library(BayesFactor)
set.seed(96761)
seg.bf1<-lmBF(income~Segment, data = seg.df)
seg.bf2<-lmBF(income~Segment+ownHome, data = seg.df)
seg.bf1/seg.bf2
