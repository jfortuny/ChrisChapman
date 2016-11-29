# 11.2 ########################################################################
#
#load("~/Rwork/ChrisChapman/data/Chapter05.RData")
load("U:/My Documents/R Projects/ChrisChapman/data/Chapter05.RData")
seg.raw <- seg.df
seg.df <- seg.raw[, -7]

summary(seg.df)

# 11.3.1.1 ####################################################################
# Quick Check Function - How are the groups useful to our business needs?
#
seg.summ <- function(data,groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(seg.df, seg.raw$Segment)

# 11.3.2 ######################################################################
# hclust() works only with numeric data; cluster::daisy converts the various data
# types and re-scales them to compute the distance matrix between points/observations
library(cluster)
seg.dist <- daisy(seg.df)
as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method = "complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
# Cophenetic Correlation coefficient (CPCC) calculates distances from the
# dendrogram, which can then be correalted with the distance matrix
# CPCC is interpreted similarly to Pearson's r
cor(cophenetic(seg.hc), seg.dist)

# 11.3.3 ######################################################################
#
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")
seg.hc.segment <- cutree(seg.hc, k=4)
table(seg.hc.segment)

# use seg.summ to evaluate the clusters
seg.summ(seg.df, seg.hc.segment)

plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab = "", xlab = "")
axis(1, at=c(1,2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1,2), labels = levels(seg.df$gender))

# 11.3.4 ######################################################################
#
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender == "Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome == "ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe == "subNo", 0, 1)
summary(seg.df.num)

set.seed(96743)
seg.k <- kmeans(seg.df.num, centers = 4)
seg.summ(seg.df, seg.k$cluster)

boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")

clusplot(seg.df, seg.k$cluster, color = TRUE, shade = TRUE,
         labels = 4, lines = 0, main = "K-means cluster plot")

# 11.4.1 ######################################################################
#
set.seed(04625)
train.prop <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw) * train.prop)
seg.df.train <- seg.raw[train.cases,]
seg.df.test <- seg.raw[-train.cases,]

library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data = seg.df.train))

(seg.nb.class <- predict(seg.nb, seg.df.test))
prop.table(table(seg.nb.class))

library(cluster)
clusplot(seg.df.test[,-7], seg.nb.class,
         color = TRUE, shade = TRUE, labels = 4, lines = 0,
         main = "Nasive Bayes classification, holdout data")

# 11.4.2 ######################################################################
#
set.seed(04625)
train.prop <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw) * train.prop)
seg.df.train <- seg.raw[train.cases,]
seg.df.test <- seg.raw[-train.cases,]

library(randomForest)
set.seed(98040)
(seg.rf <- randomForest(formula = Segment ~ ., data = seg.df.train, ntree = 3000))

library(cluster)
(seg.rf.class <- predict(seg.rf, seg.df.test))
clusplot(seg.df.test[,-7], seg.rf.class,
         color = TRUE, shade = TRUE, labels = 4, lines = 0,
         main = "Random Forest classification, holdout data")

mean(seg.df.test$Segment==seg.rf.class)
table(seg.df.test$Segment, seg.rf.class)

library(mclust)
adjustedRandIndex(seg.df.test$Segment, seg.rf.class)

# 11.4.3 ######################################################################
#
set.seed(98040)
(seg.rf <- randomForest(formula = Segment ~ ., data = seg.df.train, ntree = 3000,
                        importance = TRUE))
importance(seg.rf)
varImpPlot(seg.rf, main = "Variable importance by segment")

# 11.5 ########################################################################
#
set.seed(92118)
train.prop <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df) * train.prop)
sub.df.train <- seg.df[train.cases,]
sub.df.test <- seg.df[-train.cases,]

clusplot(sub.df.train[, -6], sub.df.train$subscribe,
          color = TRUE, shade = TRUE,
          labels = 4, lines = 0, main = "Subscribers clusters, training data")

library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(formula = subscribe ~ ., data = sub.df.train, ntree = 3000))

# to correct the class imbalance we sample more from the small group
set.seed(11954)
(sub.rf <- randomForest(formula = subscribe ~ ., data = sub.df.train,
                        ntree = 3000, sampsize = c(25,25)))

sub.rf.sub <- predict(sub.rf, sub.df.test)
table(sub.rf.sub, sub.df.test$subscribe)

adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)
