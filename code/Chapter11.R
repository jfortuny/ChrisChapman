# 11.2 ########################################################################
#
load("~/Rwork/ChrisChapman/data/Chapter05.RData")
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