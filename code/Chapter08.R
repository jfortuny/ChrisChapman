# 8.1 #########################################################################
brand.ratings<-read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
tail(brand.ratings)
summary(brand.ratings)
str(brand.ratings)

# 8.1.1 #######################################################################
brand.sc<-brand.ratings
brand.sc[, 1:9]<-scale(brand.ratings[, 1:9])
summary(brand.sc)

library(corrplot)
corrplot(cor(brand.sc[,1:9]), order = "hclust")

# 8.1.2 #######################################################################
brand.mean<-aggregate(.~brand, data = brand.sc, mean)
brand.mean
rownames(brand.mean)<-brand.mean[,1]
brand.mean<-brand.mean[,-1]

library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col = brewer.pal(9, "GnBu"),
          trace = "none",
          key = FALSE,
          dendrogram = "none",
          main = "\n\n\n\n\nBrand Attributes")

# 8.2.3 #######################################################################
brand.pc<-prcomp(brand.sc[,1:9])
brand.pc
summary(brand.pc)
plot(brand.pc, type = "l")
biplot(brand.pc)

brand.mean.pc<-prcomp(brand.mean, scale. = TRUE)
summary(brand.mean.pc)
plot(brand.mean.pc, type = "l")
biplot(brand.mean.pc)

# 8.2.4 #######################################################################
biplot(brand.mean.pc, main = "Brand Positioning", cex = c(1.5,1))

# 8.3.3 #######################################################################
library(nFactors)
nScree(brand.sc[,1:9])
eigen(cor(brand.sc[,1:9]))

factanal(brand.sc[,1:9], factors = 2)
factanal(brand.sc[,1:9], factors = 3)

library(GPArotation)
(brand.fa.ob<-factanal(brand.sc[,1:9], factors = 3, rotation = "oblimin"))

library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings,
          col = brewer.pal(9, "Greens"),
          trace = "none",
          key = FALSE,
          dendrogram = "none",
          Colv = FALSE,
          cexCol = 1.2,
          main = "\n\n\n\n\nFactor loadings for brand adjectives")

library(semPlot)
semPaths(brand.fa.ob,
         what = "est",
         residuals = FALSE,
         cut=0.3,
         posCol=c("white","darkgreen"), negCol=c("white","red"),
         edge.label.cex = 0.75,
         nCharNodes = 7)

# 8.3.4 #######################################################################
brand.fa.ob<-factanal(brand.sc[,1:9], factors = 3,
                      rotation = "oblimin", scores = "Bartlett")
brand.scores<-data.frame(brand.fa.ob$scores)
brand.scores$brand<-brand.sc$brand
head(brand.scores)

brand.fa.mean<-aggregate(.~brand, data=brand.scores, mean)
rownames(brand.fa.mean)<-brand.fa.mean[,1]
brand.fa.mean<-brand.fa.mean[,-1]
names(brand.fa.mean)<-c("Leader", "Value", "Latest")
brand.fa.mean

library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.fa.mean),
          col = brewer.pal(9, "GnBu"),
          trace = "none",
          key = FALSE,
          dendrogram = "none",
          cexCol = 1.2,
          main = "\n\n\n\n\nMean Factor Score by Brand")
