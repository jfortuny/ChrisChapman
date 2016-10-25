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
