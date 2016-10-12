# 3.1.1 #######################################################################
k.stores<-20
k.weeks<-104
store.df<-data.frame(matrix(NA, ncol = 10, nrow = k.stores*k.weeks))
names(store.df)<-c("storeNum","Year","Week","p1sales",
                   "p2sales","p1price","p2price",
                   "p1prom","p2prom","country")

store.num<-101:(100+k.stores)
(store.cty<-c(rep("US",3), rep("DE",5), rep("GB",3), rep("BR",2),
              rep("JP",4), rep("AU",1), rep("CN",2)))
store.df$storeNum<-rep(store.num, each=k.weeks)
store.df$country<-rep(store.cty, each=k.weeks)
rm(store.num, store.cty)

(store.df$Week<-rep(1:52, times=k.stores*2))
(store.df$Year<-rep(rep(1:2, each=k.weeks/2), times=k.stores))
str(store.df)

store.df$storeNum<-factor(store.df$storeNum)
store.df$country<-factor(store.df$country)

# 3.1.2
set.seed(98250)
store.df$p1prom<-rbinom(n=nrow(store.df), size=1, p=0.1)
store.df$p2prom<-rbinom(n=nrow(store.df), size=1, p=0.15)
store.df$p1price<-sample(x=c(2.19,2.29,2.49,2.79,2.9), size=nrow(store.df), replace=T)
store.df$p2price<-sample(x=c(2.29,2.49,2.59,2.99,3.19),
                         size=nrow(store.df), replace=T)

# sales count follows a Poisson distribution
tmp.sales1<-rpois(nrow(store.df), lambda=120)
tmp.sales2<-rpois(nrow(store.df), lambda=100)
# relative sales of product are affected by sales of the other
tmp.sales1<-tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2<-tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)
# sales are also affected by promotion
store.df$p1sales<-floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales<-floor(tmp.sales2 * (1 + store.df$p1prom*0.4))

head(store.df)
str(store.df)

# 3.2 #######################################################################
table(store.df$p1price)
table(store.df$p1price, store.df$p1prom)

# 3.3 #######################################################################
summary(store.df)
library(psych)
describe(store.df)

colMeans(store.df[,2:9])
apply(store.df[,2:9], MARGIN = 2, FUN = mean)

# 3.4.1 #######################################################################
hist(store.df$p1sales)
hist(store.df$p1sales,
     main = "Product 1 weekly sales frequency, All Stores",
     xlab = "Product 1 Sales (Units)",
     ylab = "Count")
hist(store.df$p1sales,
     main = "Product 1 weekly sales frequency, All Stores",
     xlab = "Product 1 Sales (Units)",
     ylab = "Count",
     breaks = 30,
     col = "lightblue")

hist(store.df$p1sales,
     main = "Product 1 weekly sales frequency, All Stores",
     xlab = "Product 1 Sales (Units)",
     ylab = "Count",
     breaks = 30,
     col = "lightblue",
     freq = FALSE,
     xaxt = "n")
axis(side = 1, at = seq(60, 300, by=20))
lines(density(store.df$p1sales),
      bw = 10,
      type = "l",
      col = "darkred",
      lwd = 2)

# 3.4.2 #######################################################################
boxplot(store.df$p1sales,
        xlab = "Weekly unit Sales",
        ylab = "P1",
        main = "Weekly Sales of Product 1, All Stores",
        horizontal = TRUE)

boxplot(store.df$p1sales~store.df$storeNum,
        horizontal = TRUE,
        xlab = "Weekly unit Sales",
        ylab = "Store",
        main = "Weekly Sales of Product 1 by Store",
        las = 1)

boxplot(store.df$p1sales~store.df$p1prom,
        horizontal = TRUE,
        yaxt = "n",
        xlab = "Weekly unit Sales",
        ylab = "P1 promoted in Store",
        main = "Weekly Sales of Product 1 with and without promotion")
axis(side = 2, at = c(1,2), labels = c("No", "Yes"))

<<<<<<< HEAD
# 3.4.3 #######################################################################
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

# 3.4.4 #######################################################################
plot(ecdf(store.df$p1sales),
     main = "Cumulative Distribution Function of P1 Weekly Sales",
     ylab = "Cumulative Proportion",
     xlab = c("P1 Weekly Sales, All Stores", "90% of weeks sold <=171 units"),
     yaxt = "n")
axis(side = 2, at = seq(0,1, by=0.1),
     las = 1,
     labels = paste(seq(0,100, by = 10), "%", sep = ""))
abline(h = 0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9),
       lty = 3)

# 3.4.5 #######################################################################
by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)
aggregate(store.df$p1sales, by=list(country=store.df$country, store=store.df$storeNum), sum)

# 3.4.6 #######################################################################
p1sales.sum<-aggregate(store.df$p1sales, by=list(country=store.df$country), sum)
library(rworldmap)
library(RColorBrewer)
p1sales.map<-joinCountryData2Map(p1sales.sum,
                                 joinCode = "ISO2",
                                 nameJoinColumn = "country")
mapCountryData(p1sales.map,
               nameColumnToPlot = "x",
               mapTitle = "Total P1 Sales by Country",
               colourPalette = brewer.pal(7, "Greens"),
               catMethod = "fixedWidth",
               addLegend = FALSE)
=======
>>>>>>> c8dcf56d4df42eac07fa5276641ffa19c063457d
