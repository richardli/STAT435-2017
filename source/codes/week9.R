# ---- 1
calif <- read.table("https://raw.githubusercontent.com/jbryer/CompStats/master/Data/cadata.dat", header = TRUE)
dim(calif)
head(calif)
# ---- 2
par(bg = 'gray50')
plot(calif$Longitude,calif$Latitude,pch=21,
	col=terrain.colors(11)[1+floor(calif$MedianHouseValue/50e3)],
	bg=terrain.colors(11)[1+floor(calif$MedianHouseValue/50e3)],
	cex=sqrt(calif$Population/median(calif$Population)),
	xlab="Longitude",ylab="Latitude",main="Median House Prices",
	sub="Circle area proportional to population")
legend(x="topright",legend=(50*(1:11)),fill=terrain.colors(11))

# ---- 3
library(tree)
treefit <- tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)
par(bg = 'white')
plot(treefit)
text(treefit,cex=0.75)
# ---- 4
par(bg = 'gray50')
plot(calif$Longitude,calif$Latitude,pch=21,
	col=terrain.colors(11)[1+floor(calif$MedianHouseValue/50e3)],
	bg=terrain.colors(11)[1+floor(calif$MedianHouseValue/50e3)],
	cex=sqrt(calif$Population/median(calif$Population)),
	xlab="Longitude",ylab="Latitude",main="Median House Prices",
	sub="Circle area proportional to population")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)
# ---- 5
par(bg = 'white')	
price.deciles <- quantile(calif$MedianHouseValue,0:10/10)
cut.prices <- cut(calif$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)

# ---- 6
summary(treefit)

# ---- 7
treefit2 <- tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif, 
				mindev = 0.002)
summary(treefit2)

# ---- 8
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treefit2,ordvars=c("Longitude","Latitude"),add=TRUE)


# ---- 9
treeprun5 <- prune.tree(treefit2, best=5)
plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treeprun5,ordvars=c("Longitude","Latitude"),add=TRUE)

# ---- 10
treeprun <- prune.tree(treefit2)
plot(treeprun)

# ---- 11
treecv <- cv.tree(treefit2)
plot(treecv)

# ---- 12
opt.trees <- which(treecv$dev == min(treecv$dev))
best.leaves <- min(treecv$size[opt.trees])
best.leaves 
treefit3 <- prune.tree(treefit2, best=best.leaves)

plot(calif$Longitude,calif$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
	xlab="Longitude",ylab="Latitude")
partition.tree(treefit3,ordvars=c("Longitude","Latitude"),add=TRUE)

# ---- 13
boxplot(calif$MedianHouseValue ~ exp(predict(treefit3, newdata = calif)))

# ---- 14
library(randomForest)
set.seed(1)
train <- sample(1:nrow(calif), nrow(calif)/2)

# ---- 15
treefit0 <- tree(log(MedianHouseValue) ~ ., data=calif, subset = train)
treecv <-  cv.tree(treefit0) 
opt.trees <- which(treecv$dev == min(treecv$dev))
best.leaves <- min(treecv$size[opt.trees])
treefit <- prune.tree(treefit0, best=best.leaves)

# ---- 16
bagfit <- randomForest(log(MedianHouseValue) ~ ., data=calif, subset = train,
					 	mtry = ncol(calif)-1)
bagfit

rffit <- randomForest(log(MedianHouseValue) ~ ., data=calif, subset = train,
					 	mtry = 3)
rffit

# ---- 17
y <- log(calif[-train, "MedianHouseValue"])
yhat.tree <- predict(treefit, newdata=calif[-train ,])
yhat.bag <- predict(bagfit, newdata=calif[-train ,])
yhat.rf <- predict(rffit, newdata=calif[-train ,])
c(mean((yhat.tree-y)^2), mean((yhat.bag-y)^2), mean((yhat.rf-y)^2))
