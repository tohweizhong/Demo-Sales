
library(kernlab)
library(caret)
library(cluster)

data(spam)
Xtt <- spam

tr_idx <- createDataPartition(spam$type, p = 0.5, list = FALSE)
Xtrain <- spam[tr_idx,]
Xtest <- spam[-tr_idx,]

ytrain <- Xtrain$type
ytest <- Xtest$type

Xtrain <- subset(Xtrain, select = -type)
Xtest <- subset(Xtest, select = -type)

Xtrain_small <- Xtrain[sample(seq(nrow(Xtrain)), 230),]

hc <- hclust(dist(Xtrain_small))
plot(hc)

km <- kmeans(Xtrain_small, centers = 2)
clusplot(Xtrain_small, km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
