
# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

library(caret)

Xtt <- read.csv("bank-additional-full.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

# ====
# Mock-up data for pushed product
# 70% short term and 30% long term

num_short <- round(nrow(Xtt) * 0.7)
num_long <- nrow(Xtt) - num_short

short <- sample(seq(nrow(Xtt)), size = num_short)
long <- setdiff(seq(nrow(Xtt)), short)

Xtt$shortlong <- NA_character_
Xtt$shortlong[short] <- "Short"
Xtt$shortlong[long] <- "Long"
Xtt$shortlong <- factor(Xtt$shortlong)

# ====

nine <- which(Xtt$pdays == 999)
Xtt$pdays[nine] <- NA

# remove some columns
Xtt <- subset(Xtt, select = -c(nr.employed, pdays, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, poutcome))

# ====

tr_idx <- createDataPartition(Xtt$y, p = 0.5, list = FALSE)
Xtrain <- Xtt[tr_idx,]
Xtest <- Xtt[-tr_idx,]


ytrain <- Xtrain$y
ytest <- Xtest$y

Xtrain <- subset(Xtrain, select = -c(y, duration))
Xtest <- subset(Xtest, select = -c(y, duration))

Xtrain_small <- Xtrain[sample(seq(nrow(Xtrain)), 230),]





# rewrite file with different separator
#write.csv(Xtt, row.names = FALSE, file = "foo.csv")

# ====
# clustering

hc <- hclust(dist(Xtrain_small))
plot(hc)

km <- kmodes(Xtrain_small, modes = 5)
clusplot(Xtrain_small, km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# ====


tr0 <- tree(data = cbind(Xtrain, ytrain), ytrain~., control = tree.control(nobs = nrow(Xtrain), minsize = 5))
plot(tr0)
text(tr0)


tr0<- rpart(data = cbind(Xtrain, ytrain), ytrain~., control = tree.control(nobs = nrow(Xtrain), minsize = 5))
plot(tr0)
text(tr0)



tr0_pred_class <- predict(tr0, newdata = Xtest, type = "class")
print(tr0_tb <- table(tr0_pred_class, ytest))
print(tr0_acc <- sum(diag(tr0_tb)) / sum(tr0_tb))

tr0_pred_prob <- predict(tr0, newdata = Xtest, type = "prob")
print(tr0_auc <- auc(response = ytest, predictor = tr0_pred_prob[,2]))

# ====

glm0 <- glm(data = cbind(Xtrain, ytrain), ytrain ~., family = binomial(link = "logit"))
summary(glm0)

plot(glm0)


rf0 <- randomForest(x = Xtrain, y = ytrain, ntrees = 300, importance = TRUE, na.action = na.omit)
varImpPlot(rf0)
