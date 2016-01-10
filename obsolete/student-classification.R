
# student-classification.R

# https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(caret)
library(rpart)
library(rpart.plot)
library(pROC)

Xtt <- read.csv("data/student-mat.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

# Create two response variables, G2_cate and G3_cate
Xtt$G3_cate <- unlist(sapply(Xtt$G3, function(x){
    if(x <= 10) return("Fail")
    else return("Pass")
}))

Xtt$G2_cate <- unlist(sapply(Xtt$G2, function(x){
    if(x <= 10) return("Fail")
    else return("Pass")
}))

Xtt$G1_cate <- unlist(sapply(Xtt$G1, function(x){
    if(x <= 10) return("Fail")
    else return("Pass")
}))


# Should be factors
be_factors <- c("Medu", "Fedu", "famrel", "goout", "Dalc", "Walc", "health", "G3_cate", "G2_cate", "G1_cate")
for(i in seq(ncol(Xtt))){
    if(colnames(Xtt)[i] %in% be_factors){
        Xtt[,i] <- factor(Xtt[,i])
    }
}

# ====
# create three datasets, pre-G1, between G1 and G2, and G2 and G3
# Xtrain_G3, Xtest_G3
# Xtrain_G2, Xtest_G2
# Xtrain_G1, Xtest_G1

tr_idx          <- createDataPartition(Xtt$G3_cate, p = 0.5, list = FALSE)
Xtrain_G3       <- Xtt[ tr_idx,]
Xtest_G3        <- Xtt[-tr_idx,]
ytrain_G3       <- Xtrain_G3$G3_cate
ytest_G3        <- Xtest_G3$G3_cate
Xtrain_G3       <- subset(Xtrain_G3, select = -c(G3, G3_cate, failures))
Xtest_G3        <- subset(Xtest_G3, select = -c(G3, G3_cate, failures))
Xtrain_G3_small <- Xtrain_G3[sample(seq(nrow(Xtrain_G3)), 100),]

tr_idx          <- createDataPartition(Xtt$G2_cate, p = 0.5, list = FALSE)
Xtrain_G2       <- Xtt[ tr_idx,]
Xtest_G2        <- Xtt[-tr_idx,]
ytrain_G2       <- Xtrain_G2$G2_cate
ytest_G2        <- Xtest_G2$G2_cate
Xtrain_G2       <- subset(Xtrain_G2, select = -c(G2, G2_cate, G3, G3_cate))
Xtest_G2        <- subset(Xtest_G2, select = -c(G2, G2_cate, G3, G3_cate))
Xtrain_G2_small <- Xtrain_G2[sample(seq(nrow(Xtrain_G2)), 100),]

tr_idx          <- createDataPartition(Xtt$G1_cate, p = 0.5, list = FALSE)
Xtrain_G1       <- Xtt[ tr_idx,]
Xtest_G1        <- Xtt[-tr_idx,]
ytrain_G1       <- Xtrain_G1$G1_cate
ytest_G1        <- Xtest_G1$G1_cate
Xtrain_G1       <- subset(Xtrain_G1, select = -c(G1, G1_cate, G2, G2_cate, G3, G3_cate))
Xtest_G1        <- subset(Xtest_G1, select = -c(G1, G1_cate, G2, G2_cate, G3, G3_cate))
Xtrain_G1_small <- Xtrain_G1[sample(seq(nrow(Xtrain_G1)), 100),]


save(list = c("Xtrain_G3", "Xtest_G3", "ytrain_G3", "ytest_G3", "Xtrain_G3_small"), file = "data/G3.RData")
save(list = c("Xtrain_G2", "Xtest_G2", "ytrain_G2", "ytest_G2", "Xtrain_G2_small"), file = "data/G2.RData")
save(list = c("Xtrain_G1", "Xtest_G1", "ytrain_G1", "ytest_G1", "Xtrain_G1_small"), file = "data/G1.RData")

# ====
# Decision tree (pre-G1)

tr0_G1 <- rpart(data = cbind(Xtrain_G1, ytrain_G1), ytrain_G1 ~.)
rpart.plot(tr0_G1)

tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
print(tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1))
print(tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb))

tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
print(tr0_G1_auc <- auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2]))

# ====
# Decision tree (G1 -> G2)

tr0_G2 <- rpart(data = cbind(Xtrain_G2, ytrain_G2), ytrain_G2 ~.)
rpart.plot(tr0_G2)

tr0_G2_pred_class <- predict(tr0_G2, newdata = Xtest_G2, type = "class")
print(tr0_G2_tb <- table(tr0_G2_pred_class, ytest_G2))
print(tr0_G2_acc <- sum(diag(tr0_G2_tb)) / sum(tr0_G2_tb))

tr0_G2_pred_prob <- predict(tr0_G2, newdata = Xtest_G2, type = "prob")
print(tr0_G2_auc <- auc(response = ytest_G2, predictor = tr0_G2_pred_prob[,2]))

# ====
# Decision tree (G1, G2 -> G3)

tr0_G3 <- rpart(data = cbind(Xtrain_G3, ytrain_G3), ytrain_G3 ~.)
rpart.plot(tr0_G3)

tr0_G3_pred_class <- predict(tr0_G3, newdata = Xtest_G3, type = "class")
print(tr0_G3_tb <- table(tr0_G3_pred_class, ytest_G3))
print(tr0_G3_acc <- sum(diag(tr0_G3_tb)) / sum(tr0_G3_tb))

tr0_G3_pred_prob <- predict(tr0_G3, newdata = Xtest_G3, type = "prob")
print(tr0_G3_auc <- auc(response = ytest_G3, predictor = tr0_G3_pred_prob[,2]))





# ====


rf0 <- randomForest(x = Xtrain, y = ytrain, ntree = 1000, importance = TRUE, proximity = TRUE)

plot(rf0$mse, type = "l", lwd = 3, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")


rf0_pred_class <- predict(rf0, newdata = Xtest, type = "class")
print(rf0_tb <- table(rf0_pred_class, ytest))
print(rf0_acc <- sum(diag(rf0_tb)) / sum(rf0_tb))

tr0_pred_prob <- predict(tr0, newdata = Xtest, type = "prob")
print(tr0_auc <- auc(response = ytest, predictor = tr0_pred_prob[,2]))



# ====

glm0 <- glm(data = cbind(Xtrain, ytrain), ytrain ~., family = binomial(link = "logit"))
summary(glm0)


# ====

cvglmnet0 <- cv.glmnet(x = Xtrain, y = ytrain, nfolds = 1, type.measure = "mse")

# ====

tr_ctrl <- trainControl(method = "boot", number = 5,
                        verboseIter = TRUE, returnData = FALSE)



tr0 <- train(x = Xtrain, y = ytrain, method = "lasso", tuneLength = 5,
             trControl = tr_ctrl)



library(caret)

tr_ctrl <- trainControl(method = "boot", number = 5,
                        verboseIter = TRUE, returnData = FALSE)

tg_tr0 <- expand.grid(cp = seq(0, 25, by = 1))


tr0 <- train(x = Xtrain, y = ytrain, method = "rpart",
             tuneGrid = tg_tr0, trControl = tr_ctrl)