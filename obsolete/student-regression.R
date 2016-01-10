
# student-regression.R

# https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(Metrics)

Xtt <- read.csv("data/student-mat.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

# Should be factors
be_factors <- c("Medu", "Fedu", "famrel", "goout", "Dalc", "Walc", "health")
for(i in seq(ncol(Xtt))){
    if(colnames(Xtt)[i] %in% be_factors){
        Xtt[,i] <- factor(Xtt[,i])
    }
}

# Engineer feature for G3MinusG2 (improvements)
Xtt$G3MinusG2 <- Xtt$G3 - Xtt$G2

# ====
# create three datasets, pre-G1, between G1 and G2, and G2 and G3
# Xtrain_G3, Xtest_G3
# Xtrain_G2, Xtest_G2
# Xtrain_G1, Xtest_G1

# subset students who failed G2
Xtt2 <- Xtt[which(Xtt$G2 < 10),]

tr_idx          <- createDataPartition(Xtt2$G3MinusG2, p = 0.7, list = FALSE)
Xtrain_G3       <- Xtt2[ tr_idx,]
Xtest_G3        <- Xtt2[-tr_idx,]
ytrain_G3       <- Xtrain_G3$G3MinusG2
ytest_G3        <- Xtest_G3$G3MinusG2
Xtrain_G3       <- subset(Xtrain_G3, select = -c(G3, G3MinusG2))
Xtest_G3        <- subset(Xtest_G3, select = -c(G3, G3MinusG2))

tr_idx          <- createDataPartition(Xtt$G2, p = 0.5, list = FALSE)
Xtrain_G2       <- Xtt[ tr_idx,]
Xtest_G2        <- Xtt[-tr_idx,]
ytrain_G2       <- Xtrain_G2$G2
ytest_G2        <- Xtest_G2$G2
Xtrain_G2       <- subset(Xtrain_G2, select = -c(G2, G3))
Xtest_G2        <- subset(Xtest_G2, select = -c(G2, G3))
Xtrain_G2_small <- Xtrain_G2[sample(seq(nrow(Xtrain_G2)), 100),]

tr_idx          <- createDataPartition(Xtt$G1, p = 0.5, list = FALSE)
Xtrain_G1       <- Xtt[ tr_idx,]
Xtest_G1        <- Xtt[-tr_idx,]
ytrain_G1       <- Xtrain_G1$G1
ytest_G1        <- Xtest_G1$G1
Xtrain_G1       <- subset(Xtrain_G1, select = -c(G1, G2, G3))
Xtest_G1        <- subset(Xtest_G1, select = -c(G1, G2, G3))
Xtrain_G1_small <- Xtrain_G1[sample(seq(nrow(Xtrain_G1)), 100),]


save(list = c("Xtrain_G3", "Xtest_G3", "ytrain_G3", "ytest_G3", "Xtrain_G3_small"), file = "data/G3.RData")
save(list = c("Xtrain_G2", "Xtest_G2", "ytrain_G2", "ytest_G2", "Xtrain_G2_small"), file = "data/G2.RData")
save(list = c("Xtrain_G1", "Xtest_G1", "ytrain_G1", "ytest_G1", "Xtrain_G1_small"), file = "data/G1.RData")

# ====
# Decision tree (pre-G1)

tr0_G1 <- rpart(data = cbind(Xtrain_G1, ytrain_G1), ytrain_G1 ~.)
rpart.plot(tr0_G1)

tr0_G1_pred <- predict(tr0_G1, newdata = Xtest_G1)
rmse(actual = ytest_G1, predicted = tr0_G1_pred)


# ====
# Decision tree (G1 -> G2)

tr0_G2 <- rpart(data = cbind(Xtrain_G2, ytrain_G2), ytrain_G2 ~.)
rpart.plot(tr0_G2)

tr0_G2_pred <- predict(tr0_G2, newdata = Xtest_G2)
rmse(actual = ytest_G2, predicted = tr0_G2_pred)


# ====
# Decision tree (G1, G2 -> G3)

tr0_G3 <- rpart(data = cbind(Xtrain_G3, ytrain_G3), ytrain_G3 ~.)
rpart.plot(tr0_G3)

tr0_G3_pred <- predict(tr0_G3, newdata = Xtest_G3)
rmse(actual = ytest_G3, predicted = tr0_G3_pred)


# ====
# Regression (G3 improvements)

lm0_G3 <- lm(data = cbind(Xtrain_G3, ytrain_G3), ytrain_G3 ~.)
summary(lm0_G3)

lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
rmse(actual = ytest_G3, predicted = lm0_G3_pred)


anova(lm0_G3)
plot(ytrain_G3 ~ Xtrain_G3$absences)
# ====



rf0_G3 <- randomForest(data = cbind(Xtrain_G3, ytrain_G3), ytrain_G3 ~.)

rf0_G3_pred <- predict(rf0_G3, newdata = Xtest_G3)
rmse(actual = ytest_G3, predicted = lm0_G3_pred)










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