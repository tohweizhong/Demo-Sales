
# https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(caret)

Xtt <- read.csv("student-mat.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

Xtt$G3_cate <- unlist(sapply(Xtt$G3, function(x){
    if(x <= 10) return("Fail")
    else return("Pass")
}))

# Should be factors
be_factors <- c("Medu", "Fedu", "famrel", "goout", "Dalc", "Walc", "health")
for(i in seq(ncol(Xtt))){
    if(colnames(Xtt)[i] %in% be_factors){
        Xtt[,i] <- factor(Xtt[,i])
    }
}


# ====

tr_idx <- createDataPartition(Xtt$G3_cate, p = 0.5, list = FALSE)
Xtrain <- Xtt[tr_idx,]
Xtest <- Xtt[-tr_idx,]

ytrain <- Xtrain$G3_cate
ytest <- Xtest$G3_cate

Xtrain <- subset(Xtrain, select = -c(G1, G2, G3, G3_cate, failures))
Xtest <- subset(Xtest, select = -c(G1, G2, G3, G3_cate, failures))

Xtrain_small <- Xtrain[sample(seq(nrow(Xtrain)), 100),]

# ====

tr0 <- tree(data = cbind(Xtrain, ytrain), ytrain~., control = tree.control(nobs = nrow(Xtrain), minsize = 5))
plot(tr0)
text(tr0)


tr0 <- rpart(data = cbind(Xtrain, ytrain), ytrain~.)
plot(tr0)
text(tr0)



tr0_pred_class <- predict(tr0, newdata = Xtest, type = "class")
print(tr0_tb <- table(tr0_pred_class, ytest))
print(tr0_acc <- sum(diag(tr0_tb)) / sum(tr0_tb))

tr0_pred_prob <- predict(tr0, newdata = Xtest, type = "prob")
print(tr0_auc <- auc(response = ytest, predictor = tr0_pred_prob[,2]))


glm0 <- glm(data = cbind(Xtrain, ytrain), ytrain ~., family = binomial(link = "logit"))
summary(glm0)


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