
# load.R

# packages
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(Metrics)

# data
load("C:/Users/weizhong/Documents/R/Demo-Sales/data/Xtt.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/data/vars.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/data/G1.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/data/G2.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/data/G3.RData")

# models
load("C:/Users/weizhong/Documents/R/Demo-Sales/models/tr0_G1.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/models/tr0_G2.RData")
load("C:/Users/weizhong/Documents/R/Demo-Sales/models/lm0_G3.RData")
