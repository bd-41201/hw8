### HW 8 Script
# Using project data (loan stats from Lending Club) to dig into trees and random forests

## read in the data
loanstats <- read.csv("loanstats_trimmed.csv")
# This is the "cleaned" data from the final project so some columns have been removed

## Part 1 - Fit a tree using CART and then prune. Plot the results for each step.
library(tree)
# We use the tree library for this

# Right now we must remove the addr_state field because the tree function cannot
# handle factors with more than 32 levels. We'll explore some methods around this
# later.
loan.tree <- tree(int_rate ~.-addr_state, data=loanstats)

# Plot to see the results
# png('loanstats_tree_unpruned.png')
par(mfrow=c(1,1))
plot(loan.tree, col=8)
text(loan.tree, digits=2,pretty=TRUE)
title("Loan Rates Tree - Unpruned")
# dev.off()

# Try to see if we can induce a more complex tree
loan.tree.2 <- tree(int_rate ~.-addr_state, data=loanstats, mindev=0.005)
# type = uniform sets the branch height to be the same for each step
# png('complex_tree_unpruned_scaled.png')
plot(loan.tree.2, col=8,type="uniform")
text(loan.tree.2, digits=2,pretty=TRUE)
title("Loan Rates Tree (Complex)- Unpruned")
# dev.off()

## Use cross-validation to prune the tree
loan.tree.2.cv <- cv.tree(loan.tree.2, K=100)
# Show the output of running cross validation on the tree
loan.tree.2.cv$size
loan.tree.2.cv$dev
# It appears that the minimum OOS deviance occurs at a size of 6 leaves and does
# not improve for additional leaves
# Confirm with a plot
# png('oos_deviance_plot.png')
plot(loan.tree.2.cv, pch=21, bg=8, type="p", cex=1.5)

# Prune the complex tree by passing paramete best=6 and plot
loan.tree.2.cut <- prune.tree(loan.tree.2, best=6)
# png('loanstats_tree_pruned.png')
plot(loan.tree.2.cut, col=8)
text(loan.tree.2.cut, digits=2, pretty=TRUE)
# dev.off()

## Q2 - Perform a random forest analysis on the same data
library(randomForest)
loanstats.rf <- randomForest(y=loanstats$int_rate, x=loanstats[,c(1:2,4:9,11:18)], ntree=250, nodesize=1000, importance=TRUE)
## variable importance plot. Add type=1 to plot % contribution to MSE
# png('rf_variable_importance.png')
varImpPlot(loanstats.rf,  type=1, pch=21, bg="navy", main='RF variable importance')

# Plot of predictive performance for tree vs random forest
MSE <- list(CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(loanstats), 5000)

  rt <- tree(int_rate ~.-addr_state, data=loanstats[train,])
  yhat.rt <- predict(rt, newdata=loanstats[-train,])
  MSE$CART <- c( MSE$CART, var(loanstats$int_rate[-train] - yhat.rt))

  rf <- randomForest(y=loanstats$int_rate[train],x=loanstats[train,c(1:2,4:9,11:18)], ntree=250, nodesize=1000)
  yhat.rf <- predict(rf, newdata=loanstats[-train,c(1:2,4:9,11:18)])
  MSE$RF <- c( MSE$RF, var(loanstats$int_rate[-train] - yhat.rf) )

  cat(i)
}

# Plot the results
# png('cart_vs_rf_mse.png')
boxplot(MSE, col=rainbow(2), xlab="model", ylab="MSE")
# dev.off()

## Bonus - Estimate causal treatment effects using random forests
library(gamlr)
# from the projec file the naive value of beta on total credit lines is -0.00040
source("../Utility Scripts/naref.R")
mmloans <- sparse.model.matrix(int_rate ~ ., data=naref(loanstats))[,-1]
y <- loanstats$int_rate
# a new model matrix excluding total_acc
xmm <- mmloans[, -grep("total_acc",colnames(mmloans))]
# replicate for the random forest
x <- loanstats[,c(1:2,4:9,11:17)]
## pull total_acc out as a separate vector (treatment)
d <- loanstats[, 18]

## step 1: fit the random forest for Total Credit Lines on x.
## said another way, predict total_acc from all other covariates
treat.rf <- randomForest(y=d,x=x, ntree=250, nodesize=1000)
## grab predicted dhat from fitted betas
dhat.rf <- predict(treat.rf)
## compare d vs. dhat
plot(dhat.rf,d,bty="n",pch=21,bg=8,main="Total Credit Lines Treatment Effect")
## calculate in sample R-squared for the d on x regression
cor(drop(dhat.rf),d)^2
# ~> [1] 0.5366338

## step 2: using the gamma lasso algorithm from class, we just put dhat into
## the model without any penalty (using the free argument).
## Re-run lasso to estimate independent effect of Total Credit Lines
causal <- gamlr(cBind(d,dhat.rf,xmm),y,free=2)
## calculate beta
coef(causal)["d",]
# ~> [1] -0.000375905
# So we see there is a very small causal effect that each additional credit account
# decreases your expected interest rate by 0.038%
