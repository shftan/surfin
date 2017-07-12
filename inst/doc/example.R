## ------------------------------------------------------------------------
library(surfin)
library(devtools)  # to install randomForestCI package from github
library(randomForest)  # to compare forest implementations
library(rpart) # for kyphosis data

## ------------------------------------------------------------------------
install_github("swager/randomForestCI")
library(randomForestCI)

## ------------------------------------------------------------------------
x = cu.summary[,c("Price","Country","Reliability","Type")]
y = cu.summary$Mileage
keep = !is.na(y)
y = y[keep]
x = x[keep,]
keep = !apply(is.na(x),1,any)
y = y[keep]
x = x[keep,]
n = length(y)
train = sample(1:n,n*0.7)
test = setdiff(1:n,train)
xtrain = x[train,]
ytrain = y[train]
xtest = x[test,]
ytest = y[test]

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="ustat",B=10)

## ------------------------------------------------------------------------
names(fit)

## ------------------------------------------------------------------------
u_train_oob = fit$predicted        # Case (1)
u_train = predict(fit,xtrain)  # Case (2)
temp = predict(fit,xtest,individualTrees=T)   # Case (3)
u_test = temp$predicted
u_test_all = temp$predictedAll
temp = data.frame(u_train_oob,u_train)
head(temp)
head(u_test)

## ------------------------------------------------------------------------
ustat = forest.varU(u_test_all,fit)
head(ustat)
plot(ustat)

## ------------------------------------------------------------------------
temp = forest.varU(u_test_all,fit,covariance=TRUE)
y.hat = temp[[1]]
cov = temp[[2]]
dim(cov)
cov[1:6,1:6]

## ------------------------------------------------------------------------
unique(diag(cov) - ustat[,2])

## ------------------------------------------------------------------------
ustat = forest.varU(fit$predictedAll,fit)
head(ustat)
plot(ustat)

## ------------------------------------------------------------------------
rf = randomForest(xtrain, ytrain, keep.inbag = TRUE) 
ij = randomForestInfJack(rf, xtrain, calibrate = TRUE)
head(ij)
plot(ij)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="infjack")
ij2_train_oob = fit$predicted   # Case (1)
ij2 = forest.varIJ(fit$predictedAll,fit)
head(ij2)
plot(ij2)

## ------------------------------------------------------------------------
rf_train_oob = rf$predicted
plot(ij2_train_oob,rf_train_oob)
lines(ij2_train_oob,ij2_train_oob,lty="dashed")

## ------------------------------------------------------------------------
x = kyphosis[,c("Age","Number","Start")]
y = kyphosis$Kyphosis
n = length(y)
train = sample(1:n,n*0.7)
test = setdiff(1:n,train)
xtrain = x[train,]
ytrain = y[train]
xtest = x[test,]
ytest = y[test]

## ------------------------------------------------------------------------
table(y)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="ustat",B=10)
names(fit)
u_train_oob = fit$predicted        # Case (1)
table(u_train_oob)
u_train = predict(fit,xtrain)  # Case (2)
table(u_train)
temp = predict(fit,xtest,individualTrees=T)   # Case (3)
u_test = temp$predicted
u_test_prob = temp$predictedProb
u_test_all = temp$predictedAll
table(u_test)

## ------------------------------------------------------------------------
ustat = forest.varU(u_test_all,fit)
head(ustat)
plot(ustat)

## ------------------------------------------------------------------------
ustat = forest.varU(u_test_prob,fit)
head(ustat)
plot(ustat)

## ------------------------------------------------------------------------
ustat = forest.varU(fit$predictedAll,fit)
head(ustat)
plot(ustat)
ustat = forest.varU(fit$predictedProb,fit)
head(ustat)
plot(ustat)

## ------------------------------------------------------------------------
#rf = randomForest(x, y, keep.inbag = TRUE)
#ij = randomForestInfJack(rf, x, calibrate = TRUE)
#head(ij)
#plot(ij)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="infjack")
ij2_train_oob = fit$predicted   # Case (1)
ij2 = forest.varIJ(fit$predictedAll,fit)
head(ij2)
plot(ij2)

## ------------------------------------------------------------------------
rf = randomForest(xtrain,ytrain,keep.forest=TRUE,keep.inbag=TRUE,replace=TRUE)
rf_train_oob = rf$predicted
table(ij2_train_oob,rf_train_oob)

