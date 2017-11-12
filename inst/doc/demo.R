## ------------------------------------------------------------------------
library(surfin)
library(devtools)  # to install randomForestCI package from github
library(randomForest)  # to compare forest implementations
library(rpart) # for kyphosis data
library(MASS) # for Boston housing and breast cancer data

## ------------------------------------------------------------------------
#install_github("swager/randomForestCI")
library(randomForestCI)
library(grf)

## ------------------------------------------------------------------------
data(Boston)
x = Boston[,1:(ncol(Boston)-1)]
y = Boston[,ncol(Boston)]
#x = cu.summary[,c("Price","Country","Reliability","Type")]
#y = cu.summary$Mileage
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
fit = forest(xtrain,ytrain,var.type="ustat",B=25,ntree=5000)

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
rf = randomForest(xtrain, ytrain, keep.inbag = TRUE, ntree=5000) 
ij = randomForestInfJack(rf, xtrain, calibrate = TRUE)
head(ij)
plot(ij)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="infjack",ntree=5000)
ij2_train_oob = fit$predicted   # Case (1)
ij2 = forest.varIJ(fit$predictedAll,fit)
head(ij2)
plot(ij2)

## ------------------------------------------------------------------------
rf_train_oob = rf$predicted
plot(ij2_train_oob,rf_train_oob)
lines(ij2_train_oob,ij2_train_oob,lty="dashed")

## ------------------------------------------------------------------------
fit = regression_forest(as.matrix(xtrain),ytrain,num.trees=5000)
tmp = predict(fit,xtrain,estimate.variance = TRUE)
ij_s = data.frame(tmp$predictions,tmp$variance.estimates)
head(ij_s)
plot(ij_s)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="ustat",B=25,ntree=5000)
ustat = forest.varU(fit$predictedAll,fit,separate=TRUE)
head(ustat)
head(ij)
head(ij_s)

## ------------------------------------------------------------------------
varU = vector("numeric")
varIJ = vector("numeric")
nts = seq(1000,7000,1000)
for (nt in nts)
{
  fit = forest(xtrain,ytrain,var.type="ustat",B=25,ntree=nt)
  varU = c(varU,mean(forest.varU(fit$predictedAll,fit)[,2]))
  rf = randomForest(xtrain, ytrain, keep.inbag = TRUE, ntree=nt) 
  varIJ = c(varIJ,mean(randomForestInfJack(rf, xtrain, calibrate = TRUE)[,2]))
}
plot(nts,varU,ylim=c(0,max(varU,varIJ)),cex.axis=0.6,ylab="Mean Est. Variance",xlab="Number of Trees",type="o",cex.lab=0.5)
points(nts,varIJ,col="blue",type="o")
legend("topright",legend=c("U-Stat","IJ"),col=c("black","blue"),lty="solid",cex=0.6)
print(varU)
print(varIJ)

## ------------------------------------------------------------------------
varU = vector("numeric")
varIJ = vector("numeric")
bs = c(10,25,50,100)
for (b in bs)
{
  fit = forest(xtrain,ytrain,var.type="ustat",B=b,ntree=5000)
  varU = c(varU,mean(forest.varU(fit$predictedAll,fit)[,2]))
}
plot(bs,varU,ylim=c(0,max(varU,varIJ)),cex.axis=0.6,ylab="Mean Est. Variance",xlab="B",type="o",cex.lab=0.5)
print(varU)

## ------------------------------------------------------------------------
#data(biopsy)
#x = biopsy[1:(ncol(biopsy)-1)]
#y = biopsy[,ncol(biopsy)]
x = kyphosis[,c("Age","Number","Start")]
y = kyphosis$Kyphosis
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
table(y)

## ------------------------------------------------------------------------
fit = forest(xtrain,ytrain,var.type="ustat",B=50,ntree=5000)
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
fit = forest(xtrain,ytrain,var.type="infjack",ntree=5000)
ij2_train_oob = fit$predicted   # Case (1)
ij2 = forest.varIJ(fit$predictedAll,fit)
head(ij2)
plot(ij2)

## ------------------------------------------------------------------------
rf = randomForest(xtrain,ytrain,keep.forest=TRUE,keep.inbag=TRUE,replace=TRUE,ntree=5000)
rf_train_oob = rf$predicted
table(ij2_train_oob,rf_train_oob)

