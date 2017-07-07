## ------------------------------------------------------------------------
library(rpart) # for kyphosis data
library(randomForest)
library(surfin)
library(devtools)

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

## ---------------------------------------------------------------------------
fit = forest(x,y,var.type="ustat",B=10)

## ---------------------------------------------------------------------------
names(fit)

## ---------------------------------------------------------------------------
fit$predicted

## ---------------------------------------------------------------------------
predict(fit,x)

## ---------------------------------------------------------------------------
ustat = forest.varU(fit)
head(ustat)
plot(ustat)

## ---------------------------------------------------------------------------
rf = randomForest(x, y, keep.inbag = TRUE)
ij = randomForestInfJack(rf, x, calibrate = TRUE)
head(ij)
plot(ij)

## ---------------------------------------------------------------------------
fit = forest(x,y,var.type="infjack")
names(fit)
pred_forest = fit$predicted
pred_forest
predict(fit,x)
ij2 = forest.varIJ(fit)
head(ij2)
plot(ij2)

## ---------------------------------------------------------------------------
fit = forest(x,y,replace=FALSE)
names(fit)
fit$predicted
predict(fit,x)

## ---------------------------------------------------------------------------
pred_randomForest = rf$predicted
plot(pred_forest,pred_randomForest)
lines(pred_forest,pred_forest,lty="dashed")

## ---------------------------------------------------------------------------
x = kyphosis[,c("Age","Number","Start")]
y = kyphosis$Kyphosis

## ---------------------------------------------------------------------------
fit = forest(x,y,var.type="ustat",B=10)
names(fit)
fit$predicted
head(forest.varU(fit))
predict(fit,x)

## ---------------------------------------------------------------------------
#rf = randomForest(x, y, keep.inbag = TRUE)
#ij = randomForestInfJack(rf, x, calibrate = TRUE)
#head(ij)
#plot(ij)

## ---------------------------------------------------------------------------
fit = forest(x,y,var.type="infjack")
names(fit)
output1 = fit$predicted
head(forest.varIJ(fit))
predict(fit,x)

## ---------------------------------------------------------------------------
fit = forest(x,y,replace=FALSE)
names(fit)
fit$predicted
predict(fit,x)

## ---------------------------------------------------------------------------
fit = randomForest(x,y,keep.forest=TRUE,keep.inbag=TRUE,replace=TRUE)
output2 = fit$predicted
table(output1,output2)

