## ------------------------------------------------------------------------
library(rpart) # for kyphosis data
library(randomForest)
library(surfin)

## ------------------------------------------------------------------------
x = kyphosis[,c("Age","Number","Start")]
y = kyphosis$Kyphosis

## ------------------------------------------------------------------------
fit = forest(x,y,var.type="ustat",B=10)
names(fit)
fit$predicted
head(forest.varU(fit))
predict(fit,x)

## ------------------------------------------------------------------------
fit = forest(x,y,var.type="infjack")
names(fit)
output1 = fit$predicted
head(forest.varIJ(fit))
predict(fit,x)

## ------------------------------------------------------------------------
fit = forest(x,y,replace=FALSE)
names(fit)
fit$predicted
predict(fit,x)

## ------------------------------------------------------------------------
fit = randomForest(x,y,keep.forest=TRUE,keep.inbag=TRUE,replace=TRUE)
output2 = fit$predicted
table(output1,output2)

## ------------------------------------------------------------------------
x = cu.summary[,c("Price","Country","Reliability","Type")]
y = cu.summary$Mileage
keep = !is.na(y)
y = y[keep]
x = x[keep,]
keep = !apply(is.na(x),1,any)
y = y[keep]
x = x[keep,]

## ------------------------------------------------------------------------
fit = forest(x,y,var.type="ustat",B=10)
names(fit)
fit$predicted
head(forest.varU(fit))
predict(fit,x)

## ------------------------------------------------------------------------
fit = forest(x,y,var.type="infjack")
names(fit)
output1 = fit$predicted
head(forest.varIJ(fit))
predict(fit,x)

## ------------------------------------------------------------------------
fit = forest(x,y,replace=FALSE)
names(fit)
fit$predicted
predict(fit,x)

## ------------------------------------------------------------------------
fit = randomForest(x,y,keep.forest=TRUE,keep.inbag=TRUE,replace=TRUE)
output2 = as.numeric(fit$predicted)
plot(output1,output2)
lines(output1,output1,lty="dashed")

