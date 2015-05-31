impute.rf <- function(data, mtry=(length(data)-2)/2, ...) {
#z <- data$z;  data$z <- NULL;
#y <- data$y;  data$y <- NULL;
#  <   cntrl <- fastRF( x=data[z==0,], y=y[z==0], mtry=mtry)
#  <   treat <- fastRF( x=data[z==1,], y=y[z==1], mtry=mtry)
#  <   return(mean(rfPredict(treat, data) - rfPredict(cntrl, data)))
}