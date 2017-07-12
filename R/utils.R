####### Helper functions 

# Convert nobs by ntree matrix of regression predictions to nobs vector
collapseRegPred = function(predNum)
{
	pred = rowMeans(predNum,na.rm=T)
	return(pred)
}

# Convert nobs by ntree matrix of probabilities to nobs vector
collapseProbPred = function(predProb,key)     
{
	if (is.null(key)) stop('key is NULL')
    # Probabilities from binary classification
	pred = rowMeans(predProb,na.rm=T)
	pred = probToFactor(pred,key)
}

# Convert nobs by ntree matrix of raw probabilities (still on scale of 1 to 2) to nobs vector 
collapseRawProbPred = function(predRawProb,key)     
{
    if (is.null(key)) stop('key is NULL')
    # Raw probabilities from binary classification
	predProb = predRawProb - 1    # cppForest returns values between 1 and 2 (because of the key value) for binary classification
	return(collapseProbPred(predProb,key))
}

# Convert nobs by ntree matrix of classes to nobs vector
collapseClassPred = function(predClass)     
{
	pred = unlist(apply(predClass,1,function(x) names(sort(table(x),decreasing=T)[1])))   # table ignores NA
}
  
## Helper function to get key to numerize a binary factor variable
# first (alphabetically-speaking) level becomes 1, second level becomes 2
getKey = function(y)
{
    tmp = dim(y)
    y_uniq = unique(as.vector(y))
    if (length(y_uniq)>2) stop('y should have only 2 categories')

	key = data.frame(y_uniq,c(1,2))
    key[,1] = as.character(key[,1])
    return(key)
}

## Helper function to factorize a binary numeric variable
numberToFactor = function(y,key)
{
  if (is.null(key)) stop('key is NULL')
  tmp = dim(y)
  if (is.null(tmp)) {
  	index = y<mean(key[,2]) # specific to binary classification
  	y[index] = key[1,1]
  	y[!index] = key[2,1]
  } else 
  {
  	rows = tmp[1]
  	for (i in 1:rows) {
  		index = y[i,]<mean(key[,2]) # specific to binary classification
  		y[i,index] = key[1,1]
  		y[i,!index] = key[2,1]
  	}
  }
  return(y)
}

## Helper function to numerize a binary factor variable
# See getKey function above for mapping
factorToNumber = function(y,key)
{
  if (is.null(key)) stop('key is NULL')
  tmp = dim(y)
  if (is.null(tmp)) {
    if (class(y)=="factor") y = as.character(y)
  	index = y==key[1,1] 
  	y[index] = key[1,2]
  	y[!index] = key[2,2]
  	y = as.numeric(y)
  } else 
  {
  	rows = tmp[1]
  	if (class(y)=="factor") y = matrix(as.character(y),nrow=rows)
  	for (i in 1:rows) {
  		index = y[i,]==key[1,1]
  		y[i,index] = key[1,2]
  		y[i,!index] = key[2,2]
  	}
    y = matrix(as.numeric(y),nrow=rows)
  }
  return(y)
}

## Helper function to convert probabilities to classes
probToFactor = function(y,key)
{
  if (is.null(key)) stop('key is NULL')
  # Probabilities from binary classification
  tmp = dim(y)
  if (is.null(tmp)) {
  	index = y<0.5 # specific to binary classification
  	y[index] = key[1,1]
  	y[!index] = key[2,1]
  } else 
  {
  	rows = tmp[1]
  	for (i in 1:rows) {
  		index = y[i,]<0.5 # specific to binary classification
  		y[i,index] = key[1,1]
  		y[i,!index] = key[2,1]
  	}
  }
  return(y)
}

## Helper function to convert raw probabilities (still on scale of 1 to 2) to classes
rawProbToFactor = function(y,key)
{
  if (is.null(key)) stop('key is NULL')
  # Raw probabilities from binary classification
  yProb = y - 1    # cppForest returns values between 1 and 2 (because of the key value) for binary classification
  return(probToFactor(yProb,key))
}