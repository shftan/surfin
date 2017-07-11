## Helper function to get key to numerize a binary factor variable
# first (alphabetically-speaking) level becomes -1, second level becomes 1
getKey = function(y)
{
    tmp = dim(y)
    y_uniq = unique(as.vector(y))
    if (length(y_uniq)>2) stop('y should have only 2 categories')

	key = data.frame(y_uniq,c(-1,1))
    key[,1] = as.character(key[,1])
    return(key)
}

## Helper function to factorize a binary numeric variable
numberToFactor = function(y,key)
{
  tmp = dim(y)
  if (is.null(tmp)) {
  	index = y<mean(key[,2]) # specific to binary classification
  	y[index] = key[1,1]
  	y[!index] = key[2,1]
  }
  else {
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
# first (alphabetically-speaking) level becomes -1, second level becomes 1
factorToNumber = function(y,key)
{
  tmp = dim(y)
  if (is.null(tmp)) {
    if (class(y)=="factor") y = as.character(y)
  	index = y==key[1,1] 
  	y[index] = key[1,2]
  	y[!index] = key[2,2]
  	y = as.numeric(y)
  }
  else {
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

## Helper function to convert probabilities to categories
probToFactor = function(y,key)
{
  tmp = dim(y)
  if (is.null(tmp)) {
  	index = y<0.5 # specific to binary classification
  	y[index] = key[1,1]
  	y[!index] = key[2,1]
  }
  else {
  	rows = tmp[1]
  	for (i in 1:rows) {
  		index = y[i,]<0.5 # specific to binary classification
  		y[i,index] = key[1,1]
  		y[i,!index] = key[2,1]
  	}
  }
  return(y)
}