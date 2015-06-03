## Helper function to factorize a binary numeric variable
numberToFactor = function(y,key)
{
  index = y<mean(key[,2]) # specific to binary classification
  y[index] = key[1,1]
  y[!index] = key[2,1]
  return(y)
}

## Helper function to numerize a binary factor variable
# first (alphabetically-speaking) level becomes 1, second level becomes 2
factorToNumber = function(y)
{
  tmp = dim(y)
  if (is.null(tmp))
  {
    y = as.numeric(y)
  }
  else
  {
    rows = tmp[1]
    y = matrix(as.numeric(factor(y)),nrow=rows)
  }
  return(y)
}