# Helper function to factorize a binary variable
numberToFactor = function(y,key)
{
  index = y<mean(key[,2]) # specific to binary classification
  y[index] = key[1,1]
  y[!index] = key[2,1]
  return(y)
}