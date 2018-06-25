covariates = read.table("Covariates.txt")

#returns a percentage of missing value per row or column
#axis = 1 = rows, 2 = columns
missing_detector = function(data_table, axis)
{
  if(axis == 1)
  {
    n = nrow(data_table)
    missing_proportion = sapply(1:n, function(i)sum(is.na(data_table[i,])))/ncol(data_table)
  }
  else
  {
    n = ncol(data_table)
    missing_proportion = sapply(1:n, function(i)sum(is.na(data_table[,i])))/nrow(data_table)
  }
  return(missing_proportion)
}
  
missing_detector(covariates, axis = 1)
missing_detector(covariates, axis = 2)

#removes the lines/columns (according to axis) whose NA proportion is higher than a threshold
#threshold = the proportion of NA above which the 
missing_remover = function(data_table, axis, threshold)
{
  proportions = missing_detector(data_table, axis)
  if(axis == 1)
  {
    show(paste(sum(proportions>threshold), " individuals were removed with threshold ", threshold))
    show(row.names(data_table)[proportions>threshold])
    return(data_table[proportions<threshold,])
  }
  if (axis == 2)
  {
    show(paste(sum(proportions>threshold), " individuals were removed with threshold ", threshold))
    show(colnames(data_table)[proportions>threshold])
    return(data_table[,proportions<threshold])
  }
}


#little examples
missing_detector(covariates, 1)
missing_remover(covariates, 1, 0.5)
missing_remover(covariates, 1, 0.2)



