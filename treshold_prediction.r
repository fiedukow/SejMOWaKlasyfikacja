#############################
## Naive threshold methods ##
#############################

#############################################################################
## This ususes given threshold to predict whether deputy has changed party ##
#############################################################################
##    column_to_use_for_predict: name of column to use threshold for.
##
##    threshold: threshold to apply. Every value below it is
##               considered as false. True otherwise.
##
##    data: data with deputies to apply threshold
#############################################################################
predict_naive_threshold = function(column_to_use_for_predict, threshold, data){
  data_column = data[column_to_use_for_predict]
  result = ifelse(data_column[column_to_use_for_predict] > threshold, TRUE, FALSE)
  return(result)
}

## example usage: 
#naive_threshold_result = predict_naive_threshold("own_dst", 0.02, poslowieMeta)

##################################################################################
## This counts given quantile of provided column data, and uses it as threshold ##
##################################################################################
##    column_to_use_for_predict: Name of column to use threshold for.
##
##    quantile_rank: Rank of quantile (form 0 to 1), to use as threshold.
##
##    data: Data with deputies to apply threshold
##################################################################################
predict_naive_threshold_quantile = function(column_to_use_for_predict, quantile_rank, data){
  data_column = data[column_to_use_for_predict]
  ordered = data_column[order(data_column[,1]),]
  index = round(length(ordered)*quantile_rank)+1
  return(predict_naive_threshold(column_to_use_for_predict, ordered[index], data))
}

## example usage:
#naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 0.5, poslowieMeta) # quantile of rank 0.5 which is median