################################
##Draw roc curve with rocr lib##
####################################################################################################
##  	predictions: vector,matrix which contains predictions
##		labels: vector, matrix which contains true class labels. Must dimensions like predictions
##		order: which is negative and which positive example: c(0, 1);
####################################################################################################
draw_roc_curve = function(roc_predictions, roc_labels, roc_order = c(0, 1))
{
  predicted_model <- prediction(roc_predictions, roc_labels, label.ordering = roc_order) ##create prediction model
  perf <- performance(predicted_model, measure = "tpr", x.measure = "fpr") 
  plot(perf)
}