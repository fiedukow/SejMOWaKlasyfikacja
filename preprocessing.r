##Library check
library("party") ##library with decision tree
library("e1071") ##library with naive bayes
library("ROCR") ##library with roc curve

source("statistics_generators.r")

## Loading data
glosowania = read.table("data/glosowania.txt", sep="\t", header=T, row.names=1) 
glosowaniaPartie = read.table("data/party_affiliations.txt", sep="\t", header=T, row.names=1) 
glosowaniaMeta = read.table("data/glosowania_metadata.txt", sep="\t", header=T, row.names=1) 
poslowieMeta_base = read.table("data/poslowie_metadata.txt", sep="\t", header=T, row.names=1) 
poslowieMeta = poslowieMeta_base
glosowania_m = as.matrix(glosowania)
glosowaniaPartie_m = as.matrix(glosowaniaPartie)
glosowaniaPartie_m = replace(glosowaniaPartie_m, which(is.na(glosowaniaPartie_m)), "brak-informacji")

## Modify voting table to meet our numeric interpretation
## 1.0 - for
## 0.5 - held
## 0.0 - againts
glosowania_to_avg = glosowania_m
glosowania_to_avg = replace(glosowania_m, which(glosowania_m == 0), 0.5)
glosowania_to_avg = replace(glosowania_m, which(glosowania_m == -1), 0)

## Find all parties during cadence
partie = unique(c(glosowaniaPartie_m))

#### OUR FIRST PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES
party_vote_distance_own = generateStatistic("party_vote_distance",
                                            glosowania_to_avg,
                                            glosowaniaPartie_m,
                                            partie)
poslowieMeta = cbind(poslowieMeta, own_dst = party_vote_distance_own)

#### OUR SECOND PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES NORMALIZED
#### BY EACH VOTING LOYALITY OF THE PARTY.
avg_party_dsts_normalized = generateStatistic("party_vote_distance_normalized",
                                              glosowania_to_avg,
                                              glosowaniaPartie_m,
                                              partie)
poslowieMeta = cbind(poslowieMeta, own_dst_norm = avg_party_dsts_normalized)

#### ANOTHER PARAMETER IS AVERAGE DISTANCE FROM PARTY MODE VOTES
party_mode_distance_own = generateStatistic("party_mode_distance",
                                            glosowania_to_avg,
                                            glosowaniaPartie_m,
                                            partie)
poslowieMeta = cbind(poslowieMeta, own_mode_dst = party_mode_distance_own)

#### ANOTHER PARAMETER IS AVERAGE LENGHT OF STREAK AGAINST PARTY
party_mode_against_streak_avg = generateStatistic("against_party_mode_average_streak",
                                                  glosowania_to_avg,
                                                  glosowaniaPartie_m,
                                                  partie)
poslowieMeta = cbind(poslowieMeta, avg_streak_against_party = party_mode_against_streak_avg)

#### ANOTHER PARAMETER IS MAX LENGHT OF STREAK AGAINST PARTY
party_mode_against_streak_max = generateStatistic("against_party_mode_max_streak",
                                                  glosowania_to_avg,
                                                  glosowaniaPartie_m,
                                                  partie)
poslowieMeta = cbind(poslowieMeta, max_streak_against_party = party_mode_against_streak_max)

#### CLASS CALCULATING
changed_party = apply(glosowaniaPartie_m, 1, function (r) {
                                               length(unique(r[which(r != "brak-informacji")])) > 1
                                             })
poslowieMeta = cbind(poslowieMeta, party_changed = changed_party)


###############################################
##            BUILDING TESTING DATA          ##
###############################################
half_length = dim(glosowania_to_avg)[2]/2
half_data_avg = glosowania_to_avg[,1:half_length]
half_data_partie = glosowaniaPartie_m[,1:half_length]
half_poslowieMeta = poslowieMeta_base
half_partie = unique(c(half_data_partie))

#### OUR FIRST PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES
half_party_vote_distance_own = generateStatistic("party_vote_distance",
                                                 half_data_avg,
                                                 half_data_partie,
                                                 half_partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_dst = half_party_vote_distance_own)

#### OUR SECOND PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES NORMALIZED
#### BY EACH VOTING LOYALITY OF THE PARTY.
half_avg_party_dsts_normalized = generateStatistic("party_vote_distance_normalized",
                                                   half_data_avg,
                                                   half_data_partie,
                                                   half_partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_dst_norm = half_avg_party_dsts_normalized)

#### ANOTHER PARAMETER IS AVERAGE DISTANCE FROM PARTY MODE VOTES
half_party_mode_distance_own = generateStatistic("party_mode_distance",
                                                 half_data_avg,
                                                 half_data_partie,
                                                 half_partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_mode_dst = half_party_mode_distance_own)

## Removing deputies which already made their choice ;)
changed_in_half = unlist(
                    lapply(
                      apply(half_data_partie, 1, unique),
                      function(x) {
                          (length(x) > 1 && length(which(x == "brak-informacji")) == 0) ||
                          (length(x) > 2)
                      }
                    )
                  )
half_poslowieMeta = half_poslowieMeta[-which(changed_in_half),]

#### BUILDING THE MODEL

##Add another another attributes to +
decisionTree = ctree(party_changed ~ own_dst + own_mode_dst, data = poslowieMeta)

##example usage to predict whether patry has changed:
predict(decisionTree, newdata = half_poslowieMeta) # newdata should be data, which we want to predict party_changed (also it should have computed attributes)

##naive bayes
bayes = naiveBayes(as.factor(party_changed) ~ own_dst + own_mode_dst, data=poslowieMeta) ##predict works same as for decistion tree

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
naive_threshold_result = predict_naive_threshold("own_dst", 0.02, poslowieMeta)

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
naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 0.5, poslowieMeta) # quantile of rank 0.5 which is median

################################
##Draw roc curve with rocr lib##
####################################################################################################
##		predictions: vector,matrix which contains predictions
##		labels: vector, matrix which contains true class labels. Must dimensions like predictions
##		order: which is negative and which positive example: c("0", "1");
####################################################################################################
pref_roc_curve = function(roc_predictions, roc_labels, roc_order)
{
	predicted_model <- prediction(roc_predictions, roc_labels, label.ordering = roc_order) ##create prediction model
	#perf <- performance(predicted_model, measure = "tpr", x.measure = "fpr") 
	#plot(pref)
	return(performance(predicted_model, measure = "tpr", x.measure = "fpr") )
}

naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 0.5, poslowieMeta) # quantile of rank 0.5 which is median
naive_threshold_result_bin <- ifelse(naive_threshold_result == TRUE, 1,0)
poslowieMetaBin <- ifelse(poslowieMeta[,"party_changed"] == TRUE, 1, 0)
perf1 <- pref_roc_curve(naive_threshold_result_bin, poslowieMetaBin, c(0,1))

naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 0.8, poslowieMeta) # quantile of rank 0.8 which is median
naive_threshold_result_bin <- ifelse(naive_threshold_result == TRUE, 1,0)
poslowieMetaBin <- ifelse(poslowieMeta[,"party_changed"] == TRUE, 1, 0)
perf2 <- pref_roc_curve(naive_threshold_result_bin, poslowieMetaBin, c(0,1))


naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 1.0, poslowieMeta) # quantile of rank 1.0 which is median
naive_threshold_result_bin <- ifelse(naive_threshold_result == TRUE, 1,0)
poslowieMetaBin <- ifelse(poslowieMeta[,"party_changed"] == TRUE, 1, 0)
perf3 <- pref_roc_curve(naive_threshold_result_bin, poslowieMetaBin, c(0,1))

naive_threshold_quantile = predict_naive_threshold_quantile("own_dst", 0.0, poslowieMeta) # quantile of rank 0.0 which is median
naive_threshold_result_bin <- ifelse(naive_threshold_result == TRUE, 1,0)
poslowieMetaBin <- ifelse(poslowieMeta[,"party_changed"] == TRUE, 1, 0)
perf4 <- pref_roc_curve(naive_threshold_result_bin, poslowieMetaBin, c(0,1))
