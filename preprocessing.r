##Library check
library("party") ##library with decision tree
library("e1071") ##library with naive bayes
library("ROCR") ##library with roc curve

##############################################################
## Calculate Party Distance party for each vote separetly.  ##
##############################################################
party_vote_distance = function(votes_values,
                               votes_parties,
                               parties,
                               party_to_calculate_statistic_to) {
  ## Creates single column of parties votes (one voting all parties)
  ## Needs single vector of votes and single vector of parties coresponding to votes
  party_votes_column = function(votes, parties) {
    votes_table = table(votes, parties)
    apply(votes_table, 2, function(x) { weighted.mean(as.numeric(rownames(votes_table)), as.numeric(x)) })    
  }

  party_votes = matrix(nrow = length(parties),
                       ncol = dim(votes_values)[2])
  rownames(party_votes) = parties;
  colnames(party_votes) = colnames(votes_parties)
  for (i in 1:dim(votes_values)[2]) {
    party_votes_column_current = party_votes_column(votes_values[,i], votes_parties[,i])  
    party_votes[,i] = replace(party_votes[,i], labels(party_votes_column_current), party_votes_column_current)
  }

  # if we were not able to calculate proper value, we will use "neutral" - 0.5
  party_votes = replace(party_votes, which(is.na(party_votes) | is.nan(party_votes)), 0.5)

  # perfect_vote table is modified table of votes where each deputy has vote of his
  # party assigned to him. It might be impossible value though as single deputy can only
  # vote 0, 0.5 or 1 and party vote may be any floting point number.
  perfect_vote = votes_values
  for (i in 1:dim(perfect_vote)[1]) {
    for (j in 1:dim(perfect_vote)[2]) {
      perfect_vote[i, j] = party_votes[votes_parties[i, j], j]
    }
  }

  # party_dsts table shows how far was each deputy from his party perfect vote
  party_dsts = abs(votes_values - perfect_vote)

  return(party_dsts)
}

##############################################################
## Average Party Vote Distance Statistic generator          ##
##############################################################
## Create table of parties votes as avearage vote of peoples in this party
## each cell is in [0, 1] range
## eg.
##        voting1 voting2 voting3
## party1   0.72    0.4     0.11
## party2   0.55    0.0     0.34
##############################################################
statistic_party_vote_distance = function(votes_values,
                                         votes_parties,
                                         parties,
                                         party_to_calculate_statistic_to) {
  party_dsts = party_vote_distance(votes_values,
                                   votes_parties,
                                   parties,
                                   party_to_calculate_statistic_to)
  avg_party_dsts = apply(party_dsts, 1, mean, na.rm=TRUE)

  return(avg_party_dsts)
}

##############################################################
## Normalized Average Party Vote Distance Statistic gener.  ##
##############################################################
## This staistic is same as statistic_party_vote_distance but
## it is normalized by party loyality in each vote.
##############################################################
statistic_party_vote_distance_normalized = function(votes_values,
                                                    votes_parties,
                                                    parties,
                                                    party_to_calculate_statistic_to) {
  party_dsts = party_vote_distance(votes_values,
                                   votes_parties,
                                   parties,
                                   party_to_calculate_statistic_to)

  # party loyality is average distance of deputy from the party to his party vote.
  partyLoyality = matrix(nrow=length(partie), ncol=dim(party_dsts)[2])
  rownames(partyLoyality) = partie;
  colnames(partyLoyality) = colnames(party_dsts)
  for (i in 1:dim(party_dsts)[2]) {
    partyLoyal = partyVotes_single(party_dsts[,i], votes_parties[,i])  
    partyLoyality[,i] = replace(partyLoyality[,i], labels(partyLoyal), partyLoyal)
  }

  party_dsts_normalizer = glosowania_to_avg
  for (i in 1:dim(party_dsts_normalizer)[1]) {
    for (j in 1:dim(party_dsts_normalizer)[2]) {
      party_dsts_normalizer[i, j] = partyLoyality[votes_parties[i, j], j]
    }
  }

  party_dsts_normalized = party_dsts - party_dsts_normalizer;
  avg_party_dsts_normalized = apply(party_dsts_normalized, 1, mean, na.rm=TRUE)

  return(avg_party_dsts_normalized)
}

##############################################################
## Average Distance to Party Vote Statistic generator       ##
##############################################################
statistic_party_mode_distance = function(votes_values,
                                         votes_parties,
                                         parties,
                                         party_to_calculate_statistic_to) {
  ## Creates single column of parties modes (one voting all parties)
  ## Needs single vector of votes and single vector of parties coresponding to votes
  party_modes_column = function(votes, parties) {
    votes_table = table(votes, parties)
    apply(votes_table, 2, function(x) { mean(as.numeric(rownames(votes_table)[which(x == max(x))])) })    
  }

  party_modes = matrix(nrow=length(partie), ncol=dim(votes_values)[2])
  rownames(party_modes) = partie;
  colnames(party_modes) = colnames(votes_parties)
  for (i in 1:dim(votes_values)[2]) {
    party_mode_column_current = party_modes_column(votes_values[,i], votes_parties[,i])  
    party_modes[,i] = replace(party_modes[,i], labels(party_mode_column_current), party_mode_column_current)
  }

  # if we were not able to calculate proper value, we will use "neutral" - 0.5
  party_modes = replace(party_modes, which(is.na(party_modes) | is.nan(party_modes)), 0.5)

  # perfect_vote_mode table is modified table of votes where each deputy has mode vote of his
  # party assigned to him. It is value from set {0, 0.25, 0.5, 0.75, 1}
  perfect_vote_mode = votes_values
  for (i in 1:dim(perfect_vote_mode)[1]) {
    for (j in 1:dim(perfect_vote_mode)[2]) {
      perfect_vote_mode[i, j] = party_modes[votes_parties[i, j], j]
    }
  }

  # party_dsts_mode table shows how far was each deputy from his party perfect mode vote
  party_dsts_mode = abs(votes_values - perfect_vote_mode)
  avg_party_dsts_mode = apply(party_dsts_mode, 1, mean, na.rm=TRUE)

  return(avg_party_dsts_mode)
}

##############################################################
## This is interface for generating meta values for deputies #
##############################################################
##
##  statistic: The statistic to be calculated, one of following:
##             { "party_vote_distance",
##               "party_vote_distance_normalized",
##               "party_mode_distance" }
##
##  votes_values: Values of deputies votes in all votings during cadency
##                as matrix in required format:
##                  * row for each deputy
##                  * column for each vote
##                  * 0 - vote against,
##                    1 - vote for,
##                    0.5 - abstain or NA
##
##  votes_parties: Matrix of deputies parties during cadency
##                 It should have exacly same size as vote_values
##                 As NA is treated as party it should be replaced by some fixed literal.
##
##  parties: List of all existing party inside whole cadency.
##           By default it is NA and will be calculated from votes_parties
##           by passing this value will make processing much faster.
##
##  party_to_calculate_statistic_to: The party that we want to calulate statistic function to
##                                   Default NA is calculating distance from own party.
##
##############################################################
generateStatistic = function(statistic,
                             votes_values,
                             votes_parties,
                             parties = NA,
                             party_to_calculate_statistic_to = NA) {
  if (is.na(parties)[1])
    parties = unique(c(votes_parties))

  switch(statistic,
         party_vote_distance =
           statistic_party_vote_distance(votes_values,
                                         votes_parties,
                                         parties,
                                         party_to_calculate_statistic_to),
         party_vote_distance_normalized =
           statistic_party_vote_distance_normalized(votes_values,
                                                    votes_parties,
                                                    parties,
                                                    party_to_calculate_statistic_to),
         party_mode_distance =
           statistic_party_mode_distance(votes_values,
                                         votes_parties,
                                         parties,
                                         party_to_calculate_statistic_to)
  )
}

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

#### OUR FIRST PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES
half_party_vote_distance_own = generateStatistic("party_vote_distance",
                                                 half_data_avg,
                                                 half_data_partie,
                                                 partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_dst = half_party_vote_distance_own)

#### OUR SECOND PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES NORMALIZED
#### BY EACH VOTING LOYALITY OF THE PARTY.
half_avg_party_dsts_normalized = generateStatistic("party_vote_distance_normalized",
                                                   half_data_avg,
                                                   half_data_partie,
                                                   partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_dst_norm = half_avg_party_dsts_normalized)

#### ANOTHER PARAMETER IS AVERAGE DISTANCE FROM PARTY MODE VOTES
half_party_mode_distance_own = generateStatistic("party_mode_distance",
                                                 half_data_avg,
                                                 half_data_partie,
                                                 partie)
half_poslowieMeta = cbind(half_poslowieMeta, own_mode_dst = half_party_mode_distance_own)

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

