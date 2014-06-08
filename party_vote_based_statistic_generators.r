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
