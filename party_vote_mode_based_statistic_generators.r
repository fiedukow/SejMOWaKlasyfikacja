party_vote_mode_distance = function(votes_values,
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

  return(party_dsts_mode)
}

##############################################################
## Average Distance to Party Vote Mode Statistic generator  ##
##############################################################
statistic_party_mode_distance = function(votes_values,
                                         votes_parties,
                                         parties,
                                         party_to_calculate_statistic_to) {
  party_dsts_mode = party_vote_mode_distance(votes_values,
                                             votes_parties,
                                             parties,
                                             party_to_calculate_statistic_to)
  avg_party_dsts_mode = apply(party_dsts_mode, 1, mean, na.rm=TRUE)

  return(avg_party_dsts_mode)
}
