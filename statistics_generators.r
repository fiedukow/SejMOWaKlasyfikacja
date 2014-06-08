source("party_vote_based_statistic_generators.r")
source("party_vote_mode_based_statistic_generators.r")

##############################################################
## This is interface for generating meta values for deputies #
##############################################################
##
##  statistic: The statistic to be calculated, one of following:
##             { "party_vote_distance",
##               "party_vote_distance_normalized",
##               "party_mode_distance",
##               "against_party_mode_average_streak",
##               "against_party_mode_max_streak" }
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