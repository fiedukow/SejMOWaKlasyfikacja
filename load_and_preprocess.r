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

#### ANOTHER PARAMETER IS AVERAGE LENGHT OF STREAK AGAINST PARTY
half_party_mode_against_streak_avg = generateStatistic("against_party_mode_average_streak",
                                                       half_data_avg,
                                                       half_data_partie,
                                                       half_partie)
half_poslowieMeta = cbind(poslowieMeta, avg_streak_against_party = half_party_mode_against_streak_avg)

#### ANOTHER PARAMETER IS MAX LENGHT OF STREAK AGAINST PARTY
half_party_mode_against_streak_max = generateStatistic("against_party_mode_max_streak",
                                                       half_data_avg,
                                                       half_data_partie,
                                                       half_partie)
half_poslowieMeta = cbind(poslowieMeta, max_streak_against_party = half_party_mode_against_streak_max)

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
