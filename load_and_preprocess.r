source("statistics_generators.r")

##############################################################
## This is interface for loading data and creating the meta ##
## data content                                             ##
##############################################################
##  returns: The list with full data at [[1]] (for building model)
##           and testing data at [[2]] (build in state in half of cadency)
##
##  N: Number of votings to look back to
##############################################################
loadAndPreprocess = function(N = NA) {
  ## Loading data
  glosowania = read.table("data/glosowania.txt", sep="\t", header=T, row.names=1)
  glosowaniaPartie = read.table("data/party_affiliations.txt", sep="\t", header=T, row.names=1)
  poslowieMeta_base = read.table("data/poslowie_metadata.txt", sep="\t", header=T, row.names=1)
  poslowieMeta = poslowieMeta_base

  glosowania_m = as.matrix(glosowania)
  glosowania_m_full = glosowania_m
  glosowaniaPartie_m = as.matrix(glosowaniaPartie)
  glosowaniaPartie_m = t(apply(glosowaniaPartie_m, 1, function (x) { ifelse(is.na(x), labels(which.max(table(x))), x) }))
  glosowaniaPartie_m_full = glosowaniaPartie_m

  if (!is.na(N)) {
    START_N = max((dim(glosowania_m)[2]-(N-1)), 1)
    glosowania_m       = glosowania_m[,START_N:dim(glosowania_m)[2]]
    glosowaniaPartie_m = glosowaniaPartie_m[,START_N:dim(glosowaniaPartie_m)[2]]
  }

  ## Modify voting table to meet our numeric interpretation
  ## 1.0 - for
  ## 0.5 - held
  ## 0.0 - againts
  glosowania_to_avg = glosowania_m
  glosowania_to_avg = replace(glosowania_m, which(glosowania_m == 0), 0.5)
  glosowania_to_avg = replace(glosowania_m, which(glosowania_m == -1), 0)
  
  glosowania_to_avg_full = glosowania_m_full
  glosowania_to_avg_full = replace(glosowania_m_full, which(glosowania_m == 0), 0.5)
  glosowania_to_avg_full = replace(glosowania_m_full, which(glosowania_m == -1), 0)

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
  changed_party = apply(glosowaniaPartie_m_full, 1, function (r) {
    length(unique(r[which(r != "brak-informacji")])) > 1
  })
  poslowieMeta = cbind(poslowieMeta, party_changed = changed_party)


  ###############################################
  ##            BUILDING TESTING DATA          ##
  ###############################################
  half_length = dim(glosowania_to_avg_full)[2]/2
  beg_half_length = max(ifelse(is.na(N), 1, half_length-(N-1)), 1)
  half_data_avg = glosowania_to_avg_full[,beg_half_length:half_length]
  half_data_partie = glosowaniaPartie_m_full[,beg_half_length:half_length]
  half_data_partie_full = glosowaniaPartie_m_full[,1:half_length]
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
  half_poslowieMeta = cbind(half_poslowieMeta, avg_streak_against_party = half_party_mode_against_streak_avg)

  #### ANOTHER PARAMETER IS MAX LENGHT OF STREAK AGAINST PARTY
  half_party_mode_against_streak_max = generateStatistic("against_party_mode_max_streak",
                                                         half_data_avg,
                                                         half_data_partie,
                                                         half_partie)

  half_poslowieMeta = cbind(half_poslowieMeta, max_streak_against_party = half_party_mode_against_streak_max)

  #### CLASS CALCULATING
  half_poslowieMeta = cbind(half_poslowieMeta, party_changed = changed_party)

  ## Removing deputies which already made their choice ;)
  changed_in_half = unlist(
    lapply(
      apply(half_data_partie_full, 1, unique),
      function(x) {
        (length(x) > 1 && length(which(x == "brak-informacji")) == 0) ||
          (length(x) > 2)
      }
    )
  )
  half_poslowieMeta = half_poslowieMeta[-which(changed_in_half),]

  list(poslowieMeta, half_poslowieMeta)
}
