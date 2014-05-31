##Library check
library("party") ##library with decision tree
library("e1071") ##library with naive bayes

## Loading data
glosowania = read.table("data/glosowania.txt", sep="\t", header=T, row.names=1) 
glosowaniaPartie = read.table("data/party_affiliations.txt", sep="\t", header=T, row.names=1) 
glosowaniaMeta = read.table("data/glosowania_metadata.txt", sep="\t", header=T, row.names=1) 
poslowieMeta = read.table("data/poslowie_metadata.txt", sep="\t", header=T, row.names=1) 
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

### Create table of parties votes as avearage vote of peoples in this party
### eg.
###        voting1 voting2 voting3
### party1   0.72    0.4     0.11
### party2   0.55    0.0     0.34

## Creates single column of parties votes (one voting all parties)
## Needs single vector of votes and single vector of parties coresponding to votes
partyVotes_single = function(votes, parties) {
  glosy = table(votes, parties) 
  apply(glosy, 2, function(x) { weighted.mean(as.numeric(rownames(glosy)), as.numeric(x)) })    
}

partyVotes = matrix(nrow=length(partie), ncol=dim(glosowania_to_avg)[2])
rownames(partyVotes) = partie;
colnames(partyVotes) = colnames(glosowania_m)
for (i in 1:dim(glosowania_to_avg)[2]) {
  partyVote = partyVotes_single(glosowania_to_avg[,i], glosowaniaPartie_m[,i])  
  partyVotes[,i] = replace(partyVotes[,i], labels(partyVote), partyVote)
}

# if we were not able to calculate proper value, we will use "neutral" - 0.5
partyVotes = replace(partyVotes, which(is.na(partyVotes) | is.nan(partyVotes)), 0.5)

# perfect_vote table is modified table of votes where each deputy has vote of his
# party assigned to himself. It might be impossible value though as single deputy can only
# vote 0, 0.5 or 1 and party vote may be any floting point number.
perfect_vote = glosowania_to_avg
for (i in 1:dim(perfect_vote)[1]) {
  for (j in 1:dim(perfect_vote)[2]) {  
    perfect_vote[i, j] = partyVotes[glosowaniaPartie_m[i, j], j]
  }
}

# party_dsts table shows how far was each deputy from his party perfect vote
party_dsts = abs(glosowania_to_avg - perfect_vote)
avg_party_dsts = apply(party_dsts, 1, mean, na.rm=TRUE)

#### OUR FIRST PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES
poslowieMeta = cbind(poslowieMeta, own_dst = avg_party_dsts)

# party loyality is average distance of deputy from the party to his party vote.
partyLoyality = matrix(nrow=length(partie), ncol=dim(party_dsts)[2])
rownames(partyLoyality) = partie;
colnames(partyLoyality) = colnames(party_dsts)
for (i in 1:dim(party_dsts)[2]) {
  partyLoyal = partyVotes_single(party_dsts[,i], glosowaniaPartie_m[,i])  
  partyLoyality[,i] = replace(partyLoyality[,i], labels(partyLoyal), partyLoyal)
}

party_dsts_normalizer = glosowania_to_avg
for (i in 1:dim(party_dsts_normalizer)[1]) {
  for (j in 1:dim(party_dsts_normalizer)[2]) {  
    party_dsts_normalizer[i, j] = partyLoyality[glosowaniaPartie_m[i, j], j]
  }
}

party_dsts_normalized = party_dsts - party_dsts_normalizer;
avg_party_dsts_normalized = apply(party_dsts_normalized, 1, mean, na.rm=TRUE)

#### OUR SECOND PARAMETER IS AVERAGE DISTANCE FROM PARTY VOTES NORMALIZED
#### BY EACH VOTING LOYALITY OF THE PARTY.
poslowieMeta = cbind(poslowieMeta, own_dst_norm = avg_party_dsts_normalized)

#### CLASS CALCULATING
changed_party = apply(glosowaniaPartie_m, 1, function (r) {
                                               length(unique(r[which(r != "brak-informacji")])) > 1
                                             })
poslowieMeta = cbind(poslowieMeta, party_changed = changed_party)


#### BUILDING THE MODEL

##Add another another attributes to +
decisionTree = ctree(party_changed ~ own_dst + own_dst_norm, data=poslowieMeta)

##example usage to predict whether patry has changed:
##predict(decisionTree, newdata=poslowieMeta) - newdata should be data, which we want to predict party_changed (also it should have computed attributes)

##naive bayes
bayes = naiveBayes(party_changed ~ own_dst + own_dst_norm, data=poslowieMeta)
##predict should work same as with decision trees, but something is wrong - to investigate
