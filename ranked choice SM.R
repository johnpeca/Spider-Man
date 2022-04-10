install.packages("googlesheets4")
install.packages("aod")
install.packages("ggplot2")
install.packages("igraph")
install.packages("readxl")
install.packages("tidyverse")

library(aod)
library(ggplot2)
library(igraph)
library(tidyverse)
library(readxl)
library(dplyr)
library(googlesheets4)
###################################################################################
###################################################################################
# ETL

# Load the file (UPDATE with right file name/location)
election = read_sheet("159fWio7ZxPA7xasGasX_s9y8zAHIhBjQvP4t_g0gGhk")

# assumes the first nonrankedColumns columns can be ignored and the remaining columns are (resp) ranked choices (IN ORDER)
nonrankedColumns = 1 # UPDATE if more leading non-ranked columns
nonrankedColumns_end = 1 # UPDATE if more trailing non-ranked columns (default can be 0 if form set up correctly)
choices = ncol(election)-nonrankedColumns-nonrankedColumns_end

# Clean column names
names(election)[nonrankedColumns+(1:choices)] = paste0('Choice',1:choices)

# Create name list (to be referenced throughout -- UPDATE for different election)
nameList = c('Tobey Maguire',
             'Andrew Garfield',
             'Tom Holland')

colStart = ncol(election)
numCandidates = length(nameList)

# Convert all preferences to numerical ranks: e.g. 1,2,3 if 3 choices, and everything else is 4
for (i  in 1:numCandidates){
  election[,ncol(election)+1] = choices + 1
  for (j in 1:choices) {
    election[which(election[,j+nonrankedColumns] == nameList[i]),ncol(election)] = j
  }
}
names(election)[ncol(election)+1-numCandidates:1] = nameList

# Make an overall preferences matrix (column > row)
preferences = matrix(0L, nrow = numCandidates, ncol = numCandidates, dimnames = list(nameList,nameList))

for(i in 1:numCandidates){
  for(j in 1:numCandidates){
    # column over row favored
    preferences[i,j] = sum(election[,i+colStart] > election[,j+colStart])
  }
}

###################################################################################
###################################################################################
# DETERMINE THE CONDORCET WINNER USING THE TIDEMAN ALGORITHM

# Create TALLY data frame to determine majorities and ranking
tally = data.frame(More=integer(),
                   More_name=character(), 
                   Less=integer(),
                   Less_name = character(),
                   Margin=integer(),
                   Minority=integer(),
                   TotalFirstMore=integer(),
                   TotalFirstLess=integer(),
                   TotalVotesMore=integer(),
                   TotalVotesLess=integer(),
                   stringsAsFactors=FALSE) 

for(i in 2:numCandidates){
  for(j in 1:(i-1)){
    Margin = abs(preferences[i,j] - preferences[j,i])
    
    if (Margin > 0){
      if (preferences[i,j] > preferences[j,i]){
        More = j
        Less = i
        Minority = preferences[j,i]
      } else {
        More = i
        Less = j
        Minority = preferences[i,j]
      }
      More_name = nameList[More]
      Less_name = nameList[Less]
      TotalFirstMore = sum(election[,colStart+More] == 1)
      TotalFirstLess = sum(election[,colStart+Less] == 1)
      TotalVotesMore = sum(election[,colStart+More]<choices + 1)
      TotalVotesLess = sum(election[,colStart+Less]<choices + 1)
      tally = rbind(tally,data.frame(More,More_name,
                                     Less,Less_name,
                                     Margin,Minority,
                                     TotalFirstMore,
                                     TotalFirstLess,
                                     TotalVotesMore,
                                     TotalVotesLess))
    }
  }
}

# SORT step
# descending sort by margin, smallest minority, and total votes for the winner and loser
tally = tally[order(-tally$Margin,tally$Minority,-tally$TotalFirstMore,tally$TotalFirstLess,
                    -tally$TotalVotesMore,tally$TotalVotesLess),] 

# LOCK step, add each edge as long as don't introduce cycle
xedges = NULL
for (i in 1:nrow(tally)){
  x = tally[i,1]
  y = tally[i,3]
  g = graph(edges = xedges,n=numCandidates)
  if (length(all_simple_paths(g,y,x,"out")) == 0){ # only add edge if not creating a cycle
    xedges = c(xedges,x,y)
  }
}

g = graph(edges = nameList[xedges])
plot(g,edge.arrow.size=0.5)

# Determine the Condorcet winner:
# The Condorcet winner is the person who would win a two-candidate election 
# against each of the other candidates in a plurality vote.
# In terms of the associated graph, it would be the lone source, as in
# the only vertex with internal degree 0.

# Compute overall Condorcet rankings
Condorcet_rank=NULL
allSet = 1:numCandidates
Rd = 1
yedges = xedges
while (any(allSet) | Rd < numCandidates + 1) {
  # determine all vertices with no outgoing edges
  Condorcet_new = setdiff(allSet,yedges[seq(2,length(yedges),2)])
  
  if (length(Condorcet_new) > 1) {
    Condorcet_new_ranked = data.frame(Condorcet_new = integer(),
                                      rankSum = integer(),
                                      rankAvg = numeric()
    )
    for (i in Condorcet_new) {
      Condorcet_new_ranked[i,] = c(i,
                                   sum(election[,colStart+i]<choices + 1),
                                   mean(rowMeans(election[,colStart+i]))
      )
    }
    Condorcet_new_ranked = Condorcet_new_ranked[order(-Condorcet_new_ranked$rankSum,
                                                      Condorcet_new_ranked$rankAvg),]
    Condorcet_new = Condorcet_new_ranked[,1]
    Condorcet_new = Condorcet_new[!is.na(Condorcet_new)]
  }
  
  Condorcet_rank = c(Condorcet_rank,Condorcet_new)
  
  allSet = setdiff(allSet,Condorcet_new)
  
  for (i in Condorcet_new){
    yedges[c(which(yedges == i),which(yedges == i)+1)] = 0
  }
  
  Rd = Rd + 1
}

Condorcet_rank_list = 1:numCandidates
Condorcet_rank_list[Condorcet_rank] = 1:numCandidates

###################################################################################
# DETERMINE THE INSTANT-RUNOFF WINNER

Tot = nrow(election)
Rd = 1
Max = 0
compare = election[,ncol(election)+1-(numCandidates:1)] # pull last n columns
output = data.frame(id = 1:numCandidates, 
                    nameList,
                    CondorcetRank = Condorcet_rank_list
)

while (Max < 0.5*Tot) {
  Round = NULL
  for (i in 1:numCandidates){
    Round[length(Round)+1] = length(which(compare[,i]==1))
  }
  
  update_output = data.frame(id = 1:numCandidates,Round)
  output = merge(x = output, y = update_output, by = 'id', all = TRUE)
  
  names(output)[ncol(output)] = paste0('Round', Rd)
  
  
  output = output[order(-output[,ncol(output)],output$CondorcetRank),]
  Max = output[1,ncol(output)]
  Min = output[numCandidates+1-Rd,1]
  
  for (i in 1:numCandidates){
    # move each remaining candidate ranked lower than the min up 1 rank
    compare[,i] = compare[,i] - (compare[,i]>compare[,Min] & compare[,i] < numCandidates+1-Rd)
  }
  compare[,Min] = numCandidates+1-Rd
  Rd = Rd + 1
}

output[,4:ncol(output)] = output[,4:ncol(output)] / Tot