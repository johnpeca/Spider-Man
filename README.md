# Spider-Man

File points to a **google sheets** output of a google form. Will need to:
(1) authenticate through google account that has access to the google sheets file, and 
(2) check box to allow edits through R.

Assumes google form has minimal information for ranking, which would just be one multi-grid questions with COLUMNS corresponding to choices and ROWS corresponding to ranked choices (e.g., first row will select the column with the user's first ranked choice, second row will indicate the second ranked choice, etc.). Default assumption can assume 1 leading column (timestamp) and 0 trailing columns, and these can be updated. If more candidates than choices, will fill in remainder of ballot to give all other options equal rank of (# choices)+1 (e.g., if 7 candidates with top 3 choices stored, remaining candidates receive rank of 3+1=4).

Currently hard codes in a nameList variable with the choices that need to match the recorded choices in the form. Some room for additional automation is in place.

Working through this file will first produce the **Tideman algorithm** determined Condorcet winner. See here: https://en.wikipedia.org/wiki/Ranked_pairs
And then the **Instant-Runoff** model winner.

**Output will include**:
(1) **election**: data frame with original google form data, which will be updated and cleaned in ETL step

(2) **preferences**: matrix that counts head-to-head totals, with column indicating winners (majority) and row losers (minority). (So (i,j) indicates number of ballots that preferred i to j, while (j,i) indicates the number of ballots that preferred j to i. Note this is not going to be symmetric.)

(3) **tally**: data frame that returns the ranked list of head-to-head battles. This compares (i,j) to (j,i) to determine who wins the head-to-head battle, and then ranks all of the head-to-head battles in order of preferences (**currently**: (1) margin of victory for majority, (2) total votes for minority, (3) total first rank choices for majority, (4) total first rank choices for minority, (5) total overall top choices for majority (e.g., if more candidates than submitted ranked choices, returns the total ballots that included the winner among their submitted top choices), (6) total overall top choices for minority)

(4) **g**: directed graph, with vertices corresponding to the options, and directed edges from majority to minority that are added according to the tally ranking as long as a cycle is not introduced (so will skip an edge from tally in that case -- so number of edges will be bounded above by the number of entries in tally, and will be strictly smaller if any edge would have resulted in a cycle). The Condorcet winner is the vertex with 0 ingoing degree.

(5) **output**: data frame that indicates the Condorcet ranking (top choice is vertex in g with 0 ingoing degree, next rank goes to the vertex in g minus the subgraph generated by the top choice, etc.) as well as the instant-runoff victor. The Round x columns indicate a round in the instant-runoff model (each round iterative removes one candidate and reruns the election until one candidate receives a majority -- so number of rounds will be between 1 and (# candidates) - 1 -- e.g., if 6 candidates, can have at most 5 rounds). Current instant-runoff model uses condorcet ranking as tie-breaker.
