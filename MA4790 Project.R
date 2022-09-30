footballPre <- read.csv("footballData.csv")

#### Data Cleaning ####

## Dropped: sofifa_id, player_url, short_name, long_name,
  ##  national_jersey_number, team_jersey_number, national_jersey_number
  ## body_type, player_tags, player_traits, joined, contract_valid_until, defending_marking
  ## *position stats, dob, club_name, nationality
  ## All Goalies rows
  ## all predictors exclusive to goalies

Drop <- c(1,2,3,4,6,9,10,23,26,28,30,31,33,40:45,46,73,76:106)
football <- footballPre[-Drop]
football <- subset(football,
                   !grepl("GK",football$player_positions)
                   )

## team_position -> sub or not (0 or 1)
for(i in 1:length(football)){
  
  if(football$team_position[i] == "SUB"){
    newValue <- 1
  }
  
  else{
    newValue <- 0
  }
  
  football$team_position[i] <- newValue
}


## Player_positions -> dummy variables

  # find list of all positions
  positions <- c()
  for (i in 1:nrow(football)) {
    currentPositions <- unlist(strsplit(football$player_positions[i], ","))
    positions <- append(positions, currentPositions)
  }
  positions <- gsub(" ", "", positions)
  positions <- unique(positions)
  
  # set new predictors
  positions_mat <- matrix(ncol = length(positions))
  for (i in 1:nrow(football)) {
    currentRow <- c()
    currentPositions <- unlist(strsplit(football$player_positions[i], ","))
    
    for (j in 1:length(positions)) {
      
      if (positions[j] %in% currentPositions) {
        currentRow <- append(currentRow, 1)
      }
      
      else {
        currentRow <- append(currentRow, 0)
      }
      
      
    }
    positions_mat <- rbind(positions_mat, currentRow)
  }
  
  # clean data frame
  positions_df <- as.data.frame(positions_mat, row.names = FALSE)
  positions_df <- positions_df[-1,]
  colnames(positions_df) <- positions
  
  # add new predictors and drop original predictor
  football <- cbind(football, positions_df)
  football <- football[-10]
  
  
## preferred_foot -> 1,0
  # left: 0, right: 1
  football$preferred_foot <- 
    ifelse(football$preferred_foot == "Left", 0, 1)
  
## nation_position -> on_national_team
  football$nation_position <- 
    ifelse(football$nation_position == "", 0, 1)
  colnames(football)[19] <- "on_national_team"
  
## national position -> on_national_team
  
## Split work rate into 2
  
## League Name
  
## drop player positions
  
## real face
  
## loaned from -> on_loan

#### Explore Data ####
## Goal
  ## to predict a players value from our predictors
## Data Structure
dim(football)
str(football)

charCount <- 0
for (i in football) {
  if (typeof(i) == "character") {
    charCount <- charCount + 1
  }
}

intCount <- dim(football)[2] - charCount

charCount
intCount

## Sample Size: 
## Response Variable: value_euro
## Number of predictors: 
## Categorical: 
## Continuous: 

#### Data Preprocessing ####