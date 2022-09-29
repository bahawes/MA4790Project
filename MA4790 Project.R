footballPre <- read.csv("footballData.csv")

#### Data Cleaning ####

## pref foot -> 1,0

## Dropped: sofifa_id, player_url, short_name, long_name,
  ##  national_jersey_number, team_jersey_number, national_jersey_number
  ## body_type, player_tags, player_traits, joined, contract_, defending_marking
  ## *position stats, dob, club_name, nationality

  ## Drop All Goalies
  ## Drop columns exclusive to goalies

## team_position -> sub or not (0 or 1)

## Column for if on national team
Drop <- c(1,2,3,4,33)
football <- footballPre[-Drop]


## How to clean position?
  ## Var for each position, 0 or 1?

## Split work rate into 2

## what does realface mean?
  ## Drop realface?

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