footballPre <- read.csv("footballData.csv")

#### Data Cleaning ####

## pref foot -> 1,0

## Dropped: sofifa_id, player_url, short_name, long_name,
  ## team_position, national_position, national_jersey_number
  ## body_type

## Drop: player_tags, player_traits, real_face?
  ## All Goalies? Just focus on players?
  ## Or remove all predictors exclusive to one position?
Drop <- c(1,2,3,4,23,27,32,33)
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