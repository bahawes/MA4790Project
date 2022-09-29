football <- read.csv("footballData.csv")

#### Data Cleaning ####

#### Explore Data ####
# Goal
# to predict a players value from our predictors
# Data Structure
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

# Sample Size: 18944
# Response Variable: value_euro
# Number of predictors: 105? (remove predictors that don't make sense like player url and name)
# Categorical: 44?
# Continuous: 61?

#### Data Preprocessing ####