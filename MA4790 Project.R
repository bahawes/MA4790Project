footballPre <- read.csv("footballData.csv")
library(corrplot)

#### Explore data ####
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

#### Data pre-processing ####

  #### Removing variables which are either not useful or impractical ####

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

  #### Adding Variables ####

    ## team_position -> sub or not (0 or 1)
      football$team_position <- 
        ifelse(football$team_position == "SUB", 0, 1)
    
    
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

    ## real face -> 1: true, 0: false
      football$real_face <- 
        ifelse(football$real_face == "Yes", 1, 0)
      
    ## loaned from -> on_loan
      football$loaned_from <- 
        ifelse(football$loaned_from == "", 0, 1)
      colnames(football)[18] <- "on_loan"
  
    ## Split work rate into 2, each with dummy variables
      ATKhigh <- c()
      ATKmed <- c()
      
      DEFhigh <- c()
      DEFmed <- c()
      
      football$work_rate <- strsplit(football$work_rate, "/")
      
      for (i in 1:length(football$work_rate)) {
        ATK <- unlist(football$work_rate[i])[1]
        DEF <- unlist(football$work_rate[i])[2]
        
        # Set ATK
        if (ATK == "High") {
          ATKhigh <- append(ATKhigh, 1)
          ATKmed <- append(ATKmed, 0)
        }
        
        else if (ATK == "Medium") {
          ATKhigh <- append(ATKhigh, 0)
          ATKmed <- append(ATKmed, 1)
        }
        
        else {
          ATKhigh <- append(ATKhigh, 0)
          ATKmed <- append(ATKmed, 0)
        }
        
        # Set DEF
        if (DEF == "High") {
          DEFhigh <- append(DEFhigh, 1)
          DEFmed <- append(DEFmed, 0)
        }
        
        else if (DEF == "Medium") {
          DEFhigh <- append(DEFhigh, 0)
          DEFmed <- append(DEFmed, 1)
        }
        
        else {
          DEFhigh <- append(DEFhigh, 0)
          DEFmed <- append(DEFmed, 0)
        }
        
      }
      
      football$ATKhigh <- ATKhigh
      football$ATKmed <- ATKmed
      football$DEFhigh <- DEFhigh
      football$DEFmed <- DEFmed
      
      football <- football[-14]
    
    ## League Name
      leagues <- unique(football$league_name)
      
      leagues_mat <- matrix(ncol = length(leagues), nrow = length(football$league_name))
      
      for (i in 1:length(football$league_name)) {
        currentLeague <- football$league_name[i]
        currentRow <- c()
        
        for (j in 1:length(leagues)) {
          currentValue <- ifelse(currentLeague == leagues[j], 1, 0)
          currentRow <- append(currentRow, currentValue)
        }

        leagues_mat[i,] <- currentRow
      }
      
      leagues_df <- as.data.frame(leagues_mat)
      colnames(leagues_df) <- leagues
      football <- cbind(football, leagues_df)
      
      football <- football[-4]

  #### Deleting predictors ####
  correlations <- cor(football[-(52:123)])
  corrplot(correlations)
      
  #### Imputation ####
