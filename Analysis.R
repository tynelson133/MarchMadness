library(dplyr)
library(reshape)
library(stringr)
library(lme4)
library(glmnet)
library(tidyr)
library(xgboost)

Teams <- read.csv('~/Downloads/Teams (1).csv')
Seasons <- read.csv("~/Downloads/Seasons (1).csv")
RegSeasonCompact <- read.csv("~/Downloads/RegularSeasonCompactResults (1).csv")
RegSeasonDetail <- read.csv("~/Downloads/RegularSeasonDetailedResults (1).csv")
TourneyCompact <- read.csv("~/Downloads/TourneyCompactResults (1).csv")
TourneyDetailed <- read.csv("~/Downloads/TourneyDetailedResults (1).csv")
TourneySeeds <- read.csv("~/Downloads/TourneySeeds (1).csv")
TourneySlots <- read.csv("~/Downloads/TourneySlots (1).csv")


Massey <- read.csv('~/Downloads/massey.csv')

##### Removing Seasons before 2003
Seasons <- Seasons %>% filter(Season < 2003)
Teams <- Teams %>% dplyr::rename(team = Team_Id)
##### Changing Team column name in Tourney Seeds and also creating a seed with just seed
## NUmber
TourneySeeds <- TourneySeeds %>% 
  dplyr::rename(team = Team) %>%
  mutate(Seed2 = as.numeric(gsub("[W,X,Y,Z,a,b,c]", "", Seed)))

###### Changing strucutre of data to stack information
ChangeDat <- function(Dat){
Dat <- Dat %>% mutate(ID = 1:nrow(Dat)) %>%
  mutate(PointDiff = abs(Wscore - Lscore))
WinTeamDat <- Dat %>% 
  select(starts_with('S'), starts_with('D'),starts_with('W'), starts_with('ID'),
         starts_with('PointDiff'), team2 = starts_with('Lteam')) %>%
  select(-Wloc)%>%
  mutate('Outcome' = 1)
colnames(WinTeamDat) <- gsub("W", "", colnames(WinTeamDat))
### Creating losing team information and changing column names
LoseTeamDat <- Dat %>% 
  select(starts_with('S'), starts_with('D'),starts_with('L'),starts_with('ID'),
         starts_with('PointDiff'), team2 = starts_with('Wteam')) %>%
  mutate('Outcome' = 0)
colnames(LoseTeamDat) <- gsub("L", "", colnames(LoseTeamDat))
### Combining Back Data
NewDat <- rbind(WinTeamDat, LoseTeamDat)

return(NewDat)
}

###### All New data with winners and losers stacked and only years 2003 and beyond
NewTourneyCompact <- ChangeDat(TourneyCompact) %>% filter(Season >= 2003)
NewTourneyDetail <- ChangeDat(TourneyDetailed) %>% filter(Season >= 2003)
NewRegSeasonDetail <- ChangeDat(RegSeasonDetail) %>% filter(Season >= 2003)
NewRegSeasonCompact <- ChangeDat(RegSeasonCompact) %>% filter(Season >= 2003)


##### Joining Seeds with team In Tourney
NewTourneyCompact <- NewTourneyCompact %>% 
  left_join(TourneySeeds,by = c("Season", "team"))

##### Combining Compact and detailed data 
TourneyDat <- left_join(NewTourneyCompact, NewTourneyDetail,
                         by = c("Season", "Daynum", "team", "score", "Outcome")) %>%
              select(-ID.x) %>% dplyr::rename(ID = ID.y) %>% 
              #filter(Daynum !=134, Daynum != 135) %>%
              mutate("Round" = ifelse(Daynum == 136 | Daynum == 137 |
                                        Daynum == 134 | Daynum == 135, 1, 
                          ifelse(Daynum == 138 | Daynum == 139, 2,
                                 ifelse(Daynum == 143 | Daynum == 144, 3,
                                        ifelse(Daynum == 145 | Daynum == 146, 4,
                                               ifelse(Daynum == 152, 5, 6))))))


SeasonDat <- left_join(NewRegSeasonCompact, NewRegSeasonDetail,
                       by = c("Season", "Daynum", "team", "score", "Outcome")) %>%
             select(-ID.x) %>% dplyr::rename(ID = ID.y, PointDiff = PointDiff.y,
                                             team2 = team2.y)  

######## Percent wins against the tournament teams ########
TourneyPercent <- lapply(unique(SeasonDat$Season), function(i){
  
Q <- SeasonDat %>% filter(Season == i)
W <- TourneySeeds %>% filter(Season == i) %>% distinct(team)
Q %>% filter(team2 %in% unlist(W)) %>%
  group_by(Season,team) %>% summarize("TPercent" = sum(Outcome)/n(),
                                      "TGames" = n())
})

TourneyPercent2 <- do.call('rbind', TourneyPercent)
#View(TourneyPercent2)
##### Trying to get number of games played of teams in tournament.
##### Looking at year 2015 for reference
View(filter(TourneyDat, Season == 2015))
View(SumSeasonDat)
###### Summarizing season dat 
SumSeasonDat <- SeasonDat %>% group_by(Season, team) %>% 
  summarize("MedPts" = median(score), "sdPts" = sd(score),
            "FGPct" = median(fgm/fga), "FGPct3" = median(fgm3/fga3),
            "FTPct" = median(ftm/fta), "MedOR" = median(or),
            "MedDR" = median(dr), "MedAst" = median(ast),
            "MedTO" = median(to), "MedSTL" = median(stl),
            "MedBLK" = median(blk), "MedPF" = median(pf),
            "MedPtDiff" = median(PointDiff))
            

######## Getting Record of last ten games
Rec10Season <- SeasonDat %>% group_by(Season, team) %>%
                arrange(Season, desc(Daynum), team ) %>%
                top_n(10, Daynum) %>% summarize('Last10' = mean(Outcome))

SumSeasonDat <- SumSeasonDat %>% left_join(Rec10Season, by = c('Season', 'team')) %>%
  left_join(TourneyPercent2, by = c('Season', 'team'))

######## Getting overall percent of teams played
## First need record of each team
RecSeason <- SeasonDat %>% group_by(Season, team) %>%
   summarize('PercentRecord' = mean(Outcome))

#### Attaching record for season dat to season dat
SeasonDat <- SeasonDat %>% left_join(RecSeason, by = c('Season', 'team'))
#### Atatching record to summary of season dat
SumSeasonDat <- SumSeasonDat %>% left_join(RecSeason, by = c('Season', 'team'))



###### Looping through all teams
Foo3 <- list()
for(i in 1:nrow(Teams)){

Foo <- SeasonDat %>% filter(team == Teams$team[i]) %>% select(team, Season, ID)

Foo2 <- SeasonDat %>% filter(ID %in% Foo$ID) %>% filter(team != Teams$team[i]) %>%
        group_by(Season) %>% summarize("OpponentRecord" = median(PercentRecord))

Foo3[[i]] <- Foo %>% left_join(Foo2, by = c("Season")) %>% distinct(Season, OpponentRecord, team)

}
#### Stacking the opponent recod to combine to SumSeasonDat
OppRecInfo <- do.call("rbind", Foo3)
SumSeasonDat <- SumSeasonDat %>% left_join(OppRecInfo, by = c("Season", "team"))
View(OverallDat)

##### Combining tournament data with summary season data
### Adding Pomeroy stats from Pomeroy Tab
OverallDat <- TourneyDat %>% left_join(SumSeasonDat, 
                        by = c("Season", "team")) %>%
  mutate(SeedLoc = gsub("[0,1,2,3,4,5,6,7,8,9, a, b, c, d]", "", Seed)) %>%
  left_join(PomeroyStats, by = c('Season','team' = 'team.x', 'Seed2')) %>%
  select(-Team_Name)

View(SumSeasonDat %>% filter(Season == 2017))
###########################################################################
###################### Installing XGBoost #################################
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)


#################################################################################
########################## Not Taking the Difference of the Two Teams ###########
################################################################################
PredRightFunc <- function(year){
UseVars <- OverallDat %>%filter(Season != year) %>%
  #select(-(Season:Outcome), -(fgm:ID), -Round, -Seed) 
  select(Seed2, sdPts, PercentRecord, OpponentRecord, Last10, FTPct, Round)

OutcomeTrain <- unlist(OverallDat %>%filter(Season != year ) %>%
                select(Outcome))

Vars <- data.matrix(UseVars)


#bst <- xgb.cv(data = Vars, label = OutcomeTrain, max_depth = 4, nfold = 5,
 #              eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
bst <- xgboost(data = Vars, label = OutcomeTrain, max_depth = 4, 
               early_stopping_rounds = 5,
              eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
#bst <- xgboost(data = Vars, label = OutcomeTrain, max_depth = 4,
 #              eta = 1, nthread = 2, nrounds = 10, objective = loglossobj,
  #             eval_metric = "error", early_stopping_rounds = 4)

#xgb.plot.multi.trees(model = bst)
importance <- xgb.importance(feature_names = colnames(Vars), model = bst)


####### Predicting 2016 
#PredData <- data.matrix(OverallDat %>%filter(Season == year, Round == round) %>%
 # select(-(Season:Outcome), -(fgm:ID), -Round, -Seed))

PredData <- data.matrix(OverallDat %>%filter(Season == year) %>%
                           #select(-(Season:Outcome), -(fgm:ID), -Round, -Seed))
          select(Seed2, sdPts, PercentRecord, OpponentRecord, Last10, FTPct, Round))
                        

#OutcomeYear <- OverallDat %>% filter(Season == year, Round == round) %>% select(Outcome) 

Pred <- predict(bst, PredData)


WinningTeam <- OverallDat %>% filter(Season == year) %>%
  left_join(Teams, by = "team") %>% mutate(Prob = round(Pred,3)) %>%
  select(Team_Name, Seed2, Outcome, Prob, ID, Round) %>%
  group_by(ID) %>%
  arrange(ID,desc(Prob)) %>%
  mutate(PredOutcome = c(1,0)) %>%
  top_n(1,Prob) %>%
  arrange(desc(Round), Seed2)

 PredRight <- WinningTeam %>% mutate("Correct" = ifelse(Outcome == PredOutcome, 1, 0)) %>%
  ungroup() %>%
  summarize(mean(Correct)) 
  
 return(list(WinningTeam, PredRight, bst))
} 


Year2016Round1 <- PredRightFunc(year = 2014)
Year2015Round1 <- PredRightFunc(year = 2015, round =1)


View(Year2016Round1[[1]])
Year2016Round1[[2]]








#############################################################################
############################## 
PredRightFunc2 <- function(year){
  UseVars <- OverallDat %>%filter(Season != year) %>%
    #select(-(Season:Outcome), -(fgm:ID), -Round, -Seed) 
    select(Seed2, sdPts, PercentRecord, OpponentRecord, Last10, FTPct, Round)
  
  OutcomeTrain <- unlist(OverallDat %>%filter(Season != year ) %>%
                           select(score))
  
  Vars <- data.matrix(UseVars)
  
  
  #bst <- xgb.cv(data = Vars, label = OutcomeTrain, max_depth = 4, nfold = 5,
  #              eta = 1, nthread = 2, nrounds = 100, objective = "binary:logistic")
  bst <- xgboost(data = Vars, label = OutcomeTrain, max_depth = 4, 
                 early_stopping_rounds = 5,
                 eta = 1, nthread = 2, nrounds = 100, objective = "reg:linear")
  #bst <- xgboost(data = Vars, label = OutcomeTrain, max_depth = 4,
  #              eta = 1, nthread = 2, nrounds = 10, objective = loglossobj,
  #             eval_metric = "error", early_stopping_rounds = 4)
  
  #xgb.plot.multi.trees(model = bst)
  importance <- xgb.importance(feature_names = colnames(Vars), model = bst)
  
  
  ####### Predicting 2016 
  #PredData <- data.matrix(OverallDat %>%filter(Season == year, Round == round) %>%
  # select(-(Season:Outcome), -(fgm:ID), -Round, -Seed))
  
  PredData <- data.matrix(OverallDat %>%filter(Season == year) %>%
                            #select(-(Season:Outcome), -(fgm:ID), -Round, -Seed))
                            select(Seed2, sdPts, PercentRecord, OpponentRecord, Last10, FTPct, Round))
  
  
  #OutcomeYear <- OverallDat %>% filter(Season == year, Round == round) %>% select(Outcome) 
  
  Pred <- predict(bst, PredData)
  
  
  WinningTeam <- OverallDat %>% filter(Season == year) %>%
    left_join(Teams, by = "team") %>% mutate(Prob = round(Pred,3)) %>%
    select(Team_Name, Seed2, Outcome, Prob, ID, Round) %>%
    group_by(ID) %>%
    arrange(ID,desc(Prob)) %>%
    mutate(PredOutcome = c(1,0)) %>%
    top_n(1,Prob) %>%
    arrange(desc(Round), Seed2)
  
  PredRight <- WinningTeam %>% mutate("Correct" = ifelse(Outcome == PredOutcome, 1, 0)) %>%
    ungroup() %>%
    summarize(mean(Correct)) 
  
  return(list(WinningTeam, PredRight, bst))
} 

Year2016Round1 <- PredRightFunc(year = 2016)
Year2016Round1[[2]]


###############################################################################
###############################################################################
##################### Taking Difference of all variables ######################
###############################################################################
###############################################################################
library(reshape)

######## Getting Difference of each matchup in the tournaments
DiffDat <- OverallDat %>% group_by(ID) %>% arrange(ID,team)%>% 
  mutate('team2' = team, 'SeedTwo' = Seed2, "Seed3" = Seed2) %>%
  summarize(Season = Season[1],
            Round = Round[1],
            team = team[1],
            team2 = team2[2],
            Seed1 = Seed2[1],
            Seed2 = SeedTwo[2],
            DSeed = Seed3[1] - Seed3[2],
            DOutcome = ifelse(Outcome[1] > Outcome[2], 1, 0),
            DPts = MedPts[1] - MedPts[2],
            DsdPts = sdPts[1] - sdPts[2],
            DFG = FGPct[1] - FGPct[2],
            DFG3 = FGPct3[1] - FGPct3[2],
            DFT = FTPct[1] - FTPct[2],
            DOR = MedOR[1] - MedOR[2],
            DDR = MedDR[1] - MedDR[2],
            DAst = MedAst[1] - MedAst[2],
            DTO = MedTO[1] - MedTO[2],
            DSTL = MedSTL[1] - MedSTL[2],
            DBLK = MedBLK[1] - MedBLK[2],
            DPF = MedPF[1] - MedPF[2],
            DLast10 = Last10[1] - Last10[2],
            DRecord = PercentRecord[1] - PercentRecord[2],
            DOppRec = OpponentRecord[1] - OpponentRecord[2],
            DPtDiff = MedPtDiff[1] - MedPtDiff[2],
            DTPercent = TPercent[1] - TPercent[2],
            DTGames = TGames[1] - TGames[2]) %>%
  select(-ID) %>% na.omit() %>% filter(DSeed != -15, DSeed != 15, DSeed != 14, DSeed != -14)

###############################################################################
###############################################################################
unique(OverallDat$Season)
########## To make every combination of every team playing each team in the tournament
ComboFunc <- lapply(unique(OverallDat$Season), function(x){
  Foo <- unlist(OverallDat %>% filter(Season == x) %>% select(team) %>% distinct())
  Combs <- t(combn(sort(Foo), 2))
  Year <- rep(x, nrow(Combs))
  cbind(Year, Combs)
}) 
####### Constructing a dataframe to connect all the season stats
EachCombo <- data.frame(do.call("rbind", ComboFunc))
colnames(EachCombo) <- c("Season", 'team', 'team2')
###### Getting the distinct team statistics for each year
DistOverallDat <- OverallDat %>% select(-Daynum, -Outcome,-score, -(fgm:Round),
                                        -team2.x, -PointDiff.x) %>%
  distinct() 
View(DistOverallDat)
###### Creating a dataframe of the team statistics from each year
EachCombo2 <- EachCombo %>% group_by(Season) %>% arrange(Season, team, team2) %>%
  left_join(DistOverallDat, by = c("team", "Season")) %>%
  left_join(DistOverallDat, by = c("team2" = 'team', "Season")) 


####################### Making Round Variable #################  
EachCombo2 <- EachCombo2 %>%
  mutate(Round = ifelse(SeedLoc.x == SeedLoc.y  &
                          (Seed2.x == 16 & Seed2.y == 1) | 
                          (Seed2.x == 9 & Seed2.y == 8) |
                          (Seed2.x == 13 & Seed2.y == 4) |
                          (Seed2.x == 12 & Seed2.y == 5) |
                          (Seed2.x == 14 & Seed2.y == 3) |
                          (Seed2.x == 11 & Seed2.y == 6) |
                          (Seed2.x == 10 & Seed2.y == 7) |
                          (Seed2.x == 15 & Seed2.y == 2) | 
                          (Seed2.y == 16 & Seed2.x == 1) | 
                          (Seed2.y == 9  & Seed2.x == 8) |
                          (Seed2.y == 13 & Seed2.x == 4) |
                          (Seed2.y == 12 & Seed2.x == 5) |
                          (Seed2.y == 14 & Seed2.x == 3) |
                          (Seed2.y == 11 & Seed2.x == 6) |
                          (Seed2.y == 10 & Seed2.x == 7) |
                          (Seed2.y == 15 & Seed2.x == 2), 1, 
                        ifelse(SeedLoc.x == SeedLoc.y  &
                                 (Seed2.x %in% c(1,16) & Seed2.y %in% c(8,9)) | 
                                 (Seed2.x %in% c(4,13) & Seed2.y %in% c(5,12)) |
                                 (Seed2.x %in% c(3,14) & Seed2.y %in% c(6,11)) |
                                 (Seed2.x %in% c(7,10) & Seed2.y %in% c(2,15)) |
                                 (Seed2.y %in% c(1,16) & Seed2.x %in% c(8,9)) | 
                                 (Seed2.y %in% c(4,13) & Seed2.x %in% c(5,12)) |
                                 (Seed2.y %in% c(3,14) & Seed2.x %in% c(6,11)) |
                                 (Seed2.y %in% c(7,10) & Seed2.x %in% c(2,15)) , 2,
                               ifelse(SeedLoc.x == SeedLoc.y  &
                                        (Seed2.x %in% c(1,16, 8,9) & Seed2.y %in% c(4,13,5,12)) | 
                                        (Seed2.x %in% c(3,14, 6,11) & Seed2.y %in% c(7,10,2,15)) | 
                                        (Seed2.y %in% c(1,16, 8,9) & Seed2.x %in% c(4,13,5,12)) | 
                                        (Seed2.y %in% c(3,14, 6,11) & Seed2.x %in% c(7,10,2,15)), 3,
                                      ifelse(SeedLoc.x == SeedLoc.y &
                                               (Seed2.x %in% c(1,16,8,9,4,13,5,12) & Seed2.y %in% c(3,14,6,11,7,10,2,15)) |
                                               (Seed2.x %in% c(1,16,8,9,4,13,5,12) & Seed2.y %in% c(3,14,6,11,7,10,2,15)), 4,
                                             ifelse(SeedLoc.x != SeedLoc.y &
                                                      (SeedLoc.x == 'W' & SeedLoc.y == 'X') |
                                                      (SeedLoc.x == 'Y' & SeedLoc.y == 'Z') |
                                                      (SeedLoc.y == 'W' & SeedLoc.x == 'X') |
                                                      (SeedLoc.y == 'Y' & SeedLoc.x == 'Z'), 5,6))))))

###### Taking differences of two teams #########################
DiffCombo <- EachCombo2 %>% mutate(DSeed = Seed2.x - Seed2.y,
                                   Seed1 = Seed2.x,
                                   Seed2 = Seed2.y,
                                   DPts = MedPts.x - MedPts.y,
                                   DsdPts = sdPts.x - sdPts.y,
                                   DFG = FGPct.x - FGPct.y,
                                   DFG3 = FGPct3.x - FGPct3.y,
                                   DFT = FTPct.x - FTPct.y,
                                   DOR = MedOR.x - MedOR.y,
                                   DDR = MedDR.x - MedDR.y,
                                   DAst = MedAst.x - MedAst.y,
                                   DTO = MedTO.x - MedTO.y,
                                   DSTL = MedSTL.x - MedSTL.y,
                                   DBLK = MedBLK.x - MedBLK.y,
                                   DPF = MedPF.x - MedPF.y,
                                   DLast10 = Last10.x - Last10.y,
                                   DRecord = PercentRecord.x - PercentRecord.y,
                                   DOppRec = OpponentRecord.x - OpponentRecord.y,
                                   DPtDiff = MedPtDiff.x - MedPtDiff.y,
                                   DTPercent = TPercent.x - TPercent.y,
                                   DTGames = TGames.x - TGames.y) %>%
  select(-(Seed.x:SeedLoc.y)) %>% ungroup() %>%
  mutate('DTPercent' = ifelse(is.na(DTPercent), 0, DTPercent),
         'DTGames' = ifelse(is.na(DTGames), 0, DTGames),
         "DFT" = ifelse(is.na(DFT), 0, DFT))
View(DiffCombo %>% filter(Season == 2013, team2 == 1458))
############################### Variables to use in xgboost #####################
PredOutcome <- function(year, MaxD, Eta, Nrounds){
  year <- 2013
TrainDat <- DiffDat %>% filter(Season != year) %>%
  #select(-Season, -team, -team2, - Seed1, -Seed2, -DOutcome) 
  #select(Round, DOR, DLast10, DRecord, DOppRec, 
   #      DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK)
  select(-Season, -team, -team2, - Seed1, -Seed2, -DPts,
         -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
         -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -Round, -DSeedN, -DOutcome) 

#TrainDat <- DiffDat %>% 
#  select(-Season, -team, -team2, - Seed1, -Seed2, -DOutcome) 


Train <- model.matrix(~. -1, data = TrainDat, sparse = TRUE)

###### The Outcome of the trained model
OutcomeTrain <- unlist(DiffDat %>% filter(Season != year) %>%
                         select(DOutcome))

#OutcomeTrain <- unlist(DiffDat %>% 
#                         select(DOutcome))


####### Making Data into right matrix to use xgboost
#Dat <- data.matrix(UseVars)
###### Xgboost model
######################### Using CrossValidation Instead ###############
#bst <- xgb.cv(data = Train, label = OutcomeTrain, max_depth = 5, eta = .05,                
#               nfold = 3, metric = 'logloss',
#              nthread = 2, nrounds = 1000, objective = "binary:logistic",
 #             prediction = TRUE)

bst2 <- xgboost(data = Train, label = OutcomeTrain, max_depth = 6,
                eval_metric = 'logloss', 
               eta = .1, nthread = 2, nrounds = 200, 
               objective = "binary:logistic")
############################################################################
#### Getting the importance of the variables 
importance <- xgb.importance(feature_names = colnames(Train), model = bst2)
#RMVar <- importance$Feature[1:6]
###################### Predicting the test data #########################
PredData <- data.matrix(DiffDat %>%filter(Season == year) %>%
                          select(-Season, -team, -team2, - Seed1, -Seed2, -DPts, - DOutcome,
                                 -DOR, -DBLK, -DRecord, -DFT, -DPF, -DOppRec, -DAdjEM,
                                 -DOAdjEM, -DPtDiff, -DFG, -DsdPts, -DDR, -DSeed, -Round, -DSeedN))
### Predicted values
Pred <- predict(bst2, PredData)

Results <- DiffDat %>% filter(Season == year) %>% 
  select(DOutcome, Seed1,team, Seed2, team2) %>%
  mutate("Pred" = Pred, "PredOutcome" = ifelse(Pred > .5, 1, 0)) %>%
  mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
  left_join(Teams, by = c('team')) %>%
  left_join(Teams, by = c('team2' = 'team'))

PredRight <- Results %>% summarize(mean(RightWrong))
PredRight
#####################################################################
###### Predicting 2013-2016 #############
###### Data to use to predict
DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
  select(-Season, -team, -team2, -Seed1, - Seed2)
###### Matrix to use to predict
MatPred <- data.matrix(DatPred)
###### Getting Predictions
Pred <- data.frame(pred = predict(bst2, MatPred))
###### Combining Predictions and data
AttachDat <- DiffCombo %>% ungroup() %>% filter(Season == year)
PredOutput <- bind_cols(AttachDat, Pred) %>% 
          unite("id", Season:team2, sep = '_') %>%
  select(id, pred)


return(list("Table" = Results, "PredictionProb" = PredRight, "Output" = PredOutput))
}
Depth <- 3:10
Rounds <- 75:100
Expand <- expand.grid(Depth, Rounds)
Prob <- c()
for(i in 1:nrow(Expand)){
Y2013 <- PredOutcome(year = 2013, MaxD = Expand[i,1] , Eta =.1, Nrounds = Expand[i,2])
Prob[i] <- unlist(Y2013$PredictionProb)
}
Prob

Y2013 <- PredOutcome(year = 2013, MaxD = 3, Eta = .1, Nrounds = 75)
Output1 <- Y2013$Output
Y2013$PredictionProb
Y2014 <- PredOutcome(year = 2014, MaxD = 3, Eta = .1, Nrounds = 75)
Y2014$PredictionProb
Output2 <- Y2014$Output

Y2015 <- PredOutcome(year = 2015, MaxD = 3, Eta = .1, Nrounds = 75)
Y2015$PredictionProb
Output3 <- Y2015$Output

Y2016 <- PredOutcome(year = 2016, MaxD = 3, Eta = .1, Nrounds = 75)
Y2016$PredictionProb
Output4 <- Y2016$Output

OverallOutput <- rbind(Output1, Output2, Output3, Output4)

write.csv(OverallOutput, "~/Downloads/TourneyAttempt2", row.names = FALSE)


#####################################################################################
#####################################################################################






  
  
 
 













