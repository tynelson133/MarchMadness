library(dplyr)
library(reshape)
library(stringr)
library(lme4)
library(glmnet)
library(tidyr)
library(xgboost)

###### Data 
Teams <- read.csv('~/Downloads/Teams (1).csv')
Seasons <- read.csv("~/Downloads/Seasons (1).csv")
RegSeasonCompact <- read.csv("~/Downloads/RegularSeasonCompactResults (1).csv")
RegSeasonDetail <- read.csv("~/Downloads/RegularSeasonDetailedResults (1).csv")
TourneyCompact <- read.csv("~/Downloads/TourneyCompactResults (1).csv")
TourneyDetailed <- read.csv("~/Downloads/TourneyDetailedResults (1).csv")
TourneySeeds <- read.csv("~/Downloads/TourneySeeds (1).csv")
TourneySlots <- read.csv("~/Downloads/TourneySlots (1).csv")


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

########## Overall Data for years 2003-2016
OverallDat <- TourneyDat %>% left_join(SumSeasonDat, 
                                       by = c("Season", "team")) %>%
  mutate(SeedLoc = gsub("[0,1,2,3,4,5,6,7,8,9, a, b, c, d]", "", Seed)) %>%
  left_join(PomeroyStats, by = c('Season','team' = 'team.x', 'Seed2')) %>%
  select(-Team_Name)

########## Overall Data for 2017 ################
OverallDat2 <- TourneySeeds %>% inner_join(SumSeasonDat, 
                                       by = c("Season", "team")) %>%
  mutate(SeedLoc = gsub("[0,1,2,3,4,5,6,7,8,9, a, b, c, d]", "", Seed)) %>%
  left_join(PomeroyStats, by = c('Season','team' = 'team.x', 'Seed2')) %>%
  select(-Team_Name)

######################## Continuous Variables ############################
######## Getting Difference of each matchup in the tournaments

DataNeeded <- function(OverallDatNeed, year = 2003){
if(year != 2017){
DiffDat <- OverallDatNeed %>% group_by(ID) %>% arrange(ID,team)%>% 
  mutate('team2' = team, 'SeedTwo' = Seed2, "Seed3" = Seed2) %>%
  summarize(Season = Season[1],
            Round = Round[1],
            team = team[1],
            team2 = team2[2],
            Seed1 = Seed2[1],
            Seed2 = SeedTwo[2],
            DSeedN = Seed3[1] - Seed3[2],
            DSeed = ifelse(Seed3[1] - Seed3[2] > 0, 1,0),
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
            DTGames = TGames[1] - TGames[2],
            DAdjEM = AdjEM[1] - AdjEM[2],
            DAdjO = AdjO[1] - AdjO[2],
            DAdjD = AdjD[1] - AdjD[2],
            DAdjT = AdjT[1] - AdjT[2],
            DOAdjEM = OAdjEM[1] - OAdjEM[2],
            DOAdjO = OAdjO[1] - OAdjO[2],
            DOAdjD = OAdjD[1] - OAdjD[2],
            Dratio1 = (AdjD[1]/(OAdjO[1] + .001)) - 
              (AdjD[2]/(OAdjO[2] + .001)),
            Dratio2 = (AdjO[1]/(OAdjD[1] + .001)) - 
              (AdjO[2]/(OAdjD[2] + .001)),
            Dratio3 = MedAst[1]/MedTO[1] - MedAst[2]/MedTO[2]) %>%
  select(-ID) %>% na.omit()
}
#######################################################################
################################ Every Combination ####################
#######################################################################
########## To make every combination of every team playing each team in the tournament
ComboFunc <- lapply(unique(OverallDatNeed$Season), function(x){
  Foo <- unlist(OverallDatNeed %>% filter(Season == x) %>% select(team) %>% distinct())
  Combs <- t(combn(sort(Foo), 2))
  Year <- rep(x, nrow(Combs))
  cbind(Year, Combs)
}) 
####### Constructing a dataframe to connect all the season stats
EachCombo <- data.frame(do.call("rbind", ComboFunc))
colnames(EachCombo) <- c("Season", 'team', 'team2')
###### Getting the distinct team statistics for each year

if(year != 2017){
  DistOverallDat <- OverallDatNeed %>% select(-Daynum, -Outcome, -(fgm:Round),
                                              -team2.x, -PointDiff.x) %>%
    distinct() 
}

DistOverallDat <- OverallDatNeed 
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

DiffCombo <- EachCombo2 %>% mutate(DSeedN = Seed2.x - Seed2.y,
                                   DSeed = ifelse(Seed2.x - Seed2.y > 0, 1, 0),
                                   Seed1 = Seed2.x,
                                   Seed2 = Seed2.y,
                                   SeedLoc = SeedLoc.x,
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
                                   DTGames = TGames.x - TGames.y,
                                   DAdjEM = AdjEM.x - AdjEM.y,
                                   DAdjO = AdjO.x - AdjO.y,
                                   DAdjD = AdjD.x - AdjD.y,
                                   DAdjT = AdjT.x - AdjT.y,
                                   DOAdjEM = OAdjEM.x - OAdjEM.y,
                                   DOAdjO = OAdjO.x - OAdjO.y,
                                   DOAdjD = OAdjD.x - OAdjD.y,
                                   Dratio1 = (AdjD.x/(OAdjO.x + .001)) - 
                                     (AdjD.y/(OAdjO.y + .001)),
                                   Dratio2 = (AdjO.x/(OAdjD.x + .001)) - 
                                     (AdjO.y/(OAdjD.y + .001)),
                                   Dratio3 = MedAst.x/MedTO.x - MedAst.y/MedTO.y)%>%
  select(-(Seed.x:SeedLoc.y),-(AdjEM.y:OAdjD.y)) %>% ungroup() %>%
  mutate('DTPercent' = ifelse(is.na(DTPercent), 0, DTPercent),
         'DTGames' = ifelse(is.na(DTGames), 0, DTGames),
         "DFT" = ifelse(is.na(DFT), 0, DFT))
if(year != 2017){
return(list('DiffDat' = DiffDat, 'DiffCombo' = DiffCombo))
}

return('DiffCombo' = DiffCombo)

}

### 2003-2016
PrevYears <- DataNeeded(OverallDatNeed = OverallDat)
### 2017
Year2017 <- DataNeeded(OverallDatNeed = OverallDat2, year = 2017)


