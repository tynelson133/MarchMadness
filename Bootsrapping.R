BootStrapFunc <- function(year, samplesize, vars){
  
  TrainDat <- DiffDat %>% filter(Season != year) %>%
    select(one_of(vars), DOutcome, DSeed) %>%
    #select(Round, DOR, DLast10, DRecord, DOppRec, 
    #       DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK, DOutcome, DSeed) %>% 
    sample_n(samplesize)

  ####### Test Data
  TestDat <- DiffDat %>%filter(Season == year) %>%
    select(one_of(vars), DSeed)
    #select(Round, DOR, DLast10, DRecord, DOppRec, 
     #      DPtDiff, DTPercent, DTGames, DFG, DDR, DAst, DBLK, DSeed)
  
  ########## MultiLevel Model 
  GLogisticMod <- glmer(paste('DOutcome ~', paste(vars, collapse="+"), '+(1 | DSeed)'),
                         family = 'binomial', data = TrainDat)
  ########## Multilevel Predictions
  Pred1 <- predict(GLogisticMod, TestDat, type = 'response', allow.new.levels=TRUE)
  
  ########## Logistic Model
  TrainDat2 <- TrainDat %>% select(-DSeed)
  TestDat2 <- TestDat %>% select(-DSeed)
  LogisticMod <- glm(DOutcome ~., family = 'binomial', data = TrainDat2)
  #### Prediction
  Pred2 <- predict(LogisticMod, TestDat2, type = 'response')
  
  
  ########## Ridge Regression
  TrainDatRidge <- data.matrix(TrainDat %>% select(-DOutcome, -DSeed))
  TrainOutcome <- unlist(TrainDat2 %>%select(DOutcome))
  ###### Model
  cvfit <- cv.glmnet(TrainDatRidge, TrainOutcome, type.measure = "class", family = 'binomial',
                     alpha = 0)
  
  ##### Prediction
  Pred3 <- predict(cvfit, data.matrix(TestDat2), s = cvfit$lambda.min, type="response")
  
  ##### Average of Predictions
  PredDat <- data.frame(Pred1, Pred2, Pred3)
  
  AvgPred <- rowMeans(PredDat)
  
 ####################### Predicting All Games ####################### 
  ###### Getting Predictions
  DatPred <- DiffCombo %>% ungroup() %>% filter(Season == year) %>%
    select(one_of(vars), DSeed)
  Pred21 <- predict(GLogisticMod, DatPred, type = 'response', allow.new.levels=TRUE)
  Pred22 <- predict(LogisticMod, DatPred %>% select(-DSeed) , type = 'response')
  Pred23 <- predict(cvfit, data.matrix(DatPred %>% select(-DSeed)), 
                    s = cvfit$lambda.min, type="response")
  OverallPred <- data.frame(Pred21, Pred22, Pred23)
  OverallPredAvg <- rowMeans(OverallPred)

  
  return(list("AvgPred" = AvgPred, 'AvgOverall' = OverallPredAvg))
}

Sim <- list()
OverallSim <- list()
for(i in 1:100){ 
  
  Foo <- BootStrapFunc(year = 2014, samplesize = 150,
                            #vars = c('DOR', 'Round', 'DRecord', 'DPtDiff', 
                             #        'DTGames', 'DDR', 'DLast10', 'DFG',
                              #       'DAst', 'DBLK', 'DOppRec'))
                       #vars = c('DOR', 'DRecord', 'DDR', 'DFG', 'DOppRec')
                       vars = c('DLast10', 'DOppRec', 'DTPercent', 
                                'DAdjEM', 'DAdjO', 'DAdjD', 'DOAdjO', 'DOAdjD'))
  Sim[[i]] <- Foo$AvgPred
  OverallSim[[i]] <- Foo$AvgOverall
}

BootVals <- do.call('cbind', Sim)
BootOverall <- do.call('cbind', OverallSim)

hist(BootVals[2,])
MedPred <- apply(BootVals, 1, median)

Results <- DiffDat %>% filter(Season == 2014) %>% 
  select(DOutcome, Seed1,team, Seed2, team2, Round) %>%
  mutate("Pred" = MedPred, "PredOutcome" = ifelse(MedPred > .5, 1, 0)) %>%
  mutate("RightWrong" = ifelse(DOutcome == PredOutcome,1,0)) %>%
  left_join(Teams, by = c('team')) %>%
  left_join(Teams, by = c('team2' = 'team'))  %>%
  #filter(Pred > .6 | Pred <.4) %>%
  arrange(Round)

View(Results)

mean(Results$RightWrong)

MedOverall <- apply(BootOverall, 1, median)

############################ 2013
Y2013 <- DiffCombo %>%filter(Season == 2013) %>% 
  select(Season, Seed1,team, Seed2, team2, SeedLoc) %>%
  mutate("Pred" = MedOverall, "PredOutcome" = ifelse(MedOverall > .5, 1, 0)) %>%
  unite("id", c(Season, team, team2), sep = '_') %>%
  select(id, Pred)
############################
Y2014 <- DiffCombo %>%filter(Season == 2014) %>% 
  select(Season, Seed1,team, Seed2, team2, SeedLoc) %>%
  mutate("Pred" = MedOverall, "PredOutcome" = ifelse(MedOverall > .5, 1, 0)) %>%
  unite("id", c(Season, team, team2), sep = '_') %>%
  select(id, Pred)

############################
Y2015  <- DiffCombo %>%filter(Season == 2015) %>% 
  select(Season, Seed1,team, Seed2, team2, SeedLoc) %>%
  mutate("Pred" = MedOverall, "PredOutcome" = ifelse(MedOverall > .5, 1, 0)) %>%
  unite("id", c(Season, team, team2), sep = '_') %>%
  select(id, Pred)

############################
Y2016  <- DiffCombo %>%filter(Season == 2016) %>% 
  select(Season, Seed1,team, Seed2, team2, SeedLoc) %>%
  mutate("Pred" = MedOverall, "PredOutcome" = ifelse(MedOverall > .5, 1, 0)) %>%
  unite("id", c(Season, team, team2), sep = '_') %>%
  select(id, Pred)

Submition7 <- rbind(Y2013, Y2014, Y2015, Y2016)
write.csv(Submition7, "~/Downloads/TourneyAttempt7", row.names = FALSE)


Round1Winners1 <- Results %>% filter(Round ==1, PredOutcome == 1) %>%
  select(Seed1, team, SeedLoc) 
Round1Winners2 <- Results %>% filter(Round ==1, PredOutcome == 0) %>%
  select(Seed1 = Seed2, team = team2, SeedLoc) 
Winners <- rbind(data.frame(Round1Winners1), data.frame(Round1Winners2))
Winners %>% arrange(SeedLoc, team)
AH <- ResultsOverall %>% filter(Round == 2,
                          (team %in% Round1Winners1$team   | 
                             team %in% Round1Winners2$team) &
                            (team2 %in% Round1Winners1$team | 
                               team2 %in% Round1Winners2$team)) %>% 
  arrange(SeedLoc)
View(AH)                

NewRes <- Results2 %>% left_join(ResultsOverall, by = c('team', 'team2'))
View(NewRes)

View(ResultsOverall %>% filter(team == 1112 | team2 == 1112, SeedLoc == 'Z'))


####################### Doing Variable Selection #############
library(bestglm)
colnames(DiffDat)
VarSelection <- DiffDat %>% filter(Season != year) %>%
  select(DLast10, DRecord, DOppRec, 
       DPtDiff, DTPercent, DTGames, DSeed,
       DAdjEM,DAdjO,DAdjD,DAdjT,DOAdjO,DOAdjD,y = DOutcome)
View(VarSelection)
res.glm <- bestglm(Xy = data.frame(VarSelection),
                   family = binomial,
                   IC = "AIC")

res.glm$BestModels

#c('DOR', 'Round', 'DRecord', 'DPtDiff', 'DTGames', 'DDR')