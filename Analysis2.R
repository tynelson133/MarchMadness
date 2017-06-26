Teams <- read.csv('~/Downloads/Teams.csv')
Seasons <- read.csv("~/Downloads/Seasons.csv")
RegSeasonCompact <- read.csv("~/Downloads/RegularSeasonCompactResults.csv")
RegSeasonDetail <- read.csv("~/Downloads/RegularSeasonDetailedResults.csv")
TourneyCompact <- read.csv("~/Downloads/TourneyCompactResults.csv")
TourneyDetailed <- read.csv("~/Downloads/TourneyDetailedResults.csv")
TourneySeeds <- read.csv("~/Downloads/TourneySeeds.csv")
TourneySlots <- read.csv("~/Downloads/TourneySlots.csv")

##### Removing Seasons before 2003
Seasons <- Seasons %>% filter(Season >= 2003)
##### Changing Team column name in Tourney Seeds and also creating a seed with just seed
## NUmber
TourneySeeds <- TourneySeeds %>% 
  dplyr::rename(team = Team) %>%
  mutate(Seed2 = as.numeric(gsub("[W,X,Y,Z,a,b,c]", "", Seed)))
View(TourneySeeds)
######## Getting Summary statistics of every team

#### Using data created from Analysis
NewTourneyCompact <- ChangeDat(TourneyCompact) %>% filter(Season >= 2003)
NewTourneyDetail <- ChangeDat(TourneyDetailed) %>% filter(Season >= 2003)
NewRegSeasonDetail <- ChangeDat(RegSeasonDetail) %>% filter(Season >= 2003)
NewRegSeasonCompact <- ChangeDat(RegSeasonCompact) %>% filter(Season >= 2003)



##### Summary of all teams stats 
SumSeasonDat <- SeasonDat  %>%
                group_by(Season, team) %>% 
  summarize("MedPts" = median(score), "sdPts" = sd(score),
            "FGPct" = median(fgm/fga), "FGPct3" = median(fgm3/fga3),
            "FTPct" = median(ftm/fta), "MedOR" = median(or),
            "MedDR" = median(dr), "MedAst" = median(ast),
            "MedTO" = median(to), "MedSTL" = median(stl),
            "MedBLK" = median(blk), "MedPF" = median(pf))

#### JUst seed and team identifier 
TeamDat <- NewTourneyCompact %>% left_join(TourneySeeds, by = c("Season", "team")) %>%
            group_by(Season) %>% filter(!duplicated(team)) %>%
            select(Season, team, Seed2) %>%
            left_join(SumSeasonDat, by = c("Season", "team")) 
            

colnames(TeamDat)
foo <- lapply(2003:2016, function(x){
  
  dat <- TeamDat %>% filter(Season == x) %>% select(team)
  
  t(combn(unlist(dat[,2]),2))
  
})


duh1 <- TeamDat %>% filter(Season == 2003, team == foo[[1]][1,1])
duh2 <- TeamDat %>% filter(Season == 2003, team == foo[[1]][1,2])
duh1 - duh2
head(foo[[1]])

#################### Looking at differences of matchups

#TourneyLarge <- TourneyCompact %>% 

          

###########################################################################
SumSeasonDat <- RegSeasonDetail %>%
  group_by(Season, team) %>% 
  summarize("MedPts" = median(score), "sdPts" = sd(score),
            "FGPct" = median(fgm/fga), "FGPct3" = median(fgm3/fga3),
            "FTPct" = median(ftm/fta), "MedOR" = median(or),
            "MedDR" = median(dr), "MedAst" = median(ast),
            "MedTO" = median(to), "MedSTL" = median(stl),
            "MedBLK" = median(blk), "MedPF" = median(pf))

colnames(TourneySeeds)
TourneyLarge <- TourneyCompact %>% filter(Season >= 2003) %>%
  left_join(TourneySeeds, by = c('Season', 'Wteam' = 'team')) %>%
  left_join(TourneySeeds, by = c('Season', 'Lteam' = 'team')) %>%
  left_join(SumSeasonDat, by = c('Season', 'Wteam' = 'team')) %>%
  left_join(SumSeasonDat, by = c('Season', 'Lteam' = 'team')) %>%
  mutate("PtDif" = MedPts.x - MedPts.y,
         "sdDif" = sdPts.x - sdPts.y,
         "FGDif" = FGPct.x - FGPct.y,
         "FG3Dif" = FGPct3.x - FGPct3.y,
         "FTDif" = FTPct.x - FTPct.y,
         "ORDif" = MedOR.x - MedOR.y,
         "DRDif" = MedDR.x - MedDR.y,
         "ASTDif" = MedAst.x - MedAst.y,
         "TODif" = MedTO.x - MedTO.y,
         "STLDif" = MedSTL.x - MedSTL.y,
         "BLKDif" = MedBLK.x - MedBLK.y,
         "PFDif" = MedPF.x - MedPF.y, 
         "SeedDif" = Seed2.x - Seed2.y)

ifelse(Wteam > Lteam, )


