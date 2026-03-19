library(dplyr)
library(xgboost)
library(ggplot2)
library(caret)
library(randomForest)




fivethirtyeight <- read.csv("538 Ratings.csv")
barthome <- read.csv("Barttorvik Home.csv")
bartaway <- read.csv("Barttorvik Away.csv")
bartneutral <- read.csv("Barttorvik Neutral.csv")
coach <- read.csv("Coach Results.csv")
conf <- read.csv("Conference Results.csv")
heat <- read.csv("Heat Check Tournament Index.csv")
kenpom <- read.csv("KenPom Barttorvik (2).csv")
resumes <- read.csv("Resumes.csv")
matchups <- read.csv("Tournament Matchups (2).csv")
results <- read.csv("Team Results.csv")
miya <- read.csv("EvanMiya.csv")

# use matchups dataset to create columns for winning seed, losing seed
# and such that matchups are in the same row

processed_matchups$lowseedteam[processed_matchups$lowseedteam == "Virginia "] <- "Virginia"
processed_matchups$highseedteam[processed_matchups$highseedteam == "Virginia "] <- "Virginia"
processed_matchups$lowseedteam[processed_matchups$lowseedteam == "Texas "] <- "Texas"
processed_matchups$highseedteam[processed_matchups$highseedteam == "Texas "] <- "Texas"


colnames(barthome)

kenpom26 <- kenpom %>%
  filter(YEAR == 2026)

data26 <- matchups %>%
  filter(YEAR == 2026)

#remove 2024 matchups

matchupsclean <- matchups %>% 
  filter(!is.na(matchups$SCORE))

# join all other data. should be roughly enough data for 50 or so columns
kenpomclean <- kenpom %>%
  filter(YEAR != 2026)

# select the win percentage as our tie breaker method for finals (1st seed vs 1st seed)

matchupsclean <- matchups %>%
  left_join(select(kenpomclean, TEAM, YEAR, WIN.), by = c("TEAM" = "TEAM", "YEAR" = "YEAR"))
tournament <- read.csv("2026_Potential_Matchups.csv")

tournament$year <- 2026
tournament$LowerSeedID <- as.character(tournament$LowerSeedID)
tournament$LowerSeedID <- as.character(tournament$LowerSeedID)

tournament26 <- tournament %>%
  left_join(abilities, by = c("LowerSeedID" = "team_id", "year")) %>%
  left_join(abilities, by = c("LowerSeedID" = "team_id", "year"))

tournament26 <- tournament26 %>%
  mutate(
    LowerSeed = case_when(
      LowerSeed == "TX Southern" ~ "Texas Southern",
      LowerSeed == "St Mary's CA" ~ "Saint Mary's",
      LowerSeed == "Col Charleston" ~ "College of Charleston",
      LowerSeed == "S Dakota St" ~ "South Dakota St",
      LowerSeed == "Kennesaw" ~ "Kennesaw St",
      LowerSeed == "TAM C. Christi" ~ "Texas A&M Corpus Chris",
      LowerSeed == "FL Atlantic" ~ "Florida Atlantic",
      LowerSeed == "MTSU" ~ "Middle Tennessee",
      LowerSeed == "St Joseph's PA" ~ "Saint Joseph's",
      LowerSeed == "Louisiana" ~ "Louisiana Lafayette",
      LowerSeed == "WKU" ~ "Western Kentucky",
      LowerSeed == "Ark Pine Bluff" ~ "Arkansas Pine Bluff",
      LowerSeed == "SUNY Albany" ~ "Albany",
      LowerSeed == "N Dakota St" ~ "North Dakota St",
      LowerSeed == "CS Fullerton" ~ "Cal St Fullerton",
      LowerSeed == "FGCU" ~ "Florida Gulf Coast",
      LowerSeed == "Northwestern LA" ~ "Northwestern St",
      LowerSeed == "ETSU" ~ "East Tennessee St",
      LowerSeed == "NC State" ~ "North Carolina St",
      LowerSeed == "E Washington" ~ "Eastern Washington",
      LowerSeed == "Southern Univ" ~ "Southern",
      LowerSeed == "F Dickinson" ~ "Fairleigh Dickinson",
      LowerSeed == "Loyola-Chicago" ~ "Loyola Chicago",
      LowerSeed == "NC Central" ~ "North Carolina Central",
      LowerSeed == "Ark Little Rock" ~ "Little Rock",
      LowerSeed == "Boston Univ" ~ "Boston University",
      LowerSeed == "E Kentucky" ~ "Eastern Kentucky",
      LowerSeed == "N Kentucky" ~ "Northern Kentucky",
      LowerSeed == "Abilene Chr" ~ "Abilene Christian",
      LowerSeed == "St Peter's" ~ "Saint Peter's",
      LowerSeed == "NC A&T" ~ "North Carolina A&T",
      LowerSeed == "St Louis" ~ "Saint Louis",
      LowerSeed == "CS Northridge" ~ "Cal St Northridge",
      LowerSeed == "G Washington" ~ "George Washington",
      LowerSeed == "Mt St Mary's" ~ "Mount St Mary's",
      LowerSeed == "Grambling" ~"Grambling St",
      LowerSeed == "SIUE" ~ "SIU Edwardsville",
      LowerSeed == "NE Omaha" ~"Nebraska Omaha",
      LowerSeed == "SF Austin" ~ "Stephen F. Austin",
      LowerSeed == "UT San Antonio" ~ "UTSA",
      LowerSeed == "CS Bakersfield" ~ "Cal St Bakersfield",
      LowerSeed == "N Colorado" ~ "Northern Colorado",
      LowerSeed == "W Michigan" ~ "Western Michigan",
      LowerSeed == "American Univ" ~ "American",
      LowerSeed == "WI Green Bay" ~ "Green Bay",
      LowerSeed == "MS Valley St" ~ "Mississippi Valley St",
      LowerSeed == "Kent" ~ "Kent St",
      LowerSeed == "WI Milwaukee" ~ "Milwaukee",
      LowerSeed == "Coastal Car" ~ "Coastal Carolina",
      TRUE ~ LowerSeed
    )
  )

kenpom26$TEAM <- gsub("\\bSt\\.", "St", kenpom26$TEAM)
kenpom$highseedteam <- gsub("\\bSt\\.", "St", kenpom$highseedteam)
kenpom26$TEAM[kenpom26$TEAM == "Queens"] <- "Queens NC"
kenpom26$TEAM[kenpom26$TEAM == "Prairie View A&M"] <- "Prairie View"



#matchups are in groups of two rows. we can create matchup ids for every two rows
# to help with our other operations in the next step
matchupsclean <- matchupsclean %>%
  mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2))

#now that we have grouped by matchup id, we have all matchups in one row, 
processed_matchups <- matchupsclean %>%
  group_by(matchup_id) %>%
  summarise(year = first(YEAR),
            round = first(CURRENT.ROUND), 
            highseedteam = if_else(SEED[1] == SEED[2],
                                   if_else(WIN.[1] > WIN.[2], TEAM[1], TEAM[2]), 
                                   TEAM[which.min(SEED)]),
            lowseedteam = if_else(SEED[1] == SEED[2],
                                  if_else(WIN.[1] > WIN.[2], TEAM[2], TEAM[1]), 
                                  TEAM[which.max(SEED)]),
            highseed = min(SEED),
            lowseed = max(SEED),
            highseedno = TEAM.NO[which.min(SEED)], 
            lowseedno = TEAM.NO[which.max(SEED)],
            highseedscore = if_else(SEED[1] == SEED[2],
                                    if_else(WIN.[1] > WIN.[2], SCORE[1], SCORE[2]), 
                                    SCORE[which.min(SEED)]),
            lowseedscore = if_else(SEED[1] == SEED[2],
                                   if_else(WIN.[1] > WIN.[2], SCORE[2], SCORE[1]), 
                                   SCORE[which.max(SEED)]),
            highseed_win = as.integer(SCORE[which.min(SEED)] > SCORE[which.max(SEED)]))


# we want to left join the data here: keep all data from matchups df, and wherever the join function finds a
# match, include the advanced stats data from kenpomclean. don't want to lose any rows from original df
highercombined <- merge(processed_matchups, kenpom, by.x =c("highseedteam", "year"), by.y = c("TEAM", "YEAR"), all.x = TRUE)
highercombined <- merge(tournament26, kenpom, by.x =c("highseedteam", "year"), by.y = c("TEAM", "YEAR"), all.x = TRUE)

highercombined <- tournament26 %>%
  left_join(kenpom26, by = c("HigherSeed" = "TEAM"))

adv <- names(winnerscombinedclean)[28:120]


winnerscombinedclean <- highercombined %>%
  select(year, round, CONF,  highseedteam, lowseedteam, highseed, lowseed, highseedno, lowseedno,
         highseedscore, lowseedscore, highseed_win,KADJ.T, KADJ.O,
         KADJ.D, KADJ.EM,  BARTHAG, WIN., ELITE.SOS, WAB,
         EFG., EFG.D, FTR, FTRD, TOV., TOV.D, OREB., DREB., OP.OREB., PPPO, PPPD, AVG.HGT, EFF.HGT, 
         EXP, TALENT, X2PT., X2PTR, X2PTRD, X3PT., X3PTR, X3PTRD)

winnerscombinedclean <- highercombined[1:120]
winnerscombinedclean <- winnerscombinedclean %>%
  select(-matches(".RANK"))
winnerscombinedclean <- winnerscombinedclean %>%
  select(-CONF, -CONF.ID, -QUAD.NO, -QUAD.ID, -TEAM.NO, -TEAM.ID, -SEED, -ROUND)

names(winnerscombinedclean)[names(winnerscombinedclean) %in% adv] <- paste("higher", names(winnerscombinedclean)[names(winnerscombinedclean) %in% adv], sep = "_")


lowercombined <- tournament26 %>%
  left_join(kenpom26, by = c("LowerSeed" = "TEAM"))

lowercombined <- merge(processed_matchups, kenpom, by.x =c("lowseedteam", "year"), by.y = c("TEAM", "YEAR"), all.x = TRUE)


loserscombinedclean <- lowercombined %>%
  select(year, round, CONF,  highseedteam, lowseedteam, highseed, lowseed, highseedno, lowseedno,
         highseedscore, lowseedscore, highseed_win,KADJ.T, KADJ.O,
         KADJ.D, KADJ.EM,  BARTHAG, WIN., ELITE.SOS, WAB,
         EFG., EFG.D, FTR, FTRD, TOV., TOV.D, OREB., DREB., OP.OREB., PPPO, PPPD, AVG.HGT, EFF.HGT, 
         EXP, TALENT, X2PT., X2PTR, X2PTRD, X3PT., X3PTR, X3PTRD)
loserscombinedclean <- lowercombined[ 1:120]
loserscombinedclean <- loserscombinedclean %>%
  select(-matches(".RANK"))
loserscombinedclean <- loserscombinedclean %>%
  select(-CONF, -CONF.ID, -QUAD.NO, -QUAD.ID, -TEAM.NO, -TEAM.ID, -SEED, -ROUND)

names(loserscombinedclean)[names(loserscombinedclean) %in% adv] <- paste("lower", names(loserscombinedclean)[names(loserscombinedclean) %in% adv], sep = "_")

loserscombinedclean <- loserscombinedclean %>%
  select(-round,  -highseedteam, -lowseedteam, -highseed, -lowseed, -highseedno,
         -lowseedno, -highseedscore, -lowseedscore)

adv <- names(loserscombinedclean)[28:120]

finalfeature <- cbind(winnerscombinedclean, loserscombinedclean)
finalfeature <- finalfeature %>%
  rename_with(~ gsub("^higher_", "", .x))
finalfeature <- gsub("^lower_", "", finalfeature)

for(i in 20:68){
  finalfeature[[paste0("diff_", names(finalfeature)[i])]] <- finalfeature[[i]] - finalfeature[[i + 68]]
}
111-82
finalfeatures <- finalfeature %>%
  select(1:13, 82:111)

finalpredictfeatures <- (finalfeature)
colnames(finalfeature)
finalfeature <- finalfeature[, !duplicated(colnames(finalfeature))]



finalfeature <- finalfeature %>%
  select(-matchup_id)

finalfeatures <- finalfeature %>%
  select(-higher_GAMES, -higher_W, -higher_L, -lower_GAMES, -lower_W, -lower_L)

finalfeature$year.1 <- NULL
finalfeature$highseed_win.1 <- NULL
finalfeature <- finalfeature %>%
  select(!duplicated(names(.)))

finalfeature <- finalfeature %>%
  select(-`year.1`)
finalfeature <- finalfeature %>%
  distinct(year, .keep_all = TRUE)

finalfeature <- finalfeature %>%
  distinct(highseed_win, .keep_all = TRUE)

finalfeatures$lowseedteam <- gsub("\\bSt\\.", "St", finalfeatures$lowseedteam)
finalfeatures$highseedteam <- gsub("\\bSt\\.", "St", finalfeatures$highseedteam)

finalfeatures <- finalfeatures %>%
  filter(year != 2026)
 


traindata <- finalfeatures %>%
  filter(!year %in% c( 2025))

testdata <- finalfeatures %>%
  filter(year %in% c( 2025))


finalfeatures$lowseedteam[finalfeatures$lowseedteam == "Virginia "] <- "Virginia"
finalfeatures$highseedteam[finalfeatures$highseedteam == "Virginia "] <- "Virginia"

finalpredictfeatures <- finalpredictfeatures %>%
  rename(ability1 = ability.x, ability2 = ability.y) %>%
  mutate(
    ability_diff = ability1 - ability2
  ) %>%
  select(-ability1, -ability2, -se.x, -se.y)

predictfeatures <- finalfeatures %>%
  select(all_of(selected_features))

print(selected_features)

traindata <- traindata %>%
  left_join(abilities, by = c("highseedteam" = "TeamName", "year" )) %>%
  left_join(abilities, by = c("lowseedteam" = "TeamName", "year")) %>%
  rename(ability1 = ability.x, ability2 = ability.y) %>%
  mutate(
    ability_diff = ability1 - ability2
  ) %>%
  select(-ability1, -ability2, -se.x, -se.y)

testdata <- testdata %>%
  left_join(abilities, by = c("highseedteam" = "TeamName", "year" )) %>%
  left_join(abilities, by = c("lowseedteam" = "TeamName", "year")) %>%
  rename(ability1 = ability.x, ability2 = ability.y) %>%
  mutate(
    ability_diff = ability1 - ability2
  ) %>%
  select(-ability1, -ability2, -se.x, -se.y)




abilities <- abilities %>%
  mutate(
    TeamName = case_when(
      TeamName == "TX Southern" ~ "Texas Southern",
      TeamName == "St Mary's CA" ~ "Saint Mary's",
      TeamName == "Col Charleston" ~ "College of Charleston",
      TeamName == "S Dakota St" ~ "South Dakota St",
      TeamName == "Kennesaw" ~ "Kennesaw St",
      TeamName == "TAM C. Christi" ~ "Texas A&M Corpus Chris",
      TeamName == "FL Atlantic" ~ "Florida Atlantic",
      TeamName == "MTSU" ~ "Middle Tennessee",
      TeamName == "St Joseph's PA" ~ "Saint Joseph's",
      TeamName == "Louisiana" ~ "Louisiana Lafayette",
      TeamName == "WKU" ~ "Western Kentucky",
      TeamName == "Ark Pine Bluff" ~ "Arkansas Pine Bluff",
      TeamName == "SUNY Albany" ~ "Albany",
      TeamName == "N Dakota St" ~ "North Dakota St",
      TeamName == "CS Fullerton" ~ "Cal St Fullerton",
      TeamName == "FGCU" ~ "Florida Gulf Coast",
      TeamName == "Northwestern LA" ~ "Northwestern St",
      TeamName == "ETSU" ~ "East Tennessee St",
      TeamName == "NC State" ~ "North Carolina St",
      TeamName == "E Washington" ~ "Eastern Washington",
      TeamName == "Southern Univ" ~ "Southern",
      TeamName == "F Dickinson" ~ "Fairleigh Dickinson",
      TeamName == "Loyola-Chicago" ~ "Loyola Chicago",
      TeamName == "NC Central" ~ "North Carolina Central",
      TeamName == "Ark Little Rock" ~ "Little Rock",
      TeamName == "Boston Univ" ~ "Boston University",
      TeamName == "E Kentucky" ~ "Eastern Kentucky",
      TeamName == "N Kentucky" ~ "Northern Kentucky",
      TeamName == "Abilene Chr" ~ "Abilene Christian",
      TeamName == "St Peter's" ~ "Saint Peter's",
      TeamName == "NC A&T" ~ "North Carolina A&T",
      TeamName == "St Louis" ~ "Saint Louis",
      TeamName == "CS Northridge" ~ "Cal St Northridge",
      TeamName == "G Washington" ~ "George Washington",
      TeamName == "Mt St Mary's" ~ "Mount St Mary's",
      TeamName == "Grambling" ~"Grambling St",
      TeamName == "SIUE" ~ "SIU Edwardsville",
      TeamName == "NE Omaha" ~"Nebraska Omaha",
      TeamName == "SF Austin" ~ "Stephen F. Austin",
      TeamName == "UT San Antonio" ~ "UTSA",
      TeamName == "CS Bakersfield" ~ "Cal St Bakersfield",
      TeamName == "N Colorado" ~ "Northern Colorado",
      TeamName == "W Michigan" ~ "Western Michigan",
      TeamName == "American Univ" ~ "American",
      TeamName == "WI Green Bay" ~ "Green Bay",
      TeamName == "MS Valley St" ~ "Mississippi Valley St",
      TeamName == "Kent" ~ "Kent St",
      TeamName == "WI Milwaukee" ~ "Milwaukee",
      TeamName == "Coastal Car" ~ "Coastal Carolina",
      TRUE ~ TeamName
    )
  )



traindataclean <- traindata %>%
  select(1:13, 122:171, 178)
  
testdataclean <- testdata %>%
  select(1:13, 122:171, 178)

add <- testdataclean %>%
  filter(year == 2023 | year == 2024)

traindataclean <- rbind(traindataclean, add)

testdataclean <- testdataclean %>%
  filter(year == 2025)


X_train <- traindataclean %>%
  select(15:64)

x_test <- testdataclean %>%
  select(15:64)

ShapTrainX <- X_train[, selected_features, drop = FALSE]
ShapTestX <- x_test[, selected_features, drop = FALSE]

X_train <- as.matrix(X_train)
y_train <- traindataclean$highseed_win

y_test <- testdataclean$highseed_win
x_test <- as.matrix(x_test)

y_train <- as.numeric(y_train)  # Converts TRUE/FALSE to 1/0
y_train <- as.numeric(as.character(y_train))  # If it's a factor, this prevents misclassification

library(xgboost)
dtrain <- xgb.DMatrix(data = ShapTrainX, label = y_train)
dtest <- xgb.DMatrix(data = ShapTestX, label = y_test)
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",   # or "auc"
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(2015)
cv_model <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 20,
  print_every_n = 10,
  verbose = 1
)

best_nrounds <- cv_model$best_iteration
best_nrounds


set.seed(2015)
# Train the model
xgboost <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 1
)

predictfeatures <- finalpredictfeatures %>%
  select(133:182)
preds <- predict(xgboost, dtest)
pred_matrix <- as.matrix(predictfeatures)
pred_matrix <- as.matrix(predictfeatures[, selected_features, drop = FALSE])
dpreds <- xgb.DMatrix(data = pred_matrix)
pred26 <- predict(xgboost, dpreds)

print(finalfeatures)
preds26 <- cbind(finalpredictfeatures, pred26)

preds26 <- preds26 %>%
  select(year, HigherSeed, HigherSeedID, HigherSeedNum, LowerSeed,  LowerSeedID, LowerSeedNum, LowerSeedNum, ability_diff, pred26)
brier_score <- mean((preds - y_test)^2)
print(brier_score)
  library(caret)
  
  # predicted labels (threshold 0.5)
  pred_labels <- ifelse(preds >= 0.5, 1, 0)
  
  # confusion matrix automatically gives all metrics
  conf_mat <- confusionMatrix(factor(pred_labels), factor(y_test), positive = "1")
  
  # metrics
  conf_mat$overall["Accuracy"]       # accuracy
  conf_mat$byClass[c("Precision","Recall","F1")]  # precision, recall, F1
  
log_loss <- -mean(y_test * log(preds) + (1 - y_test) * log(1 - preds))
print(log_loss)

write.csv(preds26, "2026Preds.csv", row.names = FALSE)

library(SHAPforxgboost)
library(dplyr)
shap_values <- shap.values(xgb_model = xgboost, X_train = as.matrix(X_train))

# extract SHAP matrix
shap_matrix <- shap_values$shap_score  # rows = samples, cols = features
shap_mean <- shap_values$mean_shap_score
shap_mean

mean_shap_df <- data.frame(
  feature = names(shap_mean),
  mean_shap = shap_mean
)

selected_features <- mean_shap_df %>%
  dplyr::filter(mean_shap >= 0.03) %>%
  dplyr::pull(feature)

print(selected_features)


matchups2025 <- read.csv("competition_submission.csv")

newkenpom <- read.csv("KenPom Barttorvik (1).csv")

newkenpom <- kenpom %>%
  filter(YEAR == 2026)


newkenpom$TEAM[newkenpom$TEAM == "UC San Diego"] = "San Diego"
newkenpom$TEAM[newkenpom$TEAM == "Nebraska Omaha"] = "Omaha"
newkenpom$TEAM[newkenpom$TEAM == "SIU Edwardsville"] = "SIUE"

data2026 <- data2026 %>%
  filter(YEAR == 2026)
mensmatchups <- left_join(data2026, kenpom, by = c("higher_seed" = "TEAM"))

matchupscombined <- left_join(mensmatchups, newkenpom, by = c("lower_seed" = "TEAM"))




names(mensmatchups)[names(mensmatchups) %in% adv] <- paste("higher", names(mensmatchups)[names(mensmatchups) %in% adv], sep = "_")
names(newkenpom)[names(newkenpom) %in% adv] <- paste("lower", names(newkenpom)[names(newkenpom) %in% adv], sep = "_")

matchupscombined <- matchupscombined %>%
  select(-matches(".RANK"))

matchupscombinedadj <- matchupscombined %>%
  rename_with(~ gsub("(.+)\\.x$", "higher_\\1", .x)) %>%
  rename_with(~ gsub("(.+)\\.y$", "lower_\\1", .x))



matchupscombinedclean <- matchupscombinedadj %>%
  select(-higher_CONF, -higher_CONF.ID, -higher_QUAD.NO, -higher_QUAD.ID, -higher_TEAM.NO,  -higher_SEED, -higher_ROUND,
         -lower_CONF, -lower_CONF.ID, -lower_QUAD.NO, -lower_QUAD.ID, -lower_TEAM.NO, -lower_SEED, -lower_ROUND)

summary(matchupscombinedclean)

setdiff(colnames(twentyfiveff), colnames(traindataclean))



twentyfiveff <- matchupscombinedclean %>%
  select(-X, -higher_seed, -lower_seed, -lower_seed_num, -lower_record,  -higher_TEAM.ID, -higher_YEAR, -lower_YEAR, -lower_TEAM.ID,
         -higher_W, -higher_L, -lower_W, -lower_L, -higher_seed_num, -higher_record, -higher_GAMES, -lower_GAMES,
         -higher_OP.DREB., -higher_RAW.T, -higher_X2PT.D, -higher_X3PT.D, -higher_BLK., -higher_BLKED.,   
         -higher_AST., -higher_OP.AST., -higher_FT., -higher_OP.FT.,-higher_ELITE.SOS, -higher_WAB,      
          -lower_OP.DREB.,  -lower_RAW.T,   -lower_X2PT.D,-lower_X3PT.D, -lower_BLK., -lower_BLKED.,    
          -lower_AST.,  -lower_OP.AST., -lower_FT., -lower_OP.FT., -lower_ELITE.SOS,  -lower_WAB)

names(dtrain)

x_test <- testdataencoded %>%
  select(-highseed_win)  # Ensure this is your features without the outcome variable

t <- as.matrix(twentyfiveff)

colnames(t) <- colnames(X_train)

t <- t[, colnames(X_train), drop = FALSE]

preds_2025 <- predict(xgboost, t)



testdata$result_predictions <- preds_2024

matchupscombinedclean$preds <- preds_2025

summary(matchupscombinedclean)

twentyfivepredictions <- matchupscombinedclean %>%
  select(higher_seed, higher_seed_num, lower_seed, lower_seed_num, preds)

upsets <- twentyfivepredictions %>%
  filter(preds <= 0.5)
oldupsets <- twentyfourpredictions %>%
  filter(preds <= 0.5)

filtered_dataset <- subset(testdatapreds, preds >= 0.68 & preds <= 0.70)

brier_score <- mean((testdatapreds$preds - filtered_dataset$highseed_win)^2)
print(brier_score)

twentyonebracket <- testdatapreds %>%
  filter(year == 2021)

twentytwobracket <- testdatapreds %>%
  filter(year == 2022)
twentythreebracket <- testdatapreds %>%
  filter(year == 2023)

brier_score <- mean((twentythreebracket$preds - twentythreebracket$highseed_win)^2)
print(brier_score)


brier_score <- mean((twentytwobracket$preds - twentytwobracket$highseed_win)^2)
print(brier_score)

brier_score <- mean((twentyonebracket$preds - twentyonebracket$highseed_win)^2)
print(brier_score)

