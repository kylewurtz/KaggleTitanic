# ---- load packages ----
# general
require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)
require(readr)
require(Amelia)
require(ggvis)
require(scales)

# modeling
require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)

# ---- read in train data set ----
train = read_csv("../KaggleTitanic/Data/train.csv")

# take an initial look at data set
glimpse(train)
summary(train)

# map missing data
missmap(train)

# check survival rate overall
prop.table(table(train$Survived))

# ---- read in test data set ----
test = read_csv("../KaggleTitanic/Data/test.csv")

# take an initial look at the data set
glimpse(test)

# ---- visualizations ----
# see distribution of survived
train %>% 
  mutate(Survived = factor(Survived, labels = c("Died","Lived"))) %>% 
  ggvis(~Survived) %>% layer_bars()

# distribution of class and survival by class
train %>% 
  mutate(Pclass = factor(Pclass, labels = c("First","Second","Third"))) %>% 
  ggvis(~Pclass) %>% layer_bars()
train %>% 
  mutate(Pclass = factor(Pclass, labels = c("First","Second","Third"))) %>% 
  group_by(Pclass) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~Pclass, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: higher class more likely to survive

# distribution of sex and survival by class
train %>% ggvis(~Sex) %>% layer_bars()
train %>% 
  group_by(Sex) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~Sex, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: women much more likely to survive

# distribution of age and survival by age
train %>% ggvis(~Age) %>% layer_histograms()
train %>% 
  mutate(AgeBucket = cut(Age, seq(0,100,10))) %>% 
  group_by(AgeBucket) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~AgeBucket, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: babies/children much more likely to survive;
# survival rate flat for young adults up through 60;
# elderly (60+) much less ikely to survive

# distribution of siblings and survival by number of siblings
train %>% ggvis(~SibSp) %>% layer_bars()
train %>% 
  group_by(SibSp) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~SibSp, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: nothing dramatic here...

# distribution of parents/kids and survival by parents/kids
train %>% ggvis(~Parch) %>% layer_bars()
train %>% 
  group_by(Parch) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~Parch, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: 1+ may be slightly better, but nothing dramatic

# distribution of fare and survival by fare
train %>% ggvis(~Fare) %>% layer_histograms()
train %>% 
  mutate(FareBucket = cut(Fare, c(0,25,50,75,100,150,200,Inf))) %>% 
  group_by(FareBucket) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~FareBucket, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: higher fares more likely to survive (overlap with class?)

# distribution of point of embarkment and survival
train %>% ggvis(~Embarked) %>% layer_bars()
train %>% 
  group_by(Embarked) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~Embarked, ~PercentSurvived) %>% 
  layer_bars()
# takeaway: C more likely to survive (overlap with class?)

# ---- "Everyone Dies" Model ----
# see results on train
train %<>%
  mutate(SurvivedPred_allPerish = rep(0, nrow(train)),
         PredCorrect_allPerish = ifelse(Survived == SurvivedPred_allPerish, 1, 0))
testStat_allPerish = sum(train$PredCorrect_allPerish)/nrow(train)
percent(testStat_allPerish)
# correct 61.6% of the time

# make same prediction on test for sample submission
test %<>% 
  mutate(Survived = rep(0, nrow(test)))

# create sample submission
submit = data.frame(
  PassengerId = test$PassengerId,
  Survived = test$Survived
)

# write output csv for submission
# write_csv(submit, "../KaggleTitanic/Submissions/TheyAllPerish/theyallperish.csv")

# ---- Gender Model ----
# predict women survive
# see results on train
train %<>%
  mutate(SurvivedPred_Gender = ifelse(Sex == "female", 1, 0),
         PredCorrect_Gender = ifelse(Survived == SurvivedPred_Gender, 1, 0))
testStat_Gender = sum(train$PredCorrect_Gender)/nrow(train)
percent(testStat_Gender)
# correct 78.7% of the time

# ---- Decision Tree Model ----
# start with a model of all predictors
predictors = c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
formula = as.formula(paste("Survived",paste(predictors, collapse=" + "),sep=" ~ "))
fit_all = rpart(formula, train, method="class")
fancyRpartPlot(fit_all)
train %<>%
  mutate(SurvivedPred_DT_All = predict(fit_all, train, type = "class"),
         PredCorrect_DT_All = ifelse(Survived == SurvivedPred_DT_All, 1, 0))
testStat_DT_All = sum(train$PredCorrect_DT_All)/nrow(train)
percent(testStat_DT_All)
# correct 84% of the time

# ---- Feature Engineering ----
# family size
train %<>%
  mutate(FamilySize = SibSp + Parch + 1)
train %>% ggvis(~FamilySize) %>% layer_histograms()
train %>% 
  group_by(FamilySize) %>% 
  summarize(PercentSurvived = mean(Survived)) %>% 
  ggvis(~FamilySize, ~PercentSurvived) %>% 
  layer_bars()
# nothing too dramatic here...

# titles
head(train$Name)
train$Name[514]
train$Name[514] = "Rothschild, Mrs. Martin (Elizabeth L Barrett)"
train %<>% separate(Name,c("Last","Title","FirstMiddle"),sep="[,.].",remove=FALSE)
train %<>%
  mutate(Title = as.factor(Title))
summary(train$Title)
