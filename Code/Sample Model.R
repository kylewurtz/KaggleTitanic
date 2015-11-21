# ---- load packages ----
require(plyr)
require(dplyr)
require(magrittr)
require(readr)
require(Amelia)
require(ggvis)

# ---- read in train data set ----
train = read_csv("../KaggleTitanic/Data/train.csv")

# take an initial look at data set
glimpse(train)
summary(train)

# map missing data
missmap(train)

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