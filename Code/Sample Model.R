# load packages
require(plyr)
require(dplyr)
require(magrittr)
require(readr)

# read in train data set
train = read_csv("../KaggleTitanic/Data/train.csv")

# take an initial look at data set
glimpse(train)
summary(train)

